import { spawn, type ChildProcessWithoutNullStreams } from "child_process";
import * as fs from "fs/promises";
import * as path from "path";
import * as vscode from "vscode";
import { logError, logInfo, revealLogs } from "../log";

type PendingRequest = {
	method: string;
	resolve: (value: unknown) => void;
	reject: (reason?: unknown) => void;
};

type LspDiagnostic = {
	message: string;
	range: {
		start: { line: number; character: number };
		end: { line: number; character: number };
	};
	severity?: number;
	source?: string;
};

type LspCompletionItem = {
	detail?: string;
	kind?: number;
	label: string;
};

type LspMessage = {
	id?: number;
	method?: string;
	params?: unknown;
	result?: unknown;
	error?: { code?: number; message?: string };
};

export class TabloLspClient implements vscode.Disposable {
	private readonly diagnostics = vscode.languages.createDiagnosticCollection("tablo");
	private initialized = false;
	private nextRequestId = 1;
	private outputBuffer = Buffer.alloc(0);
	private pendingRequests = new Map<number, PendingRequest>();
	private process: ChildProcessWithoutNullStreams | undefined;
	private started = false;
	private readonly subscriptions: vscode.Disposable[];

	constructor() {
		this.subscriptions = [
			this.diagnostics,
			vscode.workspace.onDidOpenTextDocument((document) => {
				void this.didOpen(document);
			}),
			vscode.workspace.onDidChangeTextDocument((event) => {
				void this.didChange(event.document);
			}),
			vscode.workspace.onDidCloseTextDocument((document) => {
				void this.didClose(document);
			}),
		];
	}

	dispose(): void {
		this.stop();
		vscode.Disposable.from(...this.subscriptions).dispose();
	}

	start(): void {
		if (this.started) {
			return;
		}

		this.started = true;
		void this.ensureStarted();
	}

	stop(): void {
		this.started = false;
		this.initialized = false;
		this.outputBuffer = Buffer.alloc(0);
		this.pendingRequests.clear();
		this.diagnostics.clear();

		if (this.process) {
			this.process.removeAllListeners();
			this.process.kill();
			this.process = undefined;
		}
	}

	async provideCompletionItems(
		document: vscode.TextDocument,
		position: vscode.Position,
	): Promise<vscode.CompletionItem[] | undefined> {
		if (!isTabloDocument(document)) {
			return undefined;
		}

		if (!await this.ensureStarted()) {
			return undefined;
		}

		const result = await this.sendRequest("textDocument/completion", {
			textDocument: {
				uri: document.uri.toString(),
			},
			position: {
				line: position.line,
				character: position.character,
			},
		});

		if (!Array.isArray(result)) {
			return undefined;
		}

		return result
			.filter(isCompletionItem)
			.map(toVsCodeCompletionItem);
	}

	private async didChange(document: vscode.TextDocument): Promise<void> {
		if (!isTabloDocument(document)) {
			return;
		}

		if (!await this.ensureStarted()) {
			return;
		}

		this.sendNotification("textDocument/didChange", {
			textDocument: {
				uri: document.uri.toString(),
				version: document.version,
			},
			contentChanges: [
				{ text: document.getText() },
			],
		});
	}

	private async didClose(document: vscode.TextDocument): Promise<void> {
		if (!isTabloDocument(document)) {
			return;
		}

		if (!this.process || !this.initialized) {
			this.diagnostics.delete(document.uri);
			return;
		}

		this.sendNotification("textDocument/didClose", {
			textDocument: {
				uri: document.uri.toString(),
			},
		});
	}

	private async didOpen(document: vscode.TextDocument): Promise<void> {
		if (!isTabloDocument(document)) {
			return;
		}

		if (!await this.ensureStarted()) {
			return;
		}

		this.sendNotification("textDocument/didOpen", {
			textDocument: {
				uri: document.uri.toString(),
				languageId: document.languageId,
				version: document.version,
				text: document.getText(),
			},
		});
	}

	private handleDiagnostics(uri: string, diagnostics: LspDiagnostic[]): void {
		const mapped = diagnostics.map((diagnostic) => {
			const severity = toVsCodeSeverity(diagnostic.severity);
			const range = new vscode.Range(
				new vscode.Position(diagnostic.range.start.line, diagnostic.range.start.character),
				new vscode.Position(diagnostic.range.end.line, diagnostic.range.end.character),
			);
			const item = new vscode.Diagnostic(range, diagnostic.message, severity);
			item.source = diagnostic.source || "tablolsp";
			return item;
		});

		this.diagnostics.set(vscode.Uri.parse(uri), mapped);
	}

	private handleMessage(message: LspMessage): void {
		if (typeof message.id === "number") {
			const pending = this.pendingRequests.get(message.id);
			if (!pending) {
				return;
			}

			this.pendingRequests.delete(message.id);

			if (message.error) {
				pending.reject(new Error(message.error.message || `LSP request ${pending.method} failed.`));
				return;
			}

			pending.resolve(message.result);
			return;
		}

		if (message.method === "textDocument/publishDiagnostics" && isDiagnosticsParams(message.params)) {
			this.handleDiagnostics(message.params.uri, message.params.diagnostics);
		}
	}

	private async ensureStarted(): Promise<boolean> {
		if (!this.started) {
			return false;
		}

		if (this.process && this.initialized) {
			return true;
		}

		if (this.process && !this.initialized) {
			return false;
		}

		const command = configuredTablolspPath();
		if (!(await executableExists(command))) {
			logError(`Could not find tablolsp executable: ${command}`);
			revealLogs();
			void vscode.window.showErrorMessage(
				`The Tablo language server executable \`${command}\` could not be found. ` +
				`Make sure \`tablolsp\` is on your PATH or set \`tablo.language.tablolspPath\` in VS Code settings.`
			);
			this.started = false;
			return false;
		}

		logInfo(`Starting tablolsp from ${command}.`);
		const child = spawn(command, [], {
			cwd: vscode.workspace.workspaceFolders?.[0]?.uri.fsPath,
			stdio: "pipe",
		});
		this.process = child;
		this.outputBuffer = Buffer.alloc(0);

		child.stdout.on("data", (chunk: Buffer) => {
			this.outputBuffer = Buffer.concat([this.outputBuffer, chunk]);
			this.processBufferedMessages();
		});
		child.stderr.on("data", (chunk: Buffer) => {
			logError(`tablolsp stderr: ${chunk.toString().trimEnd()}`);
		});
		child.on("error", (error) => {
			logError(`tablolsp process error: ${error.message}`);
		});
		child.on("exit", (code, signal) => {
			logInfo(`tablolsp exited with code ${code ?? "<unknown>"}${signal ? ` and signal ${signal}` : ""}.`);
			this.process = undefined;
			this.initialized = false;
		});

		try {
			await this.sendRequest("initialize", {
				processId: process.pid,
				clientInfo: {
					name: "tablo-vscode",
				},
				rootUri: vscode.workspace.workspaceFolders?.[0]?.uri.toString() || null,
				capabilities: {},
				workspaceFolders: (vscode.workspace.workspaceFolders || []).map((folder) => ({
					uri: folder.uri.toString(),
					name: folder.name,
				})),
			});
			this.initialized = true;
			this.sendNotification("initialized", {});

			for (const document of vscode.workspace.textDocuments) {
				await this.didOpen(document);
			}

			return true;
		}
		catch (error) {
			logError(`Failed to initialize tablolsp: ${asErrorMessage(error)}`);
			revealLogs();
			void vscode.window.showErrorMessage(`Failed to initialize tablolsp: ${asErrorMessage(error)}`);
			this.stop();
			return false;
		}
	}

	private processBufferedMessages(): void {
		while (true) {
			const separatorIndex = this.outputBuffer.indexOf("\r\n\r\n");
			if (separatorIndex < 0) {
				return;
			}

			const headerText = this.outputBuffer.slice(0, separatorIndex).toString("utf8");
			const contentLength = parseContentLength(headerText);
			if (contentLength === undefined) {
				logError(`tablolsp sent a message without Content-Length: ${headerText}`);
				this.outputBuffer = Buffer.alloc(0);
				return;
			}

			const messageStart = separatorIndex + 4;
			const messageEnd = messageStart + contentLength;
			if (this.outputBuffer.length < messageEnd) {
				return;
			}

			const body = this.outputBuffer.slice(messageStart, messageEnd).toString("utf8");
			this.outputBuffer = this.outputBuffer.slice(messageEnd);

			try {
				this.handleMessage(JSON.parse(body) as LspMessage);
			}
			catch (error) {
				logError(`Failed to parse LSP message: ${asErrorMessage(error)}`);
			}
		}
	}

	private sendNotification(method: string, params: unknown): void {
		if (!this.process) {
			return;
		}

		this.writeMessage({
			jsonrpc: "2.0",
			method,
			params,
		});
	}

	private sendRequest(method: string, params: unknown): Promise<unknown> {
		const id = this.nextRequestId++;

		return new Promise((resolve, reject) => {
			this.pendingRequests.set(id, {
				method,
				resolve,
				reject,
			});

			this.writeMessage({
				jsonrpc: "2.0",
				id,
				method,
				params,
			});
		});
	}

	private writeMessage(message: unknown): void {
		if (!this.process) {
			return;
		}

		const body = Buffer.from(JSON.stringify(message), "utf8");
		const header = Buffer.from(`Content-Length: ${body.length}\r\n\r\n`, "utf8");
		this.process.stdin.write(header);
		this.process.stdin.write(body);
	}
}

function asErrorMessage(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}

function configuredTablolspPath(): string {
	return vscode.workspace.getConfiguration("tablo").get<string>("language.tablolspPath") || "tablolsp";
}

async function executableExists(command: string): Promise<boolean> {
	if (command.includes(path.sep) || path.isAbsolute(command)) {
		try {
			await fs.access(command);
			return true;
		}
		catch {
			return false;
		}
	}

	const resolver = process.platform === "win32" ? "where" : "which";

	return new Promise((resolve) => {
		const checker = spawn(resolver, [command], { stdio: "ignore" });
		checker.on("exit", (code) => resolve(code === 0));
		checker.on("error", () => resolve(false));
	});
}

function isDiagnosticsParams(value: unknown): value is { uri: string; diagnostics: LspDiagnostic[] } {
	if (!value || typeof value !== "object") {
		return false;
	}

	const candidate = value as { diagnostics?: unknown; uri?: unknown };
	return typeof candidate.uri === "string" && Array.isArray(candidate.diagnostics);
}

function isCompletionItem(value: unknown): value is LspCompletionItem {
	if (!value || typeof value !== "object") {
		return false;
	}

	const candidate = value as { detail?: unknown; kind?: unknown; label?: unknown };
	return typeof candidate.label === "string"
		&& (candidate.detail === undefined || typeof candidate.detail === "string")
		&& (candidate.kind === undefined || typeof candidate.kind === "number");
}

function isTabloDocument(document: vscode.TextDocument): boolean {
	return document.languageId === "tablo" && document.uri.scheme === "file";
}

function parseContentLength(headerText: string): number | undefined {
	for (const line of headerText.split("\r\n")) {
		const [name, value] = line.split(":", 2);
		if (name?.toLowerCase() === "content-length") {
			const parsed = Number.parseInt(value.trim(), 10);
			return Number.isFinite(parsed) ? parsed : undefined;
		}
	}

	return undefined;
}

function toVsCodeSeverity(severity: number | undefined): vscode.DiagnosticSeverity {
	switch (severity) {
		case 1:
			return vscode.DiagnosticSeverity.Error;
		case 2:
			return vscode.DiagnosticSeverity.Warning;
		case 3:
			return vscode.DiagnosticSeverity.Information;
		case 4:
			return vscode.DiagnosticSeverity.Hint;
		default:
			return vscode.DiagnosticSeverity.Error;
	}
}

function toVsCodeCompletionItem(item: LspCompletionItem): vscode.CompletionItem {
	const completionItem = new vscode.CompletionItem(item.label, toVsCodeCompletionItemKind(item.kind));
	completionItem.detail = item.detail;
	return completionItem;
}

function toVsCodeCompletionItemKind(kind: number | undefined): vscode.CompletionItemKind {
	switch (kind) {
		case 3:
			return vscode.CompletionItemKind.Function;
		case 5:
			return vscode.CompletionItemKind.Field;
		case 14:
			return vscode.CompletionItemKind.Keyword;
		case 21:
			return vscode.CompletionItemKind.Constant;
		case 25:
			return vscode.CompletionItemKind.TypeParameter;
		default:
			return vscode.CompletionItemKind.Text;
	}
}
