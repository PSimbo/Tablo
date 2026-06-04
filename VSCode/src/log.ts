import * as vscode from "vscode";

let outputChannel: vscode.OutputChannel | undefined;

export function tabloOutputChannel(): vscode.OutputChannel {
	if (!outputChannel) {
		outputChannel = vscode.window.createOutputChannel("Tablo");
	}

	return outputChannel;
}

export function logInfo(message: string): void {
	tabloOutputChannel().appendLine(`[info] ${message}`);
}

export function logError(message: string): void {
	tabloOutputChannel().appendLine(`[error] ${message}`);
}

export function revealLogs(preserveFocus = true): void {
	tabloOutputChannel().show(preserveFocus);
}
