import * as vscode from "vscode";
import { registerCompletionProvider } from "./completion";
import { TabloLspClient } from "./lsp";

export function registerTabloLanguageFeatures(context: vscode.ExtensionContext): void {
	const lspClient = new TabloLspClient();
	let languageDisposable: vscode.Disposable | undefined;

	const refreshLanguageFeatures = (): void => {
		languageDisposable?.dispose();

		if (isLspEnabled()) {
			lspClient.start();
			const selector = { language: "tablo", scheme: "file" };
			languageDisposable = vscode.Disposable.from(
				vscode.languages.registerCompletionItemProvider(selector, lspClient, ".", "_"),
				vscode.languages.registerSignatureHelpProvider(selector, lspClient, "(", ","),
			);
			return;
		}

		lspClient.stop();
		languageDisposable = registerCompletionProvider();
	};

	refreshLanguageFeatures();

	context.subscriptions.push(
		lspClient,
		{
			dispose: () => {
				languageDisposable?.dispose();
			},
		},
		vscode.workspace.onDidChangeConfiguration((event) => {
			if (event.affectsConfiguration("tablo.language.enableLsp")
				|| event.affectsConfiguration("tablo.language.tablolspPath")) {
				refreshLanguageFeatures();
			}
		}),
	);
}

function isLspEnabled(): boolean {
	return vscode.workspace.getConfiguration("tablo").get<boolean>("language.enableLsp", false);
}
