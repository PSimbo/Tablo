import * as vscode from "vscode";
import { registerTabloDebugging } from "./debug";
import { tabloOutputChannel } from "./log";

export function activate(context: vscode.ExtensionContext): void {
	context.subscriptions.push(tabloOutputChannel());
	registerTabloDebugging(context);
}

export function deactivate(): void {}
