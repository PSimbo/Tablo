import * as vscode from "vscode";
import { registerTabloDebugging } from "./debug";
import { registerTabloLanguageFeatures } from "./language";
import { tabloOutputChannel } from "./log";

export function activate(context: vscode.ExtensionContext): void {
	context.subscriptions.push(tabloOutputChannel());
	registerTabloDebugging(context);
	registerTabloLanguageFeatures(context);
}

export function deactivate(): void {}
