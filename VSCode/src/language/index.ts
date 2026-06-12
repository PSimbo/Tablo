import * as vscode from "vscode";
import { registerCompletionProvider } from "./completion";

export function registerTabloLanguageFeatures(context: vscode.ExtensionContext): void {
	registerCompletionProvider(context);
}
