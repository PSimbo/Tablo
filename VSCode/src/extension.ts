import * as vscode from "vscode";
import { registerTabloDebugging } from "./debug";

export function activate(context: vscode.ExtensionContext): void {
	registerTabloDebugging(context);
}

export function deactivate(): void {}
