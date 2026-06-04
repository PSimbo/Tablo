import * as vscode from "vscode";
import { compileSourceForDebugging } from "./tabloCompiler";
import { isTabloObjectFile, isTabloSourceFile } from "./paths";

export class TabloDebugConfigurationProvider implements vscode.DebugConfigurationProvider {
	resolveDebugConfiguration(
		_folder: vscode.WorkspaceFolder | undefined,
		config: vscode.DebugConfiguration
	): vscode.DebugConfiguration | undefined {
		const resolved = { ...config };

		if (!resolved.type) {
			resolved.type = "tablo";
		}

		if (!resolved.request) {
			resolved.request = "launch";
		}

		if (!resolved.name) {
			resolved.name = "Launch Tablo Program";
		}

		if (!resolved.sourceFile && !resolved.program && vscode.window.activeTextEditor) {
			resolved.sourceFile = vscode.window.activeTextEditor.document.uri.fsPath;
		}

		if (!resolved.cwd) {
			resolved.cwd = "${workspaceFolder}";
		}

		return resolved;
	}

	async resolveDebugConfigurationWithSubstitutedVariables(
		_folder: vscode.WorkspaceFolder | undefined,
		config: vscode.DebugConfiguration
	): Promise<vscode.DebugConfiguration | undefined> {
		const resolved = { ...config };

		if (resolved.sourceFile && isTabloSourceFile(resolved.sourceFile)) {
			try {
				resolved.program = await compileSourceForDebugging(
					resolved.sourceFile,
					_folder,
					resolved.tablocPath as string | undefined
				);
			}
			catch (error) {
				const message = error instanceof Error ? error.message : String(error);
				void vscode.window.showErrorMessage(`Failed to compile Tablo source for debugging: ${message}`);
				return undefined;
			}
		}

		if (!resolved.program && typeof resolved.sourceFile === "string" && isTabloObjectFile(resolved.sourceFile)) {
			resolved.program = resolved.sourceFile;
		}

		if (!resolved.program || typeof resolved.program !== "string") {
			void vscode.window.showErrorMessage("Tablo debugging requires either a `.tablo` source file or a `.tbo` object file.");
			return undefined;
		}

		return resolved;
	}
}
