import * as path from "path";
import * as vscode from "vscode";

export function defaultDebugOutputDirectory(): string {
	return vscode.workspace.getConfiguration("tablo").get<string>("debug.outputDirectory") || ".tablo/debug";
}

export function defaultTablocPath(): string {
	return vscode.workspace.getConfiguration("tablo").get<string>("debug.tablocPath") || "tabloc";
}

export function defaultTablodbgPath(): string {
	return vscode.workspace.getConfiguration("tablo").get<string>("debug.tablodbgPath") || "tablodbg";
}

export function isTabloSourceFile(filePath: string): boolean {
	return filePath.toLowerCase().endsWith(".tablo");
}

export function isTabloObjectFile(filePath: string): boolean {
	return filePath.toLowerCase().endsWith(".tbo");
}

export function resolveDebugOutputPath(sourceFile: string, workspaceFolder: vscode.WorkspaceFolder | undefined): string {
	const workspaceRoot = workspaceFolder?.uri.fsPath;
	const relativeSourcePath = workspaceRoot
		? path.relative(workspaceRoot, sourceFile)
		: path.basename(sourceFile);
	const outputRoot = workspaceRoot
		? path.join(workspaceRoot, defaultDebugOutputDirectory())
		: path.join(path.dirname(sourceFile), ".tablo", "debug");

	return path.join(
		outputRoot,
		relativeSourcePath.replace(/\.tablo$/i, ".tbo")
	);
}
