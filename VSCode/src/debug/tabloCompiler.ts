import * as fs from "fs/promises";
import * as path from "path";
import { execFile } from "child_process";
import * as vscode from "vscode";
import { defaultTablocPath, resolveDebugOutputPath } from "./paths";

export async function compileSourceForDebugging(
	sourceFile: string,
	workspaceFolder: vscode.WorkspaceFolder | undefined,
	explicitTablocPath?: string
): Promise<string> {
	const outputPath = resolveDebugOutputPath(sourceFile, workspaceFolder);
	await fs.mkdir(path.dirname(outputPath), { recursive: true });

	const tablocPath = explicitTablocPath || defaultTablocPath();

	await execFileAsync(tablocPath, [sourceFile, outputPath], {
		cwd: workspaceFolder?.uri.fsPath || path.dirname(sourceFile),
	});

	return outputPath;
}

function execFileAsync(
	command: string,
	args: string[],
	options: { cwd?: string }
): Promise<void> {
	return new Promise((resolve, reject) => {
		execFile(command, args, options, (error, _stdout, stderr) => {
			if (!error) {
				resolve();
				return;
			}

			const message = stderr?.trim() || error.message;
			reject(new Error(message));
		});
	});
}
