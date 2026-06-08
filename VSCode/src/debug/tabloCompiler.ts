import * as fs from "fs/promises";
import * as path from "path";
import { execFile } from "child_process";
import * as vscode from "vscode";
import { logError, logInfo } from "../log";
import { defaultProjectConfigPath, defaultTablocPath, resolveDebugOutputPath } from "./paths";

export async function compileSourceForDebugging(
	sourceFile: string,
	workspaceFolder: vscode.WorkspaceFolder | undefined,
	explicitTablocPath?: string,
	explicitProjectConfigPath?: string
): Promise<string> {
	const outputPath = resolveDebugOutputPath(sourceFile, workspaceFolder);
	await fs.mkdir(path.dirname(outputPath), { recursive: true });

	const tablocPath = explicitTablocPath || defaultTablocPath();
	const projectConfigPath = explicitProjectConfigPath || defaultProjectConfigPath();
	const args = projectConfigPath
		? ["--config", projectConfigPath, sourceFile, outputPath]
		: [sourceFile, outputPath];
	logInfo(
		`Compiling ${sourceFile} to ${outputPath} using ${tablocPath}` +
		`${projectConfigPath ? ` with config ${projectConfigPath}` : ""}.`
	);

	await execFileAsync(tablocPath, args, {
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

			if (error.code === "ENOENT") {
				logError(`Could not find tabloc executable: ${command}`);
				reject(new Error(
					`The Tablo compiler executable \`${command}\` could not be found. ` +
					`Make sure \`tabloc\` is on your PATH or set \`tablo.debug.tablocPath\` in VS Code settings.`
				));
				return;
			}

			const message = stderr?.trim() || error.message;
			logError(`tabloc failed: ${message}`);
			reject(new Error(message));
		});
	});
}
