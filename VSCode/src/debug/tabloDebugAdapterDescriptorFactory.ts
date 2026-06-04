import * as fs from "fs/promises";
import * as path from "path";
import { execFile } from "child_process";
import * as vscode from "vscode";
import { logError, logInfo, revealLogs } from "../log";
import { defaultTablodbgPath } from "./paths";

export class TabloDebugAdapterDescriptorFactory implements vscode.DebugAdapterDescriptorFactory, vscode.Disposable {
	async createDebugAdapterDescriptor(
		session: vscode.DebugSession,
		_executable: vscode.DebugAdapterExecutable | undefined
	): Promise<vscode.DebugAdapterDescriptor | undefined> {
		const configuredPath = session.configuration.tablodbgPath as string | undefined;
		const command = configuredPath || defaultTablodbgPath();
		const args = ["--dap"];
		const options: vscode.DebugAdapterExecutableOptions = {};

		if (session.configuration.cwd) {
			options.cwd = session.configuration.cwd;
		}

		logInfo(`Starting tablodbg from ${command} with cwd ${options.cwd || "<default>"}.`);

		if (!(await executableExists(command))) {
			logError(`Could not find tablodbg executable: ${command}`);
			revealLogs();
			void vscode.window.showErrorMessage(
				`The Tablo debugger executable \`${command}\` could not be found. ` +
				`Make sure \`tablodbg\` is on your PATH or set \`tablo.debug.tablodbgPath\` in VS Code settings.`
			);
			return undefined;
		}

		return new vscode.DebugAdapterExecutable(command, args, options);
	}

	dispose(): void {}
}

async function executableExists(command: string): Promise<boolean> {
	if (command.includes(path.sep) || path.isAbsolute(command)) {
		try {
			await fs.access(command);
			return true;
		}
		catch {
			return false;
		}
	}

	const resolver = process.platform === "win32" ? "where" : "which";

	return new Promise((resolve) => {
		execFile(resolver, [command], (error) => {
			resolve(!error);
		});
	});
}
