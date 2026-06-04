import * as vscode from "vscode";
import { defaultTablodbgPath } from "./paths";

export class TabloDebugAdapterDescriptorFactory implements vscode.DebugAdapterDescriptorFactory, vscode.Disposable {
	createDebugAdapterDescriptor(
		session: vscode.DebugSession,
		_executable: vscode.DebugAdapterExecutable | undefined
	): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
		const configuredPath = session.configuration.tablodbgPath as string | undefined;
		const command = configuredPath || defaultTablodbgPath();
		const args = ["--dap"];
		const options: vscode.DebugAdapterExecutableOptions = {};

		if (session.configuration.cwd) {
			options.cwd = session.configuration.cwd;
		}

		return new vscode.DebugAdapterExecutable(command, args, options);
	}

	dispose(): void {}
}
