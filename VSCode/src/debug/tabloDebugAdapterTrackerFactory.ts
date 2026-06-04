import * as vscode from "vscode";
import { logError, logInfo } from "../log";

export class TabloDebugAdapterTrackerFactory implements vscode.DebugAdapterTrackerFactory {
	createDebugAdapterTracker(session: vscode.DebugSession): vscode.ProviderResult<vscode.DebugAdapterTracker> {
		logInfo(`Started debug session "${session.name}".`);

		return {
			onDidSendMessage: (message: unknown) => {
				const candidate = message as {
					body?: { output?: string };
					command?: string;
					event?: string;
					message?: string;
					success?: boolean;
					type?: string;
				};

				if (candidate.type === "response" && candidate.success === false) {
					const errorMessage = candidate.message || `Debug request failed${candidate.command ? `: ${candidate.command}` : ""}.`;
					logError(errorMessage);
					void vscode.window.showErrorMessage(`Tablo debug error: ${errorMessage}`);
				}

				if (candidate.type === "event" && candidate.event === "output" && candidate.body?.output) {
					logInfo(candidate.body.output.trimEnd());
				}
			},
			onError: (error: Error) => {
				logError(`Debug adapter error: ${error.message}`);
				void vscode.window.showErrorMessage(`Tablo debug adapter error: ${error.message}`);
			},
			onExit: (code: number | undefined, signal: string | undefined) => {
				logInfo(`Debug adapter exited${code !== undefined ? ` with code ${code}` : ""}${signal ? ` (signal ${signal})` : ""}.`);
			},
		};
	}
}
