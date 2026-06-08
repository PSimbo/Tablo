import * as vscode from "vscode";
import { logError, logInfo } from "../log";

export class TabloDebugAdapterTrackerFactory implements vscode.DebugAdapterTrackerFactory {
	createDebugAdapterTracker(session: vscode.DebugSession): vscode.ProviderResult<vscode.DebugAdapterTracker> {
		logInfo(`Started debug session "${session.name}".`);
		let sawNormalTerminationEvent = false;

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

				if (candidate.type === "event" && (candidate.event === "terminated" || candidate.event === "exited")) {
					sawNormalTerminationEvent = true;
				}

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
				if (sawNormalTerminationEvent && error.message === "read error") {
					logInfo("Ignoring adapter read error reported after normal session termination.");
					return;
				}

				logError(`Debug adapter error: ${error.message}`);
				void vscode.window.showErrorMessage(`Tablo debug adapter error: ${error.message}`);
			},
			onExit: (code: number | undefined, signal: string | undefined) => {
				logInfo(`Debug adapter exited${code !== undefined ? ` with code ${code}` : ""}${signal ? ` (signal ${signal})` : ""}.`);
			},
		};
	}
}
