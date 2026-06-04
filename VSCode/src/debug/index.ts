import * as vscode from "vscode";
import { TabloDebugAdapterDescriptorFactory } from "./tabloDebugAdapterDescriptorFactory";
import { TabloDebugAdapterTrackerFactory } from "./tabloDebugAdapterTrackerFactory";
import { TabloDebugConfigurationProvider } from "./tabloDebugConfigurationProvider";

export function registerTabloDebugging(context: vscode.ExtensionContext): void {
	const configurationProvider = new TabloDebugConfigurationProvider();
	const descriptorFactory = new TabloDebugAdapterDescriptorFactory();
	const trackerFactory = new TabloDebugAdapterTrackerFactory();

	context.subscriptions.push(
		vscode.debug.registerDebugConfigurationProvider("tablo", configurationProvider),
		vscode.debug.registerDebugAdapterDescriptorFactory("tablo", descriptorFactory),
		vscode.debug.registerDebugAdapterTrackerFactory("tablo", trackerFactory),
		descriptorFactory
	);
}
