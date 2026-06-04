import * as vscode from "vscode";
import { TabloDebugAdapterDescriptorFactory } from "./tabloDebugAdapterDescriptorFactory";
import { TabloDebugConfigurationProvider } from "./tabloDebugConfigurationProvider";

export function registerTabloDebugging(context: vscode.ExtensionContext): void {
	const configurationProvider = new TabloDebugConfigurationProvider();
	const descriptorFactory = new TabloDebugAdapterDescriptorFactory();

	context.subscriptions.push(
		vscode.debug.registerDebugConfigurationProvider("tablo", configurationProvider),
		vscode.debug.registerDebugAdapterDescriptorFactory("tablo", descriptorFactory),
		descriptorFactory
	);
}
