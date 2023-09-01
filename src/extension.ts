import * as path from "path";
import { workspace, ExtensionContext, Uri } from "vscode";

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const serverModule = context.asAbsolutePath(
		path.join("build", "exec", "server.js")
	);

	const serverOptions: ServerOptions = {
		run: { module: serverModule, transport: TransportKind.ipc },
		debug: {
			module: serverModule,
			transport: TransportKind.ipc,
		}
	};

	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: "file", language: "jsgf" }],
		synchronize: {
			fileEvents: workspace.createFileSystemWatcher("**/*.jsgf")
		}
	};

	client = new LanguageClient(
		"jsgf",
		"JSGF Language Server",
		serverOptions,
		clientOptions
	);

	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
