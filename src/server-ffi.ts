import {
  _Connection,
	createConnection,
	TextDocuments,
	Diagnostic,
	DiagnosticSeverity,
	ProposedFeatures,
	InitializeParams,
	DidChangeConfigurationNotification,
	CompletionItem,
	CompletionItemKind,
	TextDocumentPositionParams,
	TextDocumentSyncKind,
	InitializeResult
} from "vscode-languageserver/node";

import {
	TextDocument,
} from "vscode-languageserver-textdocument";

import * as path from "path";
import * as url from "url";
import * as fs from "fs";

const defaultSettings: ExampleSettings = { maxNumberOfProblems: 1000 };

interface State {
  connection: _Connection,
  documents: TextDocuments<TextDocument>,
  hasConfigurationCapability: Boolean,
  documentSettings: Map<string, Thenable<ExampleSettings>>,
  globalSettings: ExampleSettings
};

interface ExampleSettings {
  maxNumberOfProblems: number;
}

export function load(): State {
  const connection = createConnection(ProposedFeatures.all);
  const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

  let hasConfigurationCapability = false;
  let hasWorkspaceFolderCapability = false;
  let hasDiagnosticRelatedInformationCapability = false;

  connection.onInitialize((params: InitializeParams) => {
    const capabilities = params.capabilities;

    hasConfigurationCapability = !!(
      capabilities.workspace && !!capabilities.workspace.configuration
    );
    hasWorkspaceFolderCapability = !!(
      capabilities.workspace && !!capabilities.workspace.workspaceFolders
    );
    hasDiagnosticRelatedInformationCapability = !!(
      capabilities.textDocument &&
      capabilities.textDocument.publishDiagnostics &&
      capabilities.textDocument.publishDiagnostics.relatedInformation
    );

    const result: InitializeResult = {
      capabilities: {
        textDocumentSync: TextDocumentSyncKind.Incremental,
        // Tell the client that this server supports code completion.
        completionProvider: {
          resolveProvider: true
        }
      }
    };
    if (hasWorkspaceFolderCapability) {
      result.capabilities.workspace = {
        workspaceFolders: {
          supported: true
        }
      };
    }
    return result;
  });

  connection.onInitialized(() => {
    if (hasConfigurationCapability) {
      // Register for all configuration changes.
      connection.client.register(DidChangeConfigurationNotification.type, undefined);
    }
    if (hasWorkspaceFolderCapability) {
      connection.workspace.onDidChangeWorkspaceFolders(_event => {
        connection.console.log("Workspace folder change event received.");
      });
    }
  });

  // The global settings, used when the `workspace/configuration` request is not supported by the client.
  // Please note that this is not the case when using this server with the client provided in this example
  // but could happen with other clients.
  let globalSettings: ExampleSettings = defaultSettings;

  // Cache the settings of all open documents
  const documentSettings: Map<string, Thenable<ExampleSettings>> = new Map();

  function getDocumentSettings(resource: string): Thenable<ExampleSettings> {
    if (!hasConfigurationCapability) {
      return Promise.resolve(globalSettings);
    }
    let result = documentSettings.get(resource);
    if (!result) {
      result = connection.workspace.getConfiguration({
        scopeUri: resource,
        section: "jsgf"
      });
      documentSettings.set(resource, result);
    }
    return result;
  }

  // Only keep settings for open documents
  documents.onDidClose(e => {
    documentSettings.delete(e.document.uri);
  });

  connection.onDidChangeWatchedFiles(_change => {
    // Monitored files have change in VSCode
    connection.console.log("We received a file change event");
  });

  // This handler provides the initial list of the completion items.
  connection.onCompletion(
    (_textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
      // The pass parameter contains the position of the text document in
      // which code complete got requested. For the example we ignore this
      // info and always provide the same completion items.
      return [
        {
          label: 'TypeScript',
          kind: CompletionItemKind.Text,
          data: 1
        },
        {
          label: 'JavaScript',
          kind: CompletionItemKind.Text,
          data: 2
        }
      ];
    }
  );

  // This handler resolves additional information for the item selected in
  // the completion list.
  connection.onCompletionResolve(
    (item: CompletionItem): CompletionItem => {
      if (item.data === 1) {
        item.detail = 'TypeScript details';
        item.documentation = 'TypeScript documentation';
      } else if (item.data === 2) {
        item.detail = 'JavaScript details';
        item.documentation = 'JavaScript documentation';
      }
      return item;
    }
  );

  // connection.onSignatureHelp(textDocumentPosition => {
  //   const document = documents.get(textDocumentPosition.textDocument.uri);
  //   const position = textDocumentPosition.position;
  //   // Implement signature help logic here and return relevant information
  // });

  // connection.onHover(textDocumentPosition => {
  //   const document = documents.get(textDocumentPosition.textDocument.uri);
  //   const position = textDocumentPosition.position;
  //   // Implement hover information logic here and return relevant details
  // });

  // documents.onDidChangeContent(change => {
  //   const document = change.document;
  //   const diagnostics = validateDocument(document);
  //   connection.sendDiagnostics({ uri: document.uri, diagnostics });
  // });
  // connection.onDocumentFormatting(formattingParams => {
  //   const document = documents.get(formattingParams.textDocument.uri);
  //   // Implement code formatting logic here and return formatted content
  // });

  // connection.onFoldingRanges(foldingRangeParams => {
  //   const document = documents.get(foldingRangeParams.textDocument.uri);
  //   // Implement code folding logic here and return folding ranges
  // });

  // connection.onReferences(referenceParams => {
  //   const document = documents.get(referenceParams.textDocument.uri);
  //   const position = referenceParams.position;
  //   // Implement find references logic here and return references
  // });

  // connection.onWorkspaceSymbol(workspaceSymbolParams => {
  //   const query = workspaceSymbolParams.query;
  //   // Implement symbol search logic here and return symbol matches
  // });

  // Make the text document manager listen on the connection
  // for open, change and close text document events
  documents.listen(connection);
  return { connection, documents, hasConfigurationCapability, documentSettings, globalSettings };
  
}

export function start(state: State) {
  // Listen on the connection
  state.connection.listen();
}

export function onChangeConfig(state: State, f: (state: State) => (textDocument: TextDocument) => () => Promise<void>) {
  state.connection.onDidChangeConfiguration(change => {
    if (state.hasConfigurationCapability) {
      // Reset all cached document settings
      state.documentSettings.clear();
    } else {
      state.globalSettings = <ExampleSettings>(
        (change.settings.languageServerExample || defaultSettings)
      );
    }

    // Revalidate all open text documents
    state.documents.all().forEach((doc) => f(state)(doc)());
  });
}

export function onChange(state: State, f: (state: State) => (textDocument: TextDocument) => () => Promise<void>) {
  state.documents.onDidChangeContent(change => {
    f(state)(change.document)();
  });
}

export function getText(textDocument: TextDocument): string {
  return textDocument.getText();
}

export function getUri(textDocument: TextDocument): string {
  return textDocument.uri;
}

export function getFullUri(abs: string, rel: string): string {
  const dir: string = path.dirname(abs);
  return path.join(dir, rel);
}

// export async function getTextFromUri(state: State, path: string): Promise<string> {
//   var res: string = "";
//   await state.connection.sendRequest("getFile", path).then((body: string) => {
//     if (body !== undefined && body.length) res = body;
//     else res = "";
//   });
//   return res;
// }

export function getTextFromUri(state: State, path: string): string {
  // TODO: check if file is open in Editor. If so, read contents from there. If not, open from filesystem
  // state.connection.console.info(`Reading contents of file '${path}'`);
  showInformationMessage(state, new URL(path).toString());
  const filedata = fs.readFileSync(new URL(path), "utf8");
  showInformationMessage(state, `${filedata}`)
  return filedata;
}

export function mkDiagnostic(isError: boolean, message: string, source: string, startLine: number, startCol: number, endLine: number, endCol: number) {
  const diagnostic: Diagnostic = {
  	severity: isError ? DiagnosticSeverity.Error : DiagnosticSeverity.Warning,
    range: {
      start: { line: startLine, character: startCol},
      end: { line: endLine, character: endCol}
    },
  	message: message,
  	source: source
  };
  return diagnostic;
}

export function mkDiagnostics(): Diagnostic[] {
  const diagnostics: Diagnostic[] = [];
  return diagnostics;
}

export function pushDiagnostic(ds: Diagnostic[], d: Diagnostic)  {
  ds.push(d);
}

export function sendDiagnostics(state: State, textDocument: TextDocument, diagnostics: Diagnostic[]) {
  state.connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

export function showInformationMessage(state: State, message: string) {
  state.connection.window.showInformationMessage(message);
}
