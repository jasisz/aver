import {
  commands,
  ExtensionContext,
  Position,
  Range,
  Selection,
  TextEditorRevealType,
  Uri,
  window,
  workspace,
} from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const config = workspace.getConfiguration("aver.lsp");
  const serverCommand: string = config.get("path", "aver-lsp");

  const serverOptions: ServerOptions = {
    command: serverCommand,
    args: [],
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "aver" }],
  };

  client = new LanguageClient(
    "aver-lsp",
    "Aver Language Server",
    serverOptions,
    clientOptions
  );

  context.subscriptions.push(
    commands.registerCommand(
      "aver.openLocation",
      async (uriString: string, line: number) => {
        if (!uriString) {
          return;
        }

        const targetUri = Uri.parse(uriString);
        const document = await workspace.openTextDocument(targetUri);
        const editor = await window.showTextDocument(document);
        const position = new Position(Math.max(0, line ?? 0), 0);
        editor.selection = new Selection(position, position);
        editor.revealRange(
          new Range(position, position),
          TextEditorRevealType.InCenter
        );
      }
    )
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
