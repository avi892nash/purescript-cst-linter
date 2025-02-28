import path from 'path';
import vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';

/**
 * @param {vscode.ExtensionContext} context - The extension context
 */
export function activate (context) {
    const serverModule = context.asAbsolutePath(
        path.join('./dist/server.js')
      );
    
  const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

  /** @type {ServerOptions} */
  const serverOptions = {
    run: { module: serverModule, transport: TransportKind.ipc },
    debug: {
      module: serverModule,
      transport: TransportKind.ipc,
      options: debugOptions
    }
  };

   /** @type {LanguageClientOptions} */
   const clientOptions = {
    documentSelector: [{ scheme: 'file', language: 'purescript' }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher('**/.pslintrc')
    }
  };

  const client = new LanguageClient(
    'pslint',
    'PSLint Server',
    serverOptions,
    clientOptions
  );
  client.start();
}

/**
 * @returns {Thenable<void> | undefined}
 */
export function deactivate() {
    if (!client) {
        return undefined;
      }
      return client.stop();
}