(import :std/sugar
        :std/logger
        ./handling
        ./interfaces
        ./types-incoming
        ./types-outgoing
        ./msgs-outgoing)


(def lsp-client #f) ; on init, is set to hashtable of https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#clientCapabilities




;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#lifeCycleMessages


(lsp-handler "shutdown"
  (lambda (_)
    (when (is-LspHandlerOf-Shutdown? lsp-impl)
          (LspHandlerOf-Shutdown-shutdown lsp-impl))))


(lsp-handler "exit"
  (lambda (_)
    (exit)))


(lsp-handler "initialize"
  (lambda (params)
    (using (lsp-impl :- LanguageServer)
      (let-hash params
        (set! lsp-client .$clientInfo)
        (hash ("serverInfo"   (hash ("name" {lsp-impl.server-name}) ("version" {lsp-impl.server-version})))
              ("capabilities" (hash
                                  ("workspace"
                                    (if (is-LspHandlerOf-Workspace-DidChangeWorkspaceFolders? lsp-impl)
                                        (hash ("workspaceFolders" (hash ("supported" #t) ("changeNotifications" #t))))
                                        (void)))
                                  ("positionEncoding"
                                    "utf-16") ; utf-16 sadly mandatory for servers & clients (other encs optional, but no point then)
                                  ("textDocumentSync"
                                    (hash ("openClose" (is-LspHandlerOf-TextDocument-DidOpenClose? lsp-impl))
                                          ("change" (if (is-LspHandlerOf-TextDocument-DidChange? lsp-impl) 1 0))))
                                  ;; keep notebookDocumentSync entirely OUT (not _just_ null), or VSCode's official "LSP client" nodejs lib bugs out (wtf...)
                                  ; ("notebookDocumentSync"
                                  ;   (void)) ; notebooks are mostly a client-side impl; eg. our VSC extension runs commands against this LSP for evals
                                  ("documentSymbolProvider"
                                    (if (is-LspHandlerOf-TextDocument-DocumentSymbol? lsp-impl)
                                        (hash ("label" (LspHandlerOf-TextDocument-DocumentSymbol-multi-tree-label lsp-impl)))
                                        #f))
                                  ("workspaceSymbolProvider"
                                    (is-LspHandlerOf-Workspace-Symbol? lsp-impl))
                                  ("definitionProvider"
                                    (is-LspHandlerOf-TextDocument-Definition? lsp-impl))
                                  ("referencesProvider"
                                    (is-LspHandlerOf-TextDocument-References? lsp-impl))
                                  ("documentHighlightProvider"
                                    (is-LspHandlerOf-TextDocument-DocumentHighlight? lsp-impl))
                                  ("completionProvider"
                                    (if (is-LspHandlerOf-TextDocument-Completion? lsp-impl) (hash) (void)))
                                  ("hoverProvider"
                                    (is-LspHandlerOf-TextDocument-Hover? lsp-impl))
                                  ("renameProvider"
                                    (if (is-LspHandlerOf-TextDocument-Rename? lsp-impl)
                                        (hash ("prepareProvider" #t))
                                        #f))
                                  ("signatureHelpProvider"
                                    (if (is-LspHandlerOf-TextDocument-SignatureHelp? lsp-impl)
                                        (hash ("triggerCharacters" (LspHandlerOf-TextDocument-SignatureHelp-list-of-trigger-chars lsp-impl)))
                                        (void)))
                                  ("diagnosticProvider"
                                    #f) ; keep false since we do "push diags" and don't support "pull diags"
                                  ("codeActionProvider"
                                    (is-LspHandlerOf-TextDocument-CodeAction? lsp-impl))
                                  ("executeCommandProvider"
                                    (if (is-LspHandlerOf-Workspace-ExecuteCommand? lsp-impl)
                                        (hash ("commands" (LspHandlerOf-Workspace-ExecuteCommand-list-of-commands lsp-impl)))
                                        (void)))

                                  ;; below: currently unimplemented. when implementing one, move it above!

                                  ("declarationProvider"
                                    #f)
                                  ("typeDefinitionProvider"
                                    #f)
                                  ("implementationProvider"
                                    #f)
                                  ("codeLensProvider"
                                    (void)) ; when changing, it's an object not #t!
                                  ("documentFormattingProvider"
                                    #f)
                                  ("documentRangeFormattingProvider"
                                    #f)
                                  ("callHierarchyProvider"
                                    #f)
                                  ("typeHierarchyProvider"
                                    #f)
                                  ("selectionRangeProvider"
                                    #f)
                                  ("linkedEditingRangeProvider"
                                    #f))))))))


(lsp-handler "initialized"
  (lambda (_)
    (when (is-LspHandlerOf-Initialized? lsp-impl)
          (LspHandlerOf-Initialized-initialized lsp-impl lsp-client))
    (when (is-LspHandlerOf-Workspace-DidChangeWatchedFiles? lsp-impl)
      ; we're not "making our own file-watcher" here below,
      ; but instead are telling the client to file-watch for us.
      ; sadly it doesn't suffice to just watch "**/*.ext", because vscode sends no
      ; individual file events from certain folder events like moved or deleted, so
      ; impls must determine folder-ness of event and deduce the file events, if needed.
      (let (watcher (make-FileSystemWatcher kind: watchkind-all globPattern: "**/*"))
        (lsp-request-client-registerCapability! "workspace/didChangeWatchedFiles"
                                                (make-DidChangeWatchedFilesRegistrationOptions
                                                  watchers: [watcher]))))))




;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#languageFeatures


(lsp-handler "textDocument/documentSymbol"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-TextDocument-DocumentSymbol)
      {lsp-impl.textDocument-documentSymbol (make-DocumentSymbolParams params)})))


(lsp-handler "workspace/symbol"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-Workspace-Symbol)
      {lsp-impl.workspace-symbol (make-WorkspaceSymbolParams params)})))


(lsp-handler "textDocument/definition"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-TextDocument-Definition)
      {lsp-impl.textDocument-definition (make-DefinitionParams params)})))


(lsp-handler "textDocument/references"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-TextDocument-References)
      {lsp-impl.textDocument-references (make-ReferenceParams params)})))


(lsp-handler "textDocument/documentHighlight"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-TextDocument-DocumentHighlight)
      {lsp-impl.textDocument-documentHighlight (make-DocumentHighlightParams params)})))


(lsp-handler "textDocument/completion"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-TextDocument-Completion)
      {lsp-impl.textDocument-completion (make-CompletionParams params)})))


(lsp-handler "textDocument/hover"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-TextDocument-Hover)
      {lsp-impl.textDocument-hover (make-HoverParams params)})))


(lsp-handler "textDocument/prepareRename"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-TextDocument-Rename)
      {lsp-impl.textDocument-prepareRename (make-PrepareRenameParams params)})))


(lsp-handler "textDocument/rename"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-TextDocument-Rename)
      {lsp-impl.textDocument-rename (make-RenameParams params)})))


(lsp-handler "textDocument/signatureHelp"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-TextDocument-SignatureHelp)
      {lsp-impl.textDocument-signatureHelp (make-SignatureHelpParams params)})))


(lsp-handler "textDocument/codeAction"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-TextDocument-CodeAction)
      {lsp-impl.textDocument-codeAction (make-CodeActionParams params)})))


(lsp-handler "workspace/executeCommand"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-Workspace-ExecuteCommand)
      {lsp-impl.workspace-executeCommand (make-ExecuteCommandParams params)})))




;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceFeatures
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_synchronization


(lsp-handler "workspace/didChangeWorkspaceFolders"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-Workspace-DidChangeWorkspaceFolders)
      {lsp-impl.workspace-didChangeWorkspaceFolders (make-DidChangeWorkspaceFoldersParams params)})))


(lsp-handler "workspace/didChangeWatchedFiles"
  (lambda (params)
    (when (is-LspHandlerOf-Workspace-DidChangeWatchedFiles? lsp-impl) ; check needed in _this_ case
      (using (lsp-impl :- LspHandlerOf-Workspace-DidChangeWatchedFiles)
        {lsp-impl.workspace-didChangeWatchedFiles (make-DidChangeWatchedFilesParams params)}))))


(lsp-handler "textDocument/didOpen"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-TextDocument-DidOpenClose)
      {lsp-impl.textDocument-didOpen (make-DidOpenTextDocumentParams params)})))


(lsp-handler "textDocument/didClose"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-TextDocument-DidOpenClose)
      {lsp-impl.textDocument-didClose (make-DidCloseTextDocumentParams params)})))


(lsp-handler "textDocument/didChange"
  (lambda (params)
    (using (lsp-impl :- LspHandlerOf-Workspace-DidChangeWatchedFiles)
      {lsp-impl.workspace-didChangeWatchedFiles (make-DidChangeWatchedFilesParams params)})))
