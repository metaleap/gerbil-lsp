(import :std/sugar
        :std/logger
        ../handling
        ../interfaces
        ./types-incoming
        ./types-outgoing
        ./outgoing)


(def lsp-client #f) ; on init, is set to hashtable of https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#clientCapabilities




;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#lifeCycleMessages


(lsp-handler "shutdown"
  (lambda (_)
    (when (is-Shutdown? lsp-impl)
          (Shutdown-shutdown lsp-impl))))


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
                                    (if (is-Workspace-DidChangeWorkspaceFolders? lsp-impl)
                                        (hash ("workspaceFolders" (hash ("supported" #t) ("changeNotifications" #t))))
                                        (void)))
                                  ("positionEncoding"
                                    "utf-16") ; utf-16 sadly mandatory for servers & clients (other encs optional, but no point then)
                                  ("textDocumentSync"
                                    (hash ("openClose" (is-TextDocument-DidOpenClose? lsp-impl))
                                          ("change" (if (is-TextDocument-DidChange? lsp-impl) 1 0))))
                                  ;; keep notebookDocumentSync entirely OUT (not _just_ null), or VSCode's official "LSP client" nodejs lib bugs out (wtf...)
                                  ; ("notebookDocumentSync"
                                  ;   (void)) ; notebooks are mostly a client-side impl; eg. our VSC extension runs commands against this LSP for evals
                                  ("documentSymbolProvider"
                                    (if (is-TextDocument-DocumentSymbol? lsp-impl)
                                        (hash ("label" (TextDocument-DocumentSymbol-multi-tree-label lsp-impl)))
                                        #f))
                                  ("workspaceSymbolProvider"
                                    (is-Workspace-Symbol? lsp-impl))
                                  ("definitionProvider"
                                    (is-TextDocument-Definition? lsp-impl))
                                  ("referencesProvider"
                                    (is-TextDocument-References? lsp-impl))
                                  ("documentHighlightProvider"
                                    (is-TextDocument-DocumentHighlight? lsp-impl))
                                  ("completionProvider"
                                    (if (is-TextDocument-Completion? lsp-impl) (hash) (void)))
                                  ("hoverProvider"
                                    (is-TextDocument-Hover? lsp-impl))
                                  ("renameProvider"
                                    (if (is-TextDocument-Rename? lsp-impl)
                                        (hash ("prepareProvider" #t))
                                        #f))
                                  ("signatureHelpProvider"
                                    (if (is-TextDocument-SignatureHelp? lsp-impl)
                                        (hash ("triggerCharacters" (TextDocument-SignatureHelp-list-of-trigger-chars lsp-impl)))
                                        (void)))
                                  ("diagnosticProvider"
                                    #f) ; keep false since we do "push diags" and don't support "pull diags"
                                  ("codeActionProvider"
                                    (is-TextDocument-CodeAction? lsp-impl))
                                  ("executeCommandProvider"
                                    (if (is-Workspace-ExecuteCommand? lsp-impl)
                                        (hash ("commands" (Workspace-ExecuteCommand-list-of-commands lsp-impl)))
                                        (void)))

                                  ("selectionRangeProvider"
                                    #f)
                                  ("callHierarchyProvider"
                                    #f)
                                  ("typeHierarchyProvider"
                                    #f)
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
                                  ("linkedEditingRangeProvider"
                                    #f))))))))


(lsp-handler "initialized"
  (lambda (_)
    (when (is-Initialized? lsp-impl)
          (Initialized-initialized lsp-impl lsp-client))
    (when (is-Workspace-DidChangeWatchedFiles? lsp-impl)
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
    (using (lsp-impl :- TextDocument-DocumentSymbol)
      {lsp-impl.textDocument-documentSymbol (make-DocumentSymbolParams params)})))


(lsp-handler "workspace/symbol"
  (lambda (params)
    (using (lsp-impl :- Workspace-Symbol)
      {lsp-impl.workspace-symbol (make-WorkspaceSymbolParams params)})))


(lsp-handler "textDocument/definition"
  (lambda (params)
    (using (lsp-impl :- TextDocument-Definition)
      {lsp-impl.textDocument-definition (make-DefinitionParams params)})))


(lsp-handler "textDocument/references"
  (lambda (params)
    (using (lsp-impl :- TextDocument-References)
      {lsp-impl.textDocument-references (make-ReferenceParams params)})))


(lsp-handler "textDocument/documentHighlight"
  (lambda (params)
    (using (lsp-impl :- TextDocument-DocumentHighlight)
      {lsp-impl.textDocument-documentHighlight (make-DocumentHighlightParams params)})))


(lsp-handler "textDocument/completion"
  (lambda (params)
    (using (lsp-impl :- TextDocument-Completion)
      {lsp-impl.textDocument-completion (make-CompletionParams params)})))


(lsp-handler "textDocument/hover"
  (lambda (params)
    (using (lsp-impl :- TextDocument-Hover)
      {lsp-impl.textDocument-hover (make-HoverParams params)})))


(lsp-handler "textDocument/prepareRename"
  (lambda (params)
    (using (lsp-impl :- TextDocument-Rename)
      {lsp-impl.textDocument-prepareRename (make-PrepareRenameParams params)})))


(lsp-handler "textDocument/rename"
  (lambda (params)
    (using (lsp-impl :- TextDocument-Rename)
      {lsp-impl.textDocument-rename (make-RenameParams params)})))


(lsp-handler "textDocument/signatureHelp"
  (lambda (params)
    (using (lsp-impl :- TextDocument-SignatureHelp)
      {lsp-impl.textDocument-signatureHelp (make-SignatureHelpParams params)})))


(lsp-handler "textDocument/codeAction"
  (lambda (params)
    (using (lsp-impl :- TextDocument-CodeAction)
      {lsp-impl.textDocument-codeAction (make-CodeActionParams params)})))


(lsp-handler "workspace/executeCommand"
  (lambda (params)
    (using (lsp-impl :- Workspace-ExecuteCommand)
      {lsp-impl.workspace-executeCommand (make-ExecuteCommandParams params)})))




;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceFeatures
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_synchronization


(lsp-handler "workspace/didChangeWorkspaceFolders"
  (lambda (params)
    (using (lsp-impl :- Workspace-DidChangeWorkspaceFolders)
      {lsp-impl.workspace-didChangeWorkspaceFolders (make-DidChangeWorkspaceFoldersParams params)})))


(lsp-handler "workspace/didChangeWatchedFiles"
  (lambda (params)
    (when (is-Workspace-DidChangeWatchedFiles? lsp-impl) ; check needed in _this_ case
      (using (lsp-impl :- Workspace-DidChangeWatchedFiles)
        {lsp-impl.workspace-didChangeWatchedFiles (make-DidChangeWatchedFilesParams params)}))))


(lsp-handler "textDocument/didOpen"
  (lambda (params)
    (using (lsp-impl :- TextDocument-DidOpenClose)
      {lsp-impl.textDocument-didOpen (make-DidOpenTextDocumentParams params)})))


(lsp-handler "textDocument/didClose"
  (lambda (params)
    (using (lsp-impl :- TextDocument-DidOpenClose)
      {lsp-impl.textDocument-didClose (make-DidCloseTextDocumentParams params)})))


(lsp-handler "textDocument/didChange"
  (lambda (params)
    (using (lsp-impl :- Workspace-DidChangeWatchedFiles)
      {lsp-impl.workspace-didChangeWatchedFiles (make-DidChangeWatchedFilesParams params)})))
