;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#lifeCycleMessages

(import :std/sugar
        :std/logger
        ../handling
        ./types-incoming
        ./types-outgoing
        ./workspace-sync
        ./all-outgoing-messages
        ./workspace-sync)

(def +server-name+        "gxlsp")
(def +server-version+     "0.0.1")
(def +server-info+        (hash ("name" +server-name+) ("version" +server-version+)))


(defstruct LspClient  ( client-name client-version ;; obtained from InitializeParams
                        initializationOptions capabilities workspaceFolders ;; dito
                      ) final: #t)
;; gxlsp is a single-client server for now.
(def +lsp-client+ (make-LspClient #f #f #f #f #f))


(lsp-handler "shutdown"
  (lambda (params)
    (void)))


(lsp-handler "exit"
  (lambda (params)
    (exit)))


(lsp-handler "initialize"
  (lambda (params)
    (using (lsp-client +lsp-client+ :- LspClient)
      (let-hash params
        (when .$clientInfo
          (set! lsp-client.client-name    (hash-get .$clientInfo "name"))
          (set! lsp-client.client-version (hash-get .$clientInfo "version")))
        (set! lsp-client.initializationOptions .$initializationOptions)
        (set! lsp-client.capabilities          .$capabilities)
        (set! lsp-client.workspaceFolders      .$workspaceFolders)
        (hash ("serverInfo"   +server-info+)
              ("capabilities" (hash
                                  ("workspace" (hash
                                    ("workspaceFolders" (hash ("supported" #t) ("changeNotifications" #t)))))
                                  ("positionEncoding"
                                    "utf-16") ; utf-16 sadly mandatory for servers & clients (other encs optional, but no point then)
                                  ("textDocumentSync"
                                    (hash ("openClose" #t)
                                          ("change" 1)))
                                  ; keep notebookDocumentSync entirely OUT (not _just_ null), or VSCode's official "LSP client" node lib bugs out. wtf...
                                  ; ("notebookDocumentSync"
                                  ;   (void)) ; notebooks are mostly a client-side impl; eg. our VSC extension runs commands against this LSP for evals

                                  ("documentSymbolProvider"
                                    (hash ("label" "TODO_DocSym_Label")))
                                  ("workspaceSymbolProvider"
                                    #t)
                                  ("definitionProvider"
                                    #t)
                                  ("referencesProvider"
                                    #t)
                                  ("documentHighlightProvider"
                                    #t)
                                  ("completionProvider"
                                    #t)
                                  ("hoverProvider"
                                    #t)
                                  ("renameProvider"
                                    (hash ("prepareProvider" #t)))
                                  ("signatureHelpProvider"
                                    (hash ("triggerCharacters" [" " "\t"])))
                                  ("diagnosticProvider"
                                    #f) ; keep false since we do "push diags" and don't support "pull diags"
                                  ("codeActionProvider"
                                    #t)
                                  ("executeCommandProvider"
                                    (hash ("commands" ["eval-in-file"])))

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
                                    #f) ; when changing, it's an object not #t!
                                  ("documentFormattingProvider"
                                    #f)
                                  ("documentRangeFormattingProvider"
                                    #f)
                                  ("linkedEditingRangeProvider"
                                    #f)
)))))))


(lsp-handler "initialized"
  (lambda (params)
    (lsp-request-workspace-workspaceFolders!
      (lambda (all-workspace-folders)
        (on-workspace-folders-changed (map make-WorkspaceFolder all-workspace-folders) [])))
    ; we're not "making our own file-watcher" here below,
    ; but instead are telling the client to file-watch for us.
    ; sadly it doesn't suffice to just watch **/*.ss, because vscode sends no
    ; individual file events from certain folder events like moved or deleted
    (let (watcher (make-FileSystemWatcher kind: watchkind-all globPattern: "**/*"))
      (lsp-request-client-registerCapability! "workspace/didChangeWatchedFiles"
                                              (make-DidChangeWatchedFilesRegistrationOptions
                                                watchers: [watcher])))))
