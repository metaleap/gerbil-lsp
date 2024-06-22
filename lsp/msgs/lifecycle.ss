;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#lifeCycleMessages

(import :std/sugar
        :std/logger
        ../handling
        ./types
        ./workspace-sync
        ./types-outgoing
        ./all-outgoing-requests)

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
                                    ; ("fileOperations" (hash
                                    ;   ("didRename" (hash
                                    ;     ("filters" [(hash
                                    ;       ("pattern" (hash ("glob" "*") ("matches" "folder"))))])))))
                                  ("textDocumentSync"
                                    (hash ("openClose" #t)
                                          ("change" 1)))
                                  ; ("notebookDocumentSync"
                                  ;   (hash ("notebook" "*.gerbilrepl")))
                                  ("positionEncoding"
                                    "utf-16") ; utf-16 sadly mandatory for servers & clients (other encs optional, but no point then)

                                  ("documentSymbolProvider"
                                    (hash ("label" "FooBarBaz")))
                                  ("workspaceSymbolProvider"
                                    #t)

                                  ("completionProvider"
                                    #f) ; when changing, it's an object not #t!
                                  ("signatureHelpProvider"
                                    #f) ; when changing, it's an object not #t!
                                  ("hoverProvider"
                                    #t)
                                  ("definitionProvider"
                                    #f)
                                  ("declarationProvider"
                                    #f)
                                  ("typeDefinitionProvider"
                                    #f)
                                  ("implementationProvider"
                                    #f)
                                  ("referencesProvider"
                                    #f)
                                  ("documentHighlightProvider"
                                    #f)
                                  ("codeActionProvider"
                                    #f) ; when changing, it's an object not #t!
                                  ("codeLensProvider"
                                    #f) ; when changing, it's an object not #t!
                                  ("documentFormattingProvider"
                                    #f)
                                  ("documentRangeFormattingProvider"
                                    #f)
                                  ("renameProvider"
                                    #f)
                                  ("executeCommandProvider"
                                    (hash ("commands" [])))
                                  ("selectionRangeProvider"
                                    #f)
                                  ("linkedEditingRangeProvider"
                                    #f)
                                  ("callHierarchyProvider"
                                    #f)
                                  ("typeHierarchyProvider"
                                    #f)
                                  ("diagnosticProvider"
                                    #f)
                                  ; ("diagnosticProvider"
                                  ;   (hash ("identifier" "Gerbil") ("interFileDependencies" #t) ("workspaceDiagnostics" #f)))
)))))))


(lsp-handler "initialized"
  (lambda (params)
    (lsp-req-workspace-workspacefolders!)
    ; we're not "making our own file-watcher" here below,
    ; but instead are telling the client to file-watch for us.
    ; sadly it doesn't suffice to just watch **/*.ss, because vscode sends no
    ; individual file events from certain folder events like moved or deleted
    (let (watcher (make-FileSystemWatcher kind: watchkind-all globPattern: "**/*"))
      (lsp-req-client-registercapability! "workspace/didChangeWatchedFiles"
                                          (make-DidChangeWatchedFilesRegistrationOptions
                                            watchers: [watcher])))))
