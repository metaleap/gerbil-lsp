;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#lifeCycleMessages

(import :std/sugar
        :std/logger
        ../handling
        ./types
        ./workspace
        ./outgoing-requests)

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
                                  ; ("textDocumentSync"
                                  ;   (hash ("openClose" #f)
                                  ;         ("change" 1)))
                                  ; ("notebookDocumentSync"
                                  ;   (hash ("notebook" "*.gerbilrepl")))
                                  ("positionEncoding"
                                    "utf-16") ; utf-16 sadly mandatory for servers & clients (other encs optional, but no point then)

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
                                  ("documentSymbolProvider"
                                    #f)
                                  ("workspaceSymbolProvider"
                                    #f) ; when changing, it's an object not #t!
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
    (def glob-exts (if (fx= 1 (length source-file-extensions)) ; special-casing due to vscode quirk
                        (list-ref source-file-extensions 0)
                        (string-append "{" (string-join source-file-extensions ",") "}")))
    (lsp-req-client-registercapability "workspace/didChangeWatchedFiles"
                                        (make-DidChangeWatchedFilesRegistrationOptions
                                          watchers: [(make-FileSystemWatcher
                                                        kind: watchkind-all
                                                        globPattern: (string-append "**/*" glob-exts))]))
    (lsp-req-workspace-workspacefolders)))
