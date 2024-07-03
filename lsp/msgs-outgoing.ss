(export #t)

(import :std/sugar
        :std/logger
        :std/os/socket
        :std/text/json
        :std/net/json-rpc
        ./handling
        ./types-incoming
        ./types-outgoing)


; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_workspaceFolders
(def (lsp-request-workspace-workspaceFolders! on-resp)
  (lsp-request! "workspace/workspaceFolders" (void)
    on-resp))



; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#client_registerCapability
(def (lsp-request-client-registerCapability! method register-options)
  (lsp-request! "client/registerCapability" (make-RegistrationParams
                                              registrations: [(make-Registration
                                                                id: (gensym)
                                                                method: method
                                                                registerOptions: register-options)])
    #f))



; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_publishDiagnostics
(def (lsp-notify-textDocument-publishDiagnostics! source-file-path diagnostics) ; diagnostics : []Diagnostic
  (lsp-notify! "textDocument/publishDiagnostics" (make-PublishDiagnosticsParams
                                                    uri: (file-path->lsp-uri source-file-path)
                                                    diagnostics: diagnostics)))



; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#window_logMessage
(def (lsp-notify-window-logMessage! message type)
  (lsp-notify! "window/logMessage" (make-LogMessageParams
                                      message: message
                                      type: type)))



; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#window_showMessage
(def (lsp-notify-window-showMessage! message (type messagetype-info))
  (lsp-notify! "window/showMessage" (make-LogMessageParams
                                      message: message
                                      type: type)))
