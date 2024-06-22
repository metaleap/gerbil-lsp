(export #t)

(import :std/sugar
        :std/logger
        :std/os/socket
        :std/text/json
        :std/net/json-rpc
        ../handling
        ./types-incoming
        ./types-outgoing
        ./workspace-sync)


(def (lsp-req-workspace-workspacefolders!)
  (lsp-req! "workspace/workspaceFolders" (void)
    (lambda (all-workspace-folders)
      (on-workspace-folders-changed (map make-WorkspaceFolder all-workspace-folders) []))))


(def (lsp-req-client-registercapability! method register-options)
  (lsp-req! "client/registerCapability" (make-RegistrationParams
                                          registrations: [(make-Registration
                                                            id: (gensym)
                                                            method: method
                                                            registerOptions: register-options)])
    #f))
