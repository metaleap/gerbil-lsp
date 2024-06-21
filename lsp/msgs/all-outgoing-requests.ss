(export #t)

(import :std/sugar
        :std/logger
        :std/os/socket
        :std/text/json
        :std/net/json-rpc
        ../handling
        ./types
        ./workspace)


(defclass (Registration JSON)
  ( id
    method
    registerOptions)
  final: #t)

(defclass (RegistrationParams JSON)
  (registrations)
  final: #t)

(defclass (DidChangeWatchedFilesRegistrationOptions JSON)
  (watchers)
  final: #t)

(defclass (FileSystemWatcher JSON)
  ( kind
    globPattern)
  final: #t)

(def watchkind-create 1)
(def watchkind-change 2)
(def watchkind-delete 4)
(def watchkind-all 7) ; 1|2|4


(def (lsp-req-workspace-workspacefolders)
  (lsp-req! "workspace/workspaceFolders" (void)
    (lambda (all-workspace-folders)
      (on-workspace-folders-changed (map make-WorkspaceFolder all-workspace-folders) []))))


(def (lsp-req-client-registercapability method register-options)
  (lsp-req! "client/registerCapability" (make-RegistrationParams
                                          registrations: [(make-Registration
                                                            id: (gensym)
                                                            method: method
                                                            registerOptions: register-options)])
    #f))
