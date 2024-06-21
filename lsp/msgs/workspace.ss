;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceFeatures

(export on-workspace-folders-changed workspace-folders)

(import :std/sugar
        :std/logger
        ../handling
        ./types)


(def workspace-folders [])


(def (on-workspace-folders-changed added removed)
  (def (folder->path (folder :- WorkspaceFolder))
    (substring folder.uri (string-length "file://") (string-length folder.uri)))
  (let ((added (map folder->path added))
        (removed (map folder->path removed)))
    (def (not-removed old-folder) (not (member old-folder removed)))
    (let (sans-removed (if (null? removed) workspace-folders (filter not-removed workspace-folders)))
      (let (not-already (lambda (new-folder) (not (member new-folder sans-removed))))
        (set! workspace-folders (append sans-removed (filter not-already added)))
        (debugf "workspace folders are now: ~a" workspace-folders)))))


(lsp-handle "workspace/didChangeWorkspaceFolders"
  (lambda (params)
    (let-hash params
      (on-workspace-folders-changed (map make-WorkspaceFolder .$added) (map make-WorkspaceFolder .$removed)))))
