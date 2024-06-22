;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceFeatures

(export #t)

(import :std/sugar
        :std/logger
        :std/misc/list
        :std/misc/path
        ../handling
        ./types)


(def source-file-extensions [".ss"])
(def workspace-folders [])


(def (lsp-uri->file-path uri)
  (substring uri (string-length "file://") (string-length uri)))


(def (source-file-path? file-path)
  (any (lambda (file-path-ext)
    (path-extension-is? file-path (string-append file-path-ext))
  ) source-file-extensions))


(def (on-source-file-changes deleted created changed)
  ; TODO: call `ide/on-source-file-changes`, see https://github.com/metaleap/gerbil-lsp/blob/main/lsp/notes.md#1-workspace-syncing
  (unless (null? deleted)
    (debugf "=== source files deleted ~a" deleted))
  (unless (null? created)
    (debugf "=== source files created: ~a" created))
  (unless (null? changed)
    (debugf "=== source files changed ~a" changed)))


(def (on-workspace-folders-changed added removed)
  (def (folder->path (folder :- WorkspaceFolder))
    (lsp-uri->file-path folder.uri))
  (let ((added (map folder->path added))
        (removed (map folder->path removed)))
    (def (not-removed old-folder) (not (member old-folder removed)))
    (let (sans-removed (if (null? removed) workspace-folders (filter not-removed workspace-folders)))
      (let (not-already (lambda (new-folder) (not (member new-folder sans-removed))))
        (set! workspace-folders (append sans-removed (filter not-already added)))
        ; TODO: call `ide/on-root-folders-changed`, see https://github.com/metaleap/gerbil-lsp/blob/main/lsp/notes.md#1-workspace-syncing
        (debugf "=== workspace folders are now: ~a" workspace-folders)))))


(lsp-handler "workspace/didChangeWorkspaceFolders"
  (lambda (params)
    (let-hash params
      (on-workspace-folders-changed (map make-WorkspaceFolder .$added) (map make-WorkspaceFolder .$removed)))))


(lsp-handler "workspace/didChangeWatchedFiles"
  (let ((file-event-file-path (… lsp-uri->file-path FileEvent-uri))
        (file-event-check-ext (… source-file-path? lsp-uri->file-path FileEvent-uri)))
    (lambda (params)
      (def (file-event-check-type file-change-type) (lambda ((it :- FileEvent)) (fx= it.type file-change-type)))
      (let-hash params
        (def file-events (filter  file-event-check-ext
                                  (unique (map make-FileEvent .$changes)))) ; unique: because vscode sends duplicates at times
        (def file-paths-deleted (filter (file-event-check-type filechangetype-deleted) file-events))
        (def file-paths-created (filter (file-event-check-type filechangetype-created) file-events))
        (def file-paths-changed (filter (file-event-check-type filechangetype-changed) file-events))
        (on-source-file-changes (file-event-file-path file-paths-deleted)
                                (file-event-file-path file-paths-created)
                                (file-event-file-path file-paths-changed))))))
