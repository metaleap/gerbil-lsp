;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceFeatures

(export on-workspace-folders-changed workspace-folders lsp-uri->file-path)

(import :std/sugar
        :std/logger
        ../handling
        ./types)


(def workspace-folders [])


(def (lsp-uri->file-path uri)
  (substring uri (string-length "file://") (string-length uri)))


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


(lsp-handler "workspace/didCreateFiles"
  (lambda (params)
    (let-hash params
      (let (file-paths (map lsp-uri->file-path (map FileCreate-uri (map make-FileCreate .$files))))
        ; TODO: call `ide/on-source-files-created`, see https://github.com/metaleap/gerbil-lsp/blob/main/lsp/notes.md#1-workspace-syncing
        (raise "DaErr")
        (debugf "=== Source files created: ~a" file-paths)))))


; {"method":"workspace/didChangeWatchedFiles","params":{"changes":[{"uri":"file:///home/_/c/l/gerbil-lsp/lsp/msgs/foo.ss","type":1}]}}
; {"params":{"files":[{"uri":"file:///home/_/c/l/gerbil-lsp/lsp/msgs/foo.ss"}]},"method":"workspace/didDeleteFiles"}
; {"method":"workspace/didChangeWatchedFiles","params":{"changes":[{"uri":"file:///home/_/c/l/gerbil-lsp/lsp/msgs/foo.ss","type":3}]}}
; {"method":"workspace/didChangeWatchedFiles","params":{"changes":[{"uri":"file:///home/_/c/l/gerbil-lsp/lsp/msgs/workspace.ss","type":2}]}}
