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


(def (on-source-files-created file-paths)
  ; TODO: call `ide/on-source-files-created`, see https://github.com/metaleap/gerbil-lsp/blob/main/lsp/notes.md#1-workspace-syncing
  (debugf "=== source files created: ~a" file-paths))


(def (on-source-files-deleted file-paths)
  ; TODO: call `ide/on-source-files-deleted`, see https://github.com/metaleap/gerbil-lsp/blob/main/lsp/notes.md#1-workspace-syncing
  (debugf "=== source files deleted: ~a" file-paths))


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
      (let (file-paths (map (… lsp-uri->file-path FileCreate-uri make-FileCreate) .$files))
        (on-source-files-created (filter source-file-path? file-paths))))))


(lsp-handler "workspace/didDeleteFiles"
  (lambda (params)
    (let-hash params
      (let (file-paths (map (… lsp-uri->file-path FileDelete-uri make-FileDelete) .$files))
        (on-source-files-deleted (filter source-file-path? file-paths))))))


(lsp-handler "workspace/didRenameFiles"
  (lambda (params)
    (let-hash params
      (def all (map make-FileRename .$files))

      (def files-added (filter (lambda ((it :- FileRename))
        (and  (not (source-file-path? (lsp-uri->file-path it.oldUri)))
              (source-file-path? (lsp-uri->file-path it.newUri)))
        ) all))
      (def files-removed (filter (lambda ((it :- FileRename))
        (and  (source-file-path? (lsp-uri->file-path it.oldUri))
              (not (source-file-path? (lsp-uri->file-path it.newUri))))
        ) all))
      (def files-renamed (filter (lambda ((it :- FileRename))
        (and  (source-file-path? (lsp-uri->file-path it.oldUri))
              (source-file-path? (lsp-uri->file-path it.newUri)))
        ) all))

      (on-source-files-created (map (… lsp-uri->file-path FileRename-newUri) files-added))
      (on-source-files-deleted (map (… lsp-uri->file-path FileRename-oldUri) files-removed))
      (let (file-renames (map (lambda ((it :- FileRename)) (cons it.oldUri it.newUri)) files-renamed))
        ; TODO: call `ide/on-source-files-renamed`, see https://github.com/metaleap/gerbil-lsp/blob/main/lsp/notes.md#1-workspace-syncing
        (debugf "=== Source files renamed: ~a" file-renames)
      ))))


(lsp-handler "workspace/didChangeWatchedFiles"
  (lambda (params)
    (void)))


; {"method":"workspace/didChangeWatchedFiles","params":{"changes":[{"uri":"file:///home/_/c/l/gerbil-lsp/lsp/msgs/foo.ss","type":1}]}}
; {"params":{"files":[{"uri":"file:///home/_/c/l/gerbil-lsp/lsp/msgs/foo.ss"}]},"method":"workspace/didDeleteFiles"}
; {"method":"workspace/didChangeWatchedFiles","params":{"changes":[{"uri":"file:///home/_/c/l/gerbil-lsp/lsp/msgs/foo.ss","type":3}]}}
; {"method":"workspace/didChangeWatchedFiles","params":{"changes":[{"uri":"file:///home/_/c/l/gerbil-lsp/lsp/msgs/workspace.ss","type":2}]}}
