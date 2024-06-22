;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceFeatures
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_synchronization

(export #t)

(import :std/sugar
        :std/logger
        :std/misc/list
        :std/misc/path
        ../handling
        ./types)


(def source-file-extensions [".ss"])
(def all-source-files [])
(def workspace-folders [])


(def (lsp-uri->file-path uri)
  (substring uri (string-length "file://") (string-length uri)))


(def (source-file-path? file-path)
  (any (lambda (file-path-ext)
    (path-extension-is? file-path (string-append file-path-ext))
  ) source-file-extensions))


(def (fs-path-not-dotted? path)
  (not (string-contains path "/.")))


(def (fs-dir-source-files dir-path)
  (def ret [])
  (def names (directory-files [path: dir-path ignore-hidden: 'dot-and-dot-dot]))
  (def paths (filter fs-path-not-dotted? (map (lambda (name) (path-expand name dir-path)) names)))
  (for-each! paths (lambda (path)
    (try
      (def fs-info (file-info path))
      (if (eq? 'directory (file-info-type fs-info))
        (set! ret (append ret (fs-dir-source-files path)))
        (when (source-file-path? path)
          (set! ret (append ret [path]))))
    (catch (e)
      (errorf "=== ignoring FS err while scrutinizing path ~a: ~a" path e)))))
  ret)


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
    (using (params (make-DidChangeWorkspaceFoldersParams params) :- DidChangeWorkspaceFoldersParams)
      (on-workspace-folders-changed (WorkspaceFoldersChangeEvent-added params.event)
                                    (WorkspaceFoldersChangeEvent-removed params.event)))))


(lsp-handler "workspace/didChangeWatchedFiles"
  (let ((file-event-file-path (… lsp-uri->file-path FileEvent-uri))
        (file-event-check-ext (… source-file-path? lsp-uri->file-path FileEvent-uri)))
    (lambda (params)
      (def (file-event-check-type file-change-type)
        (lambda ((it :- FileEvent)) (fx= it.type file-change-type)))
      (using (params (make-DidChangeWatchedFilesParams params) :- DidChangeWatchedFilesParams)
        (def changes (unique params.changes)) ; unique: because vscode sends duplicates at times

        ; first, grab actual Gerbil source file (not folder) events...
        (def file-events (filter file-event-check-ext changes))
        (def file-events-deleted (filter (file-event-check-type filechangetype-deleted) file-events))
        (def file-events-created (filter (file-event-check-type filechangetype-created) file-events))
        (def file-events-changed (filter (file-event-check-type filechangetype-changed) file-events))

        ; sadly, folder events (such as moving or deleting one) are not
        ; translated by vscode (and maybe also not by other LSP clients)
        ; into just individual file events, so we have to do it ourselves here
        (for-each! changes (lambda ((file-event :- FileEvent))
          (def path (file-event-file-path file-event))
          (when (and (not (eq? filechangetype-changed file-event.type)) (fs-path-not-dotted? path))
            (try
              (def fs-info (file-info path))
              (when (eq? 'directory (file-info-type fs-info))
                (debugf "ISDIR:~a" path))
            (catch (e)
              (errorf "=== ignoring FS err while scrutinizing path ~a: ~a" path e))))))

        (on-source-file-changes (filter fs-path-not-dotted? (map file-event-file-path file-events-deleted))
                                (filter fs-path-not-dotted? (map file-event-file-path file-events-created))
                                (filter fs-path-not-dotted? (map file-event-file-path file-events-changed)))))))


(lsp-handler "textDocument/didOpen"
  (lambda (params)
    (using (params (make-DidOpenTextDocumentParams params) :- DidOpenTextDocumentParams)
      (using (it params.textDocument :- TextDocumentItem)
        (when (string=? it.languageId "gerbil")
          (debugf "=== source file opened: ~a" (lsp-uri->file-path it.uri)))))))


(lsp-handler "textDocument/didClose"
  (lambda (params)
    (using (params (make-DidCloseTextDocumentParams params) :- DidCloseTextDocumentParams)
      (using (it params.textDocument :- TextDocumentIdentifier)
        (let (source-file-path (lsp-uri->file-path it.uri))
          (when (source-file-path? source-file-path)
            (debugf "=== source file closed: ~a" source-file-path)))))))


(lsp-handler "textDocument/didChange"
  (lambda (params)
    (using (params (make-DidChangeTextDocumentParams params) :- DidChangeTextDocumentParams)
      (using (it params.textDocument :- VersionedTextDocumentIdentifier)
        (let (source-file-path (lsp-uri->file-path it.uri))
          (when (source-file-path? source-file-path)
            ; TODO: call `ide/on-source-file-edited`, see https://github.com/metaleap/gerbil-lsp/blob/main/lsp/notes.md#1-workspace-syncing
            (debugf "=== source file edited: ~a" source-file-path)))))))
