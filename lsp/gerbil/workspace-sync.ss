(export #t)

(import :std/sugar
        :std/format
        :std/logger
        :std/misc/func
        :std/misc/list
        :std/misc/string
        ./common
        ../msgs/outgoing
        ../msgs/types-incoming
        ../msgs/types-outgoing)


(def … compose1)
(def err-msg-fmt-fs "=== ignoring FS err while scrutinizing path ~a: ~a")
(def source-file-extensions [".ss"]) ; TODO: discuss & decide: add .scm or not?



(def (source-file-path? file-path)
  (or (string-suffix? "/gerbil.pkg" file-path)
      (any (lambda (file-path-ext)
              (string-suffix? file-path-ext file-path)
            ) source-file-extensions)))


(def (fs-path-not-dotted? path)
  (not (string-contains path "/.")))


(def (fs-path-in-dir? dir-path)
  (lambda (path)
    (let (dir-path (string-append (string-trim-suffix "/" dir-path) "/"))
      (string-prefix? dir-path path))))


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
      (errorf err-msg-fmt-fs path e)))))
  ret)


(def (on-source-file-changes removed added changed) ; each arg is a list of file paths
  (set! removed (filter (lambda (path) (member path source-file-paths)) removed))
  (def changed-in-added (filter (lambda (path) (member path source-file-paths)) added))
  (def added-in-changed (filter (lambda (path) (not (member path source-file-paths))) changed))
  (set! added (append added-in-changed (filter (lambda (path) (not (member path changed-in-added))) added)))
  (set! changed (append changed-in-added (filter (lambda (path) (not (member path added-in-changed))) changed)))
  (unless (null? removed)
    (set! source-file-paths (filter (lambda (path) (not (member path removed))) source-file-paths))
    (debugf "=== source files removed ~a" removed))
  (unless (null? added)
    (set! source-file-paths (unique (append source-file-paths added)))
    (debugf "=== source files added: ~a" added))
  (unless (null? changed)
    (debugf "=== source files changed ~a" changed))
  ; TODO: call `ide/on-source-file-changes`, see ../notes.md#1-workspace-syncing
  )


(def (on-workspace-folders-changed added removed)
  (def (folder->path (folder :- WorkspaceFolder))
    (lsp-uri->file-path folder.uri))
  (let ((added (filter fs-path-not-dotted? (map folder->path added)))
        (removed (filter fs-path-not-dotted? (map folder->path removed))))
    (def file-paths-removed [])
    (def file-paths-added [])
    (for-each! removed (lambda (dir-path)
      (set! file-paths-removed (append file-paths-removed (filter (fs-path-in-dir? dir-path) source-file-paths)))))
    (for-each! added (lambda (dir-path)
      (let (file-paths (fs-dir-source-files dir-path))
        (for-each! file-paths (lambda (file-path)
          (unless (member file-path source-file-paths)
              (set! file-paths-added (append file-paths-added [file-path]))))))))
    (on-source-file-changes file-paths-removed file-paths-added [])))



(defmethod {workspace-didChangeWorkspaceFolders LspGerbil}
  (lambda (_ (params :- DidChangeWorkspaceFoldersParams))
    (on-workspace-folders-changed (WorkspaceFoldersChangeEvent-added params.event)
                                  (WorkspaceFoldersChangeEvent-removed params.event))))


(defmethod {workspace-didChangeWatchedFiles LspGerbil}
  (let ((file-event-file-path (… lsp-uri->file-path FileEvent-uri))
        (file-event-check-ext (… source-file-path? lsp-uri->file-path FileEvent-uri)))
    (lambda (_ (params :- DidChangeWatchedFilesParams))
      (def (file-event-check-type file-change-type)
        (lambda ((it :- FileEvent)) (fx= it.type file-change-type)))
      (def changes (unique params.changes)) ; unique: because vscode sent duplicates at times

      ; first, grab actual Gerbil source file (not folder) events...
      (def file-events (filter file-event-check-ext changes))
      (def file-paths-deleted (map file-event-file-path (filter (file-event-check-type filechangetype-deleted) file-events)))
      (def file-paths-created (map file-event-file-path (filter (file-event-check-type filechangetype-created) file-events)))
      (def file-paths-changed (map file-event-file-path (filter (file-event-check-type filechangetype-changed) file-events)))

      ; sadly, folder events (such as moving or deleting one) are not
      ; translated by vscode (and possibly also not by other LSP clients)
      ; into just individual file events, so we have to do it ourselves here
      (for-each! changes (lambda ((file-event :- FileEvent))
        (def path (file-event-file-path file-event))
        (when (fs-path-not-dotted? path)
          (cond
            ((eq? file-event.type filechangetype-deleted)
              (set! file-paths-deleted (append  file-paths-deleted
                                                (filter (fs-path-in-dir? path) source-file-paths))))
            ((eq? file-event.type filechangetype-created)
              (try
                (def fs-info (file-info path))
                (when (eq? 'directory (file-info-type fs-info))
                  (let (file-paths (fs-dir-source-files path))
                    (for-each! file-paths (lambda (file-path)
                      (unless (member file-path source-file-paths)
                          (set! file-paths-created  (append file-paths-created [file-path])))))))
              (catch (e)
                (errorf err-msg-fmt-fs path e))))))))

      (on-source-file-changes (filter fs-path-not-dotted? file-paths-deleted)
                              (filter fs-path-not-dotted? file-paths-created)
                              (filter fs-path-not-dotted? file-paths-changed)))))



(defmethod {textDocument-didChange LspGerbil}
  (lambda (_ (params :- DidChangeTextDocumentParams))
    (using (it params.textDocument :- VersionedTextDocumentIdentifier)
      (let (source-file-path (lsp-uri->file-path it.uri))
        (when (source-file-path? source-file-path)
          ; TODO: call `ide/on-source-file-edited`, see ../notes.md#1-workspace-syncing
          (debugf "=== source file edited: ~a" source-file-path))))))


(defmethod {textDocument-didClose LspGerbil}
  (lambda (_ (params :- DidCloseTextDocumentParams))
    (using (it params.textDocument :- TextDocumentIdentifier)
      (let (source-file-path (lsp-uri->file-path it.uri))
        (when (source-file-path? source-file-path)
          (lsp-notify-textDocument-publishDiagnostics! source-file-path [])
          (debugf "=== source file closed: ~a" source-file-path))))))


(defmethod {textDocument-didOpen LspGerbil}
  (lambda (_ (params :- DidOpenTextDocumentParams))
    (using (it params.textDocument :- TextDocumentItem)
      (let (source-file-path (lsp-uri->file-path it.uri))
        (when (source-file-path? source-file-path)
          (lsp-notify-textDocument-publishDiagnostics! source-file-path [
            (make-Diagnostic  range: (make-Range (make-Position 0 0) (make-Position 0 4))
                              severity: diagnosticseverity-information
                              code: 0001
                              source: "gerbil"
                              tags: []
                              message: "TODO: take this out of `textDocument/didOpen` once `ide/on-file-notices-changed` is ready")
            ])
          (debugf "=== source file opened: ~a" source-file-path))))))


(defmethod {initialized LspGerbil}
  (lambda (_ _)
    (lsp-request-workspace-workspaceFolders!
      (lambda (workspace-folders)
        (on-workspace-folders-changed (map make-WorkspaceFolder workspace-folders) [])))))
