;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceFeatures
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_synchronization

(import :std/sugar
        :std/logger
        :std/misc/list
        :std/misc/string
        ../handling
        ../interfaces
        ./types-incoming
        ./types-outgoing
        ./outgoing)



(lsp-handler "workspace/didChangeWorkspaceFolders"
  (lambda (params)
    (using (lsp-impl :- Workspace-DidChangeWorkspaceFolders)
      {lsp-impl.workspace-didChangeWorkspaceFolders (make-DidChangeWorkspaceFoldersParams params)})))


(lsp-handler "workspace/didChangeWatchedFiles"
  (lambda (params)
    (when (is-Workspace-DidChangeWatchedFiles? lsp-impl) ; check needed in _this_ case
      (using (lsp-impl :- Workspace-DidChangeWatchedFiles)
        {lsp-impl.workspace-didChangeWatchedFiles (make-DidChangeWatchedFilesParams params)}))))


(lsp-handler "textDocument/didOpen"
  (lambda (params)
    (using (lsp-impl :- TextDocument-DidOpenClose)
      {lsp-impl.textDocument-didOpen (make-DidOpenTextDocumentParams params)})))


(lsp-handler "textDocument/didClose"
  (lambda (params)
    (using (lsp-impl :- TextDocument-DidOpenClose)
      {lsp-impl.textDocument-didClose (make-DidCloseTextDocumentParams params)})))


(lsp-handler "textDocument/didChange"
  (lambda (params)
    (using (lsp-impl :- Workspace-DidChangeWatchedFiles)
      {lsp-impl.workspace-didChangeWatchedFiles (make-DidChangeWatchedFilesParams params)})))
