;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#languageFeatures

(import :std/sugar
        :std/format
        :std/logger
        :std/text/json
        ../handling
        ../interfaces
        ./types-incoming
        ./types-outgoing
        ./outgoing
        ./workspace-sync)



(lsp-handler "textDocument/documentSymbol"
  (lambda (params)
    (using (lsp-impl :- TextDocument-DocumentSymbol)
      {lsp-impl.textDocument-documentSymbol (make-DocumentSymbolParams params)})))


(lsp-handler "workspace/symbol"
  (lambda (params)
    (using (lsp-impl :- Workspace-Symbol)
      {lsp-impl.workspace-symbol (make-WorkspaceSymbolParams params)})))


(lsp-handler "textDocument/definition"
  (lambda (params)
    (using (lsp-impl :- TextDocument-Definition)
      {lsp-impl.textDocument-definition (make-DefinitionParams params)})))


(lsp-handler "textDocument/references"
  (lambda (params)
    (using (lsp-impl :- TextDocument-References)
      {lsp-impl.textDocument-references (make-ReferenceParams params)})))


(lsp-handler "textDocument/documentHighlight"
  (lambda (params)
    (using (lsp-impl :- TextDocument-DocumentHighlight)
      {lsp-impl.textDocument-documentHighlight (make-DocumentHighlightParams params)})))


(lsp-handler "textDocument/completion"
  (lambda (params)
    (using (lsp-impl :- TextDocument-Completion)
      {lsp-impl.textDocument-completion (make-CompletionParams params)})))


(lsp-handler "textDocument/hover"
  (lambda (params)
    (using (lsp-impl :- TextDocument-Hover)
      {lsp-impl.textDocument-hover (make-HoverParams params)})))


(lsp-handler "textDocument/prepareRename"
  (lambda (params)
    (using (lsp-impl :- TextDocument-Rename)
      {lsp-impl.textDocument-prepareRename (make-PrepareRenameParams params)})))


(lsp-handler "textDocument/rename"
  (lambda (params)
    (using (lsp-impl :- TextDocument-Rename)
      {lsp-impl.textDocument-rename (make-RenameParams params)})))


(lsp-handler "textDocument/signatureHelp"
  (lambda (params)
    (using (lsp-impl :- TextDocument-SignatureHelp)
      {lsp-impl.textDocument-signatureHelp (make-SignatureHelpParams params)})))


(lsp-handler "textDocument/codeAction"
  (lambda (params)
    (using (lsp-impl :- TextDocument-CodeAction)
      {lsp-impl.textDocument-codeAction (make-CodeActionParams params)})))


(lsp-handler "workspace/executeCommand"
  (lambda (params)
    (using (lsp-impl :- Workspace-ExecuteCommand)
      {lsp-impl.workspace-executeCommand (make-ExecuteCommandParams params)})))
