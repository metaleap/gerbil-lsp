(export #t)

(import :std/sugar
        ./types-incoming
        ./types-outgoing)


;; a language server based on the `lsp` package is an instance of a class
;; implementing `LanguageServer` plus any of the interfaces down below


;; shared global that is set by `lsp-serve` to a `LanguageServer` impl
(def lsp-impl #f)



(interface LanguageServer
  (server-name)
  (server-version))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentSymbol
(interface LspHandlerOf-TextDocument-DocumentSymbol
  (multi-tree-label)
  (textDocument-documentSymbol (params :- DocumentSymbolParams))) ; returns: [DocumentSymbol]

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_symbol
(interface LspHandlerOf-Workspace-Symbol
  (workspace-symbol (params :- WorkspaceSymbolParams))) ; returns: [WorkspaceSymbol]

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition
(interface LspHandlerOf-TextDocument-Definition
  (textDocument-definition (params :- DefinitionParams))) ; returns: [Location]

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references
(interface LspHandlerOf-TextDocument-References
  (textDocument-references (params :- ReferenceParams))) ; returns: [Location]

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentHighlight
(interface LspHandlerOf-TextDocument-DocumentHighlight
  (textDocument-documentHighlight (params :- DocumentHighlightParams))) ; returns: [DocumentHighlight]

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion
(interface LspHandlerOf-TextDocument-Completion
  (textDocument-completion (params :- CompletionParams))) ; returns: [CompletionItem]

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover
(interface LspHandlerOf-TextDocument-Hover
  (textDocument-hover (params :- HoverParams))) ; returns: Hover | (void)

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rename
; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_prepareRename
(interface LspHandlerOf-TextDocument-Rename
  (textDocument-prepareRename (params :- PrepareRenameParams))  ; returns: Range | (void)
  (textDocument-rename (params :- RenameParams)))               ; returns WorkspaceEdit | (void)

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_signatureHelp
(interface LspHandlerOf-TextDocument-SignatureHelp
  (list-of-trigger-chars)
  (textDocument-signatureHelp (params :- SignatureHelpParams))) ; returns: SignatureHelp

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_codeAction
(interface LspHandlerOf-TextDocument-CodeAction
  (textDocument-codeAction (params :- CodeActionParams))) ; returns: [Command]

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_executeCommand
(interface LspHandlerOf-Workspace-ExecuteCommand
  (list-of-commands) ; returns: [:string]
  (workspace-executeCommand (params :- ExecuteCommandParams))) ; returns: <any>

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles
(interface LspHandlerOf-Workspace-DidChangeWatchedFiles
  (workspace-didChangeWatchedFiles (params :- DidChangeWatchedFilesParams))) ; returns: (void)

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWorkspaceFolders
(interface LspHandlerOf-Workspace-DidChangeWorkspaceFolders
  (workspace-didChangeWorkspaceFolders (params :- DidChangeWorkspaceFoldersParams))) ; returns: (void)

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange
(interface LspHandlerOf-TextDocument-DidChange
  (textDocument-didChange (params :- DidChangeTextDocumentParams))) ; returns: (void)

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didClose
; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didOpen
(interface LspHandlerOf-TextDocument-DidOpenClose
  (textDocument-didClose (params :- DidCloseTextDocumentParams)) ; returns: (void)
  (textDocument-didOpen (params :- DidOpenTextDocumentParams))) ; returns: (void)

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#shutdown
(interface LspHandlerOf-Shutdown
  (shutdown)) ; returns: (void)

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialized
(interface LspHandlerOf-Initialized
  (initialized client-capabilities)) ; returns: (void)
