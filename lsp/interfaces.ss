(export #t)

(import :std/sugar
        ./msgs/types-incoming
        ./msgs/types-outgoing)



; set by `lsp-serve` to a class instance implementing
; any or many of the interfaces in ./interfaces.ss
(def lsp-impl #f)



(interface TextDocument-DocumentSymbol
  (textDocument-documentSymbol (params :- DocumentSymbolParams))) ; returns: [DocumentSymbol]

(interface Workspace-Symbol
  (workspace-symbol (params :- WorkspaceSymbolParams))) ; returns: [WorkspaceSymbol]

(interface TextDocument-Definition
  (textDocument-definition (params :- DefinitionParams))) ; returns: [Location]

(interface TextDocument-References
  (textDocument-references (params :- ReferenceParams))) ; returns: [Location]

(interface TextDocument-DocumentHighlight
  (textDocument-documentHighlight (params :- DocumentHighlightParams))) ; returns: [DocumentHighlight]

(interface TextDocument-Completion
  (textDocument-completion (params :- CompletionParams))) ; returns: [CompletionItem]

(interface TextDocument-Hover
  (textDocument-hover (params :- HoverParams))) ; returns: Hover | (void)

(interface TextDocument-Rename
  (textDocument-prepareRename (params :- PrepareRenameParams))  ; returns: Range | (void)
  (textDocument-rename (params :- RenameParams)))               ; returns WorkspaceEdit | (void)

(interface TextDocument-SignatureHelp
  (textDocument-signatureHelp (params :- SignatureHelpParams))) ; returns: SignatureHelp

(interface TextDocument-CodeAction
  (textDocument-codeAction (params :- CodeActionParams))) ; returns: [Command]

(interface Workspace-ExecuteCommand
  (workspace-executeCommand (params :- ExecuteCommandParams))) ; returns: <any>

(interface Workspace-DidChangeWatchedFiles
  (workspace-didChangeWatchedFiles (params :- DidChangeWatchedFilesParams)))

(interface Workspace-DidChangeWorkspaceFolders
  (workspace-didChangeWorkspaceFolders (params :- DidChangeWorkspaceFoldersParams)))

(interface TextDocument-DidChange
  (textDocument-didChange (params :- DidChangeTextDocumentParams)))

(interface TextDocument-DidOpenClose
  (textDocument-didClose (params :- DidCloseTextDocumentParams))
  (textDocument-didOpen (params :- DidOpenTextDocumentParams)))

(interface Shutdown
  (shutdown))

(interface Initialized
  (initialized))
