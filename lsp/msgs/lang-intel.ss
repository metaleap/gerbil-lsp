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


(def client-is-gerbil-vscode-ext #f)


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
    ; TODO: produce real results obtained from ../notes.md#info-items
    (using (params (make-HoverParams params) :- HoverParams)
      (let (source-file-path (lsp-file->file-path params.textDocument))
        (let (content (format "**TODO:** call `ide/info-items` with `~a` and L~a,C~a."
                                  source-file-path
                                  (Position-line params.position)
                                  (Position-character params.position)))
          (make-Hover range: (void)
                      contents: (make-MarkupContent kind: markupkind-markdown
                                                    value: content)))))))


(lsp-handler "textDocument/prepareRename"
  (lambda (params)
    ; TODO: produce real results obtained from ../notes.md#can-rename
    (using (params (make-PrepareRenameParams params) :- PrepareRenameParams)
      (let (source-file-path (lsp-file->file-path params.textDocument))
        (make-Range params.position
                    (make-Position  (Position-line params.position)
                                    (+ 4 (Position-character params.position))))))))


(lsp-handler "textDocument/rename"
  (lambda (params)
    ; TODO: produce real results obtained from ../notes.md#rename
    (using (params (make-RenameParams params) :- RenameParams)
      (let (source-file-path (lsp-file->file-path params.textDocument))
        (make-WorkspaceEdit
          changes: (hash (,(file-path->lsp-uri source-file-path) [
            (make-TextEdit  newText: params.newName
                            range: (make-Range  params.position
                                                (make-Position  (Position-line params.position)
                                                                (+ 4 (Position-character params.position)))))
          ])))))))


(lsp-handler "textDocument/signatureHelp"
  (lambda (params)
    ; TODO: produce real results obtained from ../notes.md#signatures
    (using (params (make-SignatureHelpParams params) :- SignatureHelpParams)
      (let (source-file-path (lsp-file->file-path params.textDocument))
        (make-SignatureHelp
          signatures: (if (fx>0? (Position-line params.position)) [] [(make-SignatureInformation
            label:  "(foo bar baz)"
            documentation: (make-MarkupContent
              kind: markupkind-markdown
              value: (format "**TODO:** call `ide/signatures` with ~a" source-file-path)))]))))))


(lsp-handler "textDocument/codeAction"
  (lambda (params)
    (using (params (make-CodeActionParams params) :- CodeActionParams)
      (if (equal? (Range-start params.range) (Range-end params.range)) []
        (let (source-file-path (lsp-file->file-path params.textDocument))
          (if client-is-gerbil-vscode-ext
              []
              [(make-Command title: "Eval" command: "eval-in-file" arguments: [params])]))))))


(lsp-handler "workspace/executeCommand"
  (lambda (params)
    ; TODO: send code-eval reqs to `ide`'s current-file interp session eventually
    (using (params (make-ExecuteCommandParams params) :- ExecuteCommandParams)
      (case params.command
        (("announce-gerbil-vscode-ext")
          (set! client-is-gerbil-vscode-ext #t))
        (("eval-in-file")
          (cmd-eval-in-file (car params.arguments) (make-Range (cadr params.arguments))))
        (("eval-expr")
          (cmd-eval-expr (car params.arguments) (cadr params.arguments)))
        (else
          (raise (format "Unknown command: ~a" params.command)))))))


(def (cmd-eval-in-file source-file-path (range :- Range))
  (if (not (source-file-path? source-file-path))
    (raise (format "invalid Gerbil source file path: ~a" source-file-path))
    (format "TODO: Summon Le Eval Overlord for ~a @ ~a .. ~a" source-file-path range.start range.end)))


(def (cmd-eval-expr source-file-path expr)
  (format "TODO: Summon Le Eval Overlord for expr `~a` in ctx of file '~a'" expr source-file-path))
