;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#languageFeatures

(import :std/sugar
        :std/format
        :std/logger
        :std/text/json
        ../handling
        ./types-incoming
        ./types-outgoing)


(def tmp-some-file-path #f) ; TODO: remove once `lsp-handler "workspace/symbol"` uses `ide/defs-search`


(def (lsp-file->file-path (file : TextDocumentIdentifier))
  (set! tmp-some-file-path (lsp-uri->file-path (TextDocumentIdentifier-uri file)))
  tmp-some-file-path) ; TODO: ditch the `set!` & return directly once `lsp-handler "workspace/symbol"` uses `ide/defs-search`


(lsp-handler "textDocument/documentSymbol"
  (lambda (params)
    ; TODO: produce real results obtained from ../notes.md#defs-in-file
    (using (params (make-DocumentSymbolParams params) :- DocumentSymbolParams)
      (let (source-file-path (lsp-file->file-path params.textDocument))
        (let (sub (make-DocumentSymbol  name: "SubSymbol"
                                        detail: "a single child symbol"
                                        kind: symbolkind-class
                                        tags: []
                                        range: (make-Range  (make-Position 2 1)
                                                            (make-Position 2 8))
                                        selectionRange: (make-Range (make-Position 2 2)
                                                                    (make-Position 2 6))
                                        children: []))
          [(make-DocumentSymbol name: "Gerbil"
                                detail: (format "**TODO:** call `ide/defs-in-file` with `~a`"
                                                source-file-path)
                                kind: symbolkind-class
                                tags: []
                                range: (make-Range  (make-Position 0 1)
                                                    (make-Position 2 8))
                                selectionRange: (make-Range (make-Position 0 2)
                                                            (make-Position 2 6))
                                children: [sub])])))))


(lsp-handler "workspace/symbol"
  (lambda (params)
    ; TODO: produce real results obtained from ../notes.md#defs-search
    (using (params (make-WorkspaceSymbolParams params) :- WorkspaceSymbolParams)
      (if (not tmp-some-file-path) [] [
        (make-WorkspaceSymbol name: "Gerbil"
                              kind: symbolkind-function
                              tags: []
                              containerName: (string-append "**TODO:** call `ide/defs-search` with `" params.query "`")
                              location: (make-Location  uri: tmp-some-file-path
                                                        range: (make-Range  (make-Position 0 1)
                                                                            (make-Position 0 4))))]))))


(lsp-handler "textDocument/definition"
  (lambda (params)
    ; TODO: produce real results obtained from ../notes.md#lookup
    (using (params (make-DefinitionParams params) :- DefinitionParams)
      (let (source-file-path (lsp-file->file-path params.textDocument))
      [ (make-Location uri: source-file-path
                      range: (make-Range  (make-Position 0 1)
                                          (make-Position 0 4)))
        (make-Location uri: source-file-path
                      range: (make-Range  (make-Position 2 1)
                                          (make-Position 2 4)))]))))


(lsp-handler "textDocument/references"
  (lambda (params)
    ; TODO: produce real results obtained from ../notes.md#lookup
    (using (params (make-ReferenceParams params) :- ReferenceParams)
      (let (source-file-path (lsp-file->file-path params.textDocument))
        [ (make-Location uri: source-file-path
                        range: (make-Range  (make-Position 0 1)
                                            (make-Position 0 4)))
          (make-Location uri: source-file-path
                        range: (make-Range  (make-Position 2 1)
                                            (make-Position 2 4)))]))))


(lsp-handler "textDocument/documentHighlight"
  (lambda (params)
    ; TODO: produce real results obtained from ../notes.md#occurrences
    (using (params (make-DocumentHighlightParams params) :- DocumentHighlightParams)
      (let (source-file-path (lsp-file->file-path params.textDocument))
        [ (make-DocumentHighlight range: (make-Range  (make-Position 0 1)
                                                      (make-Position 0 4))
                                  kind: documenthighlightkind-text)
          (make-DocumentHighlight range: (make-Range  (make-Position 2 1)
                                                      (make-Position 2 4))
                                  kind: documenthighlightkind-text)]))))




(lsp-handler "textDocument/completion"
  (lambda (params)
    ; TODO: produce real results obtained from ../notes.md#completions
    (using (params (make-CompletionParams params) :- CompletionParams)
      (let (source-file-path (lsp-file->file-path params.textDocument))
        (let (content (format "**TODO:** call `ide/info-items` with `~a` and L~a,C~a."
                                source-file-path
                                (Position-line params.position)
                                (Position-character params.position)))
          [(make-CompletionItem label: "TODO"
                                labelDetails: (make-CompletionItemLabelDetails detail: "_LD_detail_" description: "_LD_description_")
                                kind: completionitemkind-function
                                tags: []
                                detail: "DetailGoesHere"
                                documentation: (make-MarkupContent  kind: markupkind-markdown
                                                                    value: content))])))))


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
    (debugf "PR01")
    ; TODO: produce real results obtained from ../notes.md#can-rename
    (using (params (make-PrepareRenameParams params) :- PrepareRenameParams)
      (debugf "PR02")
      (let (source-file-path (lsp-file->file-path params.textDocument))
        (debugf "PR03")
        (let (ret (make-Range params.position
                              (make-Position  (Position-line params.position)
                                              (+ 4 (Position-character params.position)))))
          (debugf "PR04")
          ret)))))


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
          [(make-Command title: "Eval" command: "eval-in-file" arguments: [params])])))))


(lsp-handler "workspace/executeCommand"
  (lambda (params)
    ; TODO: send code-eval reqs to `ide`'s current-file interp session eventually
    (using (params (make-ExecuteCommandParams params) :- ExecuteCommandParams)
      (case params.command
        (("eval-in-file") 123)
        (else 321)))))
