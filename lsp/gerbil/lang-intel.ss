(export #t)

(import :std/sugar
        :std/format
        ../msgs/types-incoming
        ../msgs/types-outgoing
        ./common
        ./workspace-sync)



(defmethod {textDocument-documentSymbol LspGerbil}
  (lambda (_ (params :- DocumentSymbolParams))
    (let (source-file-path (lsp-file->file-path params.textDocument))
      ; TODO: produce real results obtained from ../notes.md#defs-in-file
      (let (sub (make-DocumentSymbol  name:           "SubSymbol"
                                      detail:         "a single child symbol"
                                      kind:           symbolkind-class
                                      tags:           []
                                      range:          (make-Range (make-Position 2 1)
                                                                  (make-Position 2 8))
                                      selectionRange: (make-Range (make-Position 2 2)
                                                                  (make-Position 2 6))
                                      children:       []))
        [(make-DocumentSymbol name: "Gerbil"
                              detail:         (format "**TODO:** call `ide/defs-in-file` with `~a`"
                                                      source-file-path)
                              kind:           symbolkind-class
                              tags:           []
                              range:          (make-Range (make-Position 0 1)
                                                          (make-Position 2 8))
                              selectionRange: (make-Range (make-Position 0 2)
                                                          (make-Position 2 6))
                              children:       [sub])]))))

(defmethod {multi-tree-label LspGerbil}
  (lambda (_)
    "TODO_DocumentSymbol_MultiTreeLabel"))


(defmethod {workspace-symbol LspGerbil}
  (lambda (_ (params :- WorkspaceSymbolParams))
    (if (null? source-file-paths)
        []
        [(make-WorkspaceSymbol  name: "Gerbil"
                                kind: symbolkind-function
                                tags: []
                                containerName: (string-append "**TODO:** call `ide/defs-search` with `" params.query "`")
                                location: (make-Location  uri:    (car source-file-paths)
                                                          range:  (make-Range  (make-Position 0 1)
                                                                              (make-Position 0 4))))])))


(defmethod {textDocument-definition LspGerbil}
  (lambda (_ (params :- DefinitionParams))
    (let (source-file-path (lsp-file->file-path params.textDocument))
      [ (make-Location  uri:    source-file-path
                        range:  (make-Range (make-Position 0 1)
                                            (make-Position 0 4)))
        (make-Location uri:   source-file-path
                      range:  (make-Range (make-Position 2 1)
                                          (make-Position 2 4)))])))


(defmethod {textDocument-references LspGerbil}
  (lambda (_ (params :- ReferenceParams))
    (let (source-file-path (lsp-file->file-path params.textDocument))
      [ (make-Location  uri:    source-file-path
                        range:  (make-Range (make-Position 0 1)
                                            (make-Position 0 4)))
        (make-Location  uri:    source-file-path
                        range:  (make-Range (make-Position 2 1)
                                            (make-Position 2 4)))])))


(defmethod {textDocument-documentHighlight LspGerbil}
  (lambda (_ (params :- DocumentHighlightParams))
    (let (source-file-path (lsp-file->file-path params.textDocument))
      [ (make-DocumentHighlight range:  (make-Range (make-Position 0 1)
                                                    (make-Position 0 4))
                                kind:   documenthighlightkind-text)
        (make-DocumentHighlight range:  (make-Range (make-Position 2 1)
                                                    (make-Position 2 4))
                                kind:   documenthighlightkind-text)])))


(defmethod {textDocument-completion LspGerbil}
  (lambda (_ (params :- CompletionParams))
      (let (source-file-path (lsp-file->file-path params.textDocument))
        (let (content (format "**TODO:** call `ide/info-items` with `~a` and L~a,C~a."
                                source-file-path
                                (Position-line params.position)
                                (Position-character params.position)))
          [(make-CompletionItem label:          "TODO"
                                labelDetails:   (make-CompletionItemLabelDetails  detail: "_LD_detail_"
                                                                                  description: "_LD_description_")
                                kind:           completionitemkind-function
                                tags:           []
                                detail:         "DetailGoesHere"
                                documentation:  (make-MarkupContent kind: markupkind-markdown
                                                                    value: content))]))))


(defmethod {textDocument-hover LspGerbil}
  (lambda (_ (params :- HoverParams))
      (let (source-file-path (lsp-file->file-path params.textDocument))
        (let (content (format "**TODO:** call `ide/info-items` with `~a` and L~a,C~a."
                              source-file-path
                              (Position-line params.position)
                              (Position-character params.position)))
          (make-Hover range:    (void)
                      contents: (make-MarkupContent kind:   markupkind-markdown
                                                    value:  content))))))


(defmethod {textDocument-prepareRename LspGerbil}
  (lambda (_ (params :- PrepareRenameParams))
    (let (source-file-path (lsp-file->file-path params.textDocument))
      (make-Range params.position
                  (make-Position  (Position-line params.position)
                                  (+ 4 (Position-character params.position)))))))


(defmethod {textDocument-rename LspGerbil}
  (lambda (_ (params :- RenameParams))
    (let (source-file-path (lsp-file->file-path params.textDocument))
      (make-WorkspaceEdit
        changes: (hash (,(file-path->lsp-uri source-file-path) [
          (make-TextEdit  newText:  params.newName
                          range:    (make-Range params.position
                                                (make-Position  (Position-line params.position)
                                                                (+ 4 (Position-character params.position)))))
        ]))))))


(defmethod {list-of-trigger-chars LspGerbil}
  (lambda (_)
    [" " "\t"]))

(defmethod {textDocument-signatureHelp LspGerbil}
  (lambda (_ (params :- SignatureHelpParams))
    (let (source-file-path (lsp-file->file-path params.textDocument))
      (make-SignatureHelp
        signatures: (if (fx>0? (Position-line params.position)) [] [(make-SignatureInformation
          label:          "(foo bar baz)"
          documentation:  (make-MarkupContent kind: markupkind-markdown
                                              value: (format  "**TODO:** call `ide/signatures` with ~a"
                                                              source-file-path)))])))))


(defmethod {textDocument-codeAction LspGerbil}
  (lambda (_ (params :- CodeActionParams))
    (if (equal? (Range-start params.range) (Range-end params.range)) []
      (let (source-file-path (lsp-file->file-path params.textDocument))
        (if client-is-gerbil-vscode-ext
            []
            [(make-Command title: "Eval" command: "eval-in-file" arguments: [params])])))))


(defmethod {list-of-commands LspGerbil}
  (lambda (_)
    ["announce-gerbil-vscode-ext" "eval-in-file" "eval-expr"]))

(defmethod {workspace-executeCommand LspGerbil}
  (lambda (_ (params :- ExecuteCommandParams))
    (case params.command
      (("announce-gerbil-vscode-ext")
        (set! client-is-gerbil-vscode-ext #t))
      (("eval-in-file")
        (cmd-eval-in-file (car params.arguments) (make-Range (cadr params.arguments))))
      (("eval-expr")
        (cmd-eval-expr (car params.arguments) (cadr params.arguments)))
      (else
        (raise (format "Unknown command: ~a" params.command))))))


(def (cmd-eval-in-file source-file-path (range :- Range))
  (if (not (source-file-path? source-file-path))
    (raise (format "invalid Gerbil source file path: ~a" source-file-path))
    (format "TODO: Summon Le Eval Overlord for ~a @ ~a .. ~a" source-file-path range.start range.end)))


(def (cmd-eval-expr source-file-path expr)
  (format "TODO: Summon Le Eval Overlord for expr `~a` in ctx of file '~a'" expr source-file-path))
