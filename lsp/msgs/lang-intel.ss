;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#languageFeatures

(import :std/sugar
        :std/format
        :std/logger
        :std/text/json
        ../handling
        ./types-incoming
        ./types-outgoing)


(lsp-handler "textDocument/documentSymbol"
  (lambda (params)
    ; TODO: produce real results obtained from ../notes.md#defs-in-file
    (using (params (make-DocumentSymbolParams params) :- DocumentSymbolParams)
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
                                              (TextDocumentIdentifier-uri params.textDocument))
                            kind: symbolkind-class
                            tags: []
                            range: (make-Range  (make-Position 0 1)
                                                (make-Position 2 8))
                            selectionRange: (make-Range (make-Position 0 2)
                                                        (make-Position 2 6))
                            children: [sub])]))))


(lsp-handler "workspace/symbol"
  (lambda (params)
    ; TODO: produce real results obtained from ../notes.md#defs-search
    (using (params (make-WorkspaceSymbolParams params) :- WorkspaceSymbolParams)
      [(make-WorkspaceSymbol  name: "Gerbil"
                              kind: symbolkind-function
                              tags: []
                              containerName: (string-append "**TODO:** call `defs-search` with `" params.query "`")
                              location: (make-Location  uri: "file:///home/_/c/l/gerbil-lsp/lsp/msgs/types-incoming.ss"
                                                        range: (make-Range  (make-Position 0 1)
                                                                            (make-Position 0 4))))])))


(lsp-handler "textDocument/definition"
  (lambda (params)
    ; TODO: produce real results obtained from ../notes.md#lookup
    (using (params (make-DefinitionParams params) :- DefinitionParams)
      [ (make-Location uri: "file:///home/_/c/l/gerbil-lsp/lsp/msgs/types-incoming.ss"
                      range: (make-Range  (make-Position 0 1)
                                          (make-Position 0 4)))
        (make-Location uri: "file:///home/_/c/l/gerbil-lsp/lsp/msgs/types-incoming.ss"
                      range: (make-Range  (make-Position 2 1)
                                          (make-Position 2 4)))])))


(lsp-handler "textDocument/references"
  (lambda (params)
    ; TODO: produce real results obtained from ../notes.md#lookup
    (using (params (make-ReferenceParams params) :- ReferenceParams)
      [ (make-Location uri: "file:///home/_/c/l/gerbil-lsp/lsp/msgs/types-incoming.ss"
                      range: (make-Range  (make-Position 0 1)
                                          (make-Position 0 4)))
        (make-Location uri: "file:///home/_/c/l/gerbil-lsp/lsp/msgs/types-incoming.ss"
                      range: (make-Range  (make-Position 2 1)
                                          (make-Position 2 4)))])))


(lsp-handler "textDocument/hover"
  (lambda (params)
    ; TODO: produce real results obtained from ../notes.md#info-tips
    (using (hp (make-HoverParams params) :- HoverParams)
      (let (content (format "**TODO:** call `ide/info-tips` with `~a` and L~a,C~a."
                                (TextDocumentIdentifier-uri hp.textDocument)
                                (Position-line hp.position)
                                (Position-character hp.position)))
        (make-Hover range: (void)
                    contents: (make-MarkupContent kind: markupkind-markdown
                                                  value: content))))))
