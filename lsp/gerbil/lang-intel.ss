(export #t)

(import :std/sugar
        :std/format
        ./common
        ../msgs/types-incoming
        ../msgs/types-outgoing)

(defmethod {textDocument-documentSymbol LspGerbil}
  (lambda (_ (params :- DocumentSymbolParams))
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
                              children: [sub])]))))


(defmethod {workspace-symbol LspGerbil}
  (lambda (_ (params :- WorkspaceSymbolParams))
    (void)))
