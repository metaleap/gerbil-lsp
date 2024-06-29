(export #t)

(import :std/sugar
        ../impl-api
        ../msgs/types-incoming
        ../msgs/types-outgoing)

(defclass LspGerbil ())

(defmethod {textDocument-documentSymbol LspGerbil}
  (lambda (_ (params :- DocumentSymbolParams))

    (void)))
