(export #t)

(import :std/sugar
        :std/text/json
        ../handling)


(defclass (Position JSON)
  (line character)
  final: #t)

(defclass (TextDocumentIdentifier JSON)
  (uri)
  final: #t)

(defclass (TextDocumentPositionParams JSON)
  ( (textDocument : TextDocumentIdentifier)
    (position     : Position))
  final: #f)

(defclass (HoverParams TextDocumentPositionParams)
  ()
  final: #t)
