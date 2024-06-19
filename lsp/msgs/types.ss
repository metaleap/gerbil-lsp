(export #t)

(import :std/sugar
        :std/text/json
        ../handling)


(defclass (Position JSON)
  (line character)
  final: #t)

(def (Position-load json-obj)
  (using (this (new-instance Position::t) :- Position)
    (set! this.line (hash-get json-obj "line"))
    (set! this.character (hash-get json-obj "character"))
    this))

(defclass (TextDocumentIdentifier JSON)
  (uri)
  final: #t)

(def (TextDocumentIdentifier-load json-obj)
  (using (this (new-instance TextDocumentIdentifier::t) :- TextDocumentIdentifier)
    (set! this.uri (hash-get json-obj "uri"))
    this))

(defclass (TextDocumentPositionParams JSON)
  ( (textDocument : TextDocumentIdentifier)
    (position     : Position))
  final: #f)

(def (TextDocumentPositionParams-load json-obj)
  (using (this (new-instance TextDocumentPositionParams::t) :- TextDocumentPositionParams)
    (set! this.position (Position-load (hash-get json-obj "position")))
    (set! this.textDocument (TextDocumentIdentifier-load (hash-get json-obj "textDocument")))
    this
  ))

(defclass (HoverParams TextDocumentPositionParams)
  ()
  final: #t)

(def (HoverParams-load json-obj)
  (using (this (new-instance HoverParams::t) :- HoverParams)
    (set! this.position (Position-load (hash-get json-obj "position")))
    (set! this.textDocument (TextDocumentIdentifier-load (hash-get json-obj "textDocument")))
    this
  ))
