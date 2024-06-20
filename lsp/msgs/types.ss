(export #t)

(import :std/sugar
        :std/text/json
        ../handling)


(defclass (Position JSON)
  (line character)
  constructor: :init!
  final: #t)

(defmethod {:init! Position}
  (lambda (this json-obj)
    (set! this.line (hash-get json-obj "line"))
    (set! this.character (hash-get json-obj "character"))))

(defclass (TextDocumentIdentifier JSON)
  (uri)
  constructor: :init!
  final: #t)

(defmethod {:init! TextDocumentIdentifier}
  (lambda (this json-obj)
    (set! this.uri (hash-get json-obj "uri"))))

(defclass (TextDocumentPositionParams JSON)
  ( (textDocument : TextDocumentIdentifier)
    (position     : Position))
  constructor: :init!
  final: #f)

(defmethod {:init! TextDocumentPositionParams}
  (lambda (this json-obj)
    (set! this.position (make-Position (hash-get json-obj "position")))
    (set! this.textDocument (make-TextDocumentIdentifier (hash-get json-obj "textDocument")))))

(defclass (HoverParams TextDocumentPositionParams)
  ()
  constructor: :init!
  final: #t)

(defmethod {:init! HoverParams}
  (lambda (this json-obj)
    (TextDocumentPositionParams:::init! this json-obj)
    ))
