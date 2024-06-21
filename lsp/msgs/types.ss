(export #t)

(import :std/sugar
        :std/text/json
        :std/misc/func
        ../handling)


(def … compose1)


(defclass (Position JSON)
  ( line
    character)
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! Position}
  (lambda (this json-obj)
    (set! this.line (hash-get json-obj "line"))
    (set! this.character (hash-get json-obj "character"))))

(defclass (TextDocumentIdentifier JSON)
  (uri)
  constructor: :init!
  equal: #t
  print: #t
  final: #f)

(defmethod {:init! TextDocumentIdentifier}
  (lambda (this json-obj)
    (set! this.uri (hash-get json-obj "uri"))))

(defclass (TextDocumentPositionParams JSON)
  ( (textDocument : TextDocumentIdentifier)
    (position     : Position))
  constructor: :init!
  equal: #t
  print: #t
  final: #f)

(defmethod {:init! TextDocumentPositionParams}
  (lambda (this json-obj)
    (set! this.position (make-Position (hash-get json-obj "position")))
    (set! this.textDocument (make-TextDocumentIdentifier (hash-get json-obj "textDocument")))))

(defclass (HoverParams TextDocumentPositionParams)
  ()
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! HoverParams}
  (lambda (this json-obj)
    (TextDocumentPositionParams:::init! this json-obj)
  ))

(defclass (FileCreate TextDocumentIdentifier)
  ()
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! FileCreate}
  (lambda (this json-obj)
    (TextDocumentIdentifier:::init! this json-obj)
  ))

(defclass (WorkspaceFolder JSON)
  ( uri
    name)
  constructor: :init!
  equal: #t
  print: #t
  final: #f)

(defmethod {:init! WorkspaceFolder}
  (lambda (this json-obj)
    (set! this.name (hash-get json-obj "name"))
    (set! this.uri (hash-get json-obj "uri"))))
