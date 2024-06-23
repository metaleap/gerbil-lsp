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
  (case-lambda
    ((this line character)
      (set! this.line line)
      (set! this.character character))
    ((this json-obj) (when json-obj
      (set! this.line (hash-get json-obj "line"))
      (set! this.character (hash-get json-obj "character"))))))



(defclass (TextDocumentIdentifier JSON)
  (uri)
  constructor: :init!
  equal: #t
  print: #t
  final: #f)

(defmethod {:init! TextDocumentIdentifier}
  (lambda (this json-obj) (when json-obj
    (set! this.uri (hash-get json-obj "uri")))))



(defclass (TextDocumentPositionParams JSON)
  ( (textDocument : TextDocumentIdentifier)
    (position     : Position))
  constructor: :init!
  equal: #t
  print: #t
  final: #f)

(defmethod {:init! TextDocumentPositionParams}
  (lambda (this json-obj) (when json-obj
    (set! this.position (make-Position (hash-get json-obj "position")))
    (set! this.textDocument (make-TextDocumentIdentifier (hash-get json-obj "textDocument"))))))



(defclass (HoverParams TextDocumentPositionParams)
  ()
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! HoverParams}
  (lambda (this json-obj) (when json-obj
    (TextDocumentPositionParams:::init! this json-obj))))



(defclass (FileEvent JSON)
  ( uri
    type)
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! FileEvent}
  (lambda (this json-obj) (when json-obj
    (set! this.uri (hash-get json-obj "uri"))
    (set! this.type (hash-get json-obj "type")))))

(def filechangetype-created 1)
(def filechangetype-changed 2)
(def filechangetype-deleted 3)



(defclass (DidChangeWatchedFilesParams JSON)
  ( changes)
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! DidChangeWatchedFilesParams}
  (lambda (this json-obj) (when json-obj
      (set! this.changes (map make-FileEvent (hash-get json-obj "changes"))))))



(defclass (WorkspaceFoldersChangeEvent JSON)
  ( added
    removed)
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! WorkspaceFoldersChangeEvent}
  (lambda (this json-obj) (when json-obj
      (set! this.added (map make-WorkspaceFolder (hash-get json-obj "added")))
      (set! this.removed (map make-WorkspaceFolder (hash-get json-obj "removed"))))))



(defclass (DidChangeWorkspaceFoldersParams JSON)
  ( (event : WorkspaceFoldersChangeEvent))
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! DidChangeWorkspaceFoldersParams}
  (lambda (this json-obj) (when json-obj
    (set! this.event (make-WorkspaceFoldersChangeEvent (hash-get json-obj "event"))))))



(defclass (WorkspaceFolder JSON)
  ( uri
    name)
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! WorkspaceFolder}
  (lambda (this json-obj) (when json-obj
    (set! this.name (hash-get json-obj "name"))
    (set! this.uri (hash-get json-obj "uri")))))



(defclass (TextDocumentItem JSON)
  ( uri
    languageId
    version
    text)
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! TextDocumentItem}
  (lambda (this json-obj) (when json-obj
    (set! this.uri (hash-get json-obj "uri"))
    (set! this.languageId (hash-get json-obj "languageId"))
    (set! this.version (hash-get json-obj "version"))
    (set! this.text (hash-get json-obj "text")))))



(defclass (DidOpenTextDocumentParams JSON)
  ( (textDocument : TextDocumentItem))
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! DidOpenTextDocumentParams}
  (lambda (this json-obj) (when json-obj
    (set! this.textDocument (make-TextDocumentItem (hash-get json-obj "textDocument"))))))



(defclass (DidCloseTextDocumentParams JSON)
  ( (textDocument : TextDocumentIdentifier))
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! DidCloseTextDocumentParams}
  (lambda (this json-obj) (when json-obj
    (set! this.textDocument (make-TextDocumentIdentifier (hash-get json-obj "textDocument"))))))



(defclass (VersionedTextDocumentIdentifier TextDocumentIdentifier)
  ( version)
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! VersionedTextDocumentIdentifier}
  (lambda (this json-obj) (when json-obj
    (TextDocumentIdentifier:::init! this json-obj)
    (set! this.version (hash-get json-obj "version")))))



(defclass (Range JSON)
  ( (start : Position)
    (end : Position))
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! Range}
  (case-lambda
    ((this start end)
      (set! this.start start)
      (set! this.end end))
    ((this json-obj)
      (when json-obj
        (set! this.start (make-Position (hash-get json-obj "start")))
        (set! this.end (make-Position (hash-get json-obj "end")))))))



(defclass (TextDocumentContentChangeEvent JSON)
  ( (range : Range)
    text)
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! TextDocumentContentChangeEvent}
  (lambda (this json-obj) (when json-obj
    (set! this.range (make-Range (hash-get json-obj "range")))
    (set! this.text (hash-get json-obj "text")))))



(defclass (DidChangeTextDocumentParams JSON)
  ( (textDocument : VersionedTextDocumentIdentifier)
    contentChanges)
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! DidChangeTextDocumentParams}
  (lambda (this json-obj) (when json-obj
      (set! this.textDocument (make-VersionedTextDocumentIdentifier (hash-get json-obj "textDocument")))
      (set! this.contentChanges (map make-TextDocumentContentChangeEvent (hash-get json-obj "contentChanges"))))))



(defclass (DocumentSymbolParams JSON)
  ( (textDocument : TextDocumentIdentifier))
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! DocumentSymbolParams}
  (lambda (this json-obj) (when json-obj
    (set! this.textDocument (make-TextDocumentIdentifier (hash-get json-obj "textDocument"))))))



(defclass (WorkspaceSymbolParams JSON)
  ( query)
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! WorkspaceSymbolParams}
  (lambda (this json-obj) (when json-obj
    (set! this.query (hash-get json-obj "query")))))



(defclass (DefinitionParams TextDocumentPositionParams)
  ()
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! DefinitionParams}
  (lambda (this json-obj) (when json-obj
    (TextDocumentPositionParams:::init! this json-obj))))



(defclass (ReferenceParams TextDocumentPositionParams)
  ()
  constructor: :init!
  equal: #t
  print: #t
  final: #t)

(defmethod {:init! ReferenceParams}
  (lambda (this json-obj) (when json-obj
    (TextDocumentPositionParams:::init! this json-obj))))
