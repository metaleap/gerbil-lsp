(export #t)

(import :std/sugar
        :std/text/json
        ./types-incoming)


(defclass (Registration JSON)
  ( id
    method
    registerOptions)
  final: #t)


(defclass (RegistrationParams JSON)
  (registrations)
  final: #t)


(defclass (DidChangeWatchedFilesRegistrationOptions JSON)
  (watchers)
  final: #t)


(defclass (FileSystemWatcher JSON)
  ( kind
    globPattern)
  final: #t)

(def watchkind-create 1)
(def watchkind-change 2)
(def watchkind-delete 4)
(def watchkind-all 7) ; 1|2|4


(defclass (MarkupContent JSON)
  ( kind
    value)
  final: #t)

(def markupkind-plaintext "plaintext")
(def markupkind-markdown "markdown")


(defclass (Hover JSON)
  ( (contents : MarkupContent)
    range)
  final: #t)


(defclass (DocumentSymbol JSON)
  ( name
    detail
    kind
    tags
    (range : Range)
    (selectionRange : Range)
    children)
  final: #t)

(def symbolkind-file 1)
(def symbolkind-module 2)
(def symbolkind-namespace 3)
(def symbolkind-package 4)
(def symbolkind-class 5)
(def symbolkind-method 6)
(def symbolkind-property 7)
(def symbolkind-field 8)
(def symbolkind-constructor 9)
(def symbolkind-enum 10)
(def symbolkind-interface 11)
(def symbolkind-function 12)
(def symbolkind-variable 13)
(def symbolkind-constant 14)
(def symbolkind-string 15)
(def symbolkind-number 16)
(def symbolkind-boolean 17)
(def symbolkind-array 18)
(def symbolkind-object 19)
(def symbolkind-key 20)
(def symbolkind-null 21)
(def symbolkind-enummember 22)
(def symbolkind-struct 23)
(def symbolkind-event 24)
(def symbolkind-operator 25)
(def symbolkind-typeparameter 26)

(def symboltag-deprecated 1)


(defclass (Location JSON)
  ( uri
    (range : Range))
  final: #t)


(defclass (WorkspaceSymbol JSON)
  ( name
    kind
    tags
    containerName
    (location : Location))
  final: #t)


(defclass (DocumentHighlight JSON)
  ( (range : Range)
    kind)
  final: #t)

(def documenthighlightkind-text 1)
(def documenthighlightkind-read 1)
(def documenthighlightkind-write 1)
