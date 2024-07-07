(export #t)

(import :std/sugar
        :std/text/json
        ./types-incoming)



(def (file-path->lsp-uri file-path)
  (string-append "file://" file-path))



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
(def documenthighlightkind-read 2)
(def documenthighlightkind-write 3)


(defclass (CompletionItemLabelDetails JSON)
  ( detail
    description)
  final: #t)


(defclass (CompletionItem JSON)
  ( label
    labelDetails ; null or CompletionItemLabelDetails
    kind
    tags
    detail
    documentation ; null or MarkupContent
    )
  final: #t)

(def completionitemkind-text 1)
(def completionitemkind-method 2)
(def completionitemkind-function 3)
(def completionitemkind-constructor 4)
(def completionitemkind-field 5)
(def completionitemkind-variable 6)
(def completionitemkind-class 7)
(def completionitemkind-interface 8)
(def completionitemkind-module 9)
(def completionitemkind-property 10)
(def completionitemkind-unit 11)
(def completionitemkind-value 12)
(def completionitemkind-enum 13)
(def completionitemkind-keyword 14)
(def completionitemkind-snippet 15)
(def completionitemkind-color 16)
(def completionitemkind-file 17)
(def completionitemkind-reference 18)
(def completionitemkind-folder 19)
(def completionitemkind-enummember 20)
(def completionitemkind-constant 21)
(def completionitemkind-struct 22)
(def completionitemkind-event 23)
(def completionitemkind-operator 24)
(def completionitemkind-typeparameter 25)


(defclass (WorkspaceEdit JSON)
  ( changes) ; hashmap{ filepath: []TextEdit }
  final: #t)


(defclass (TextEdit JSON)
  ( (range : Range)
    newText)
  final: #t)


(defclass (SignatureHelp JSON)
  ( signatures) ; []SignatureInformation
  final: #t)


(defclass (SignatureInformation JSON)
  ( label
    documentation ; null or MarkupContent
    )
  final: #t)


(defclass (PublishDiagnosticsParams JSON)
  ( uri
    diagnostics) ; []Diagnostic
  final: #t)


(defclass (Diagnostic JSON)
  ( (range : Range)
    severity
    code
    source
    message
    tags)
  final: #t)

(def diagnosticseverity-error 1)
(def diagnosticseverity-warning 2)
(def diagnosticseverity-information 3)
; (def diagnosticseverity-hint 4) ; exists in LSP but not showing up in VSCode => it doesn't exist

(def diagnostictag-unnecessary 1)
(def diagnostictag-deprecated 2)


(defclass (Command JSON)
  ( title
    command
    arguments)
  final: #t)


(defclass (LogMessageParams JSON)
  ( type
    message)
  final: #t)

(def messagetype-error 1)
(def messagetype-warning 2)
(def messagetype-info 3)
(def messagetype-log 4)
(def messagetype-debug 5)
