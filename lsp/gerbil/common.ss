(export #t)


(def source-file-paths [])
(def client-is-gerbil-vscode-ext #f)

(defclass LspGerbil ())


;; implement `LanguageServer` interface:

(defmethod {server-name LspGerbil}    (lambda (_) "gxlsp"))
(defmethod {server-version LspGerbil} (lambda (_) "0.0.1"))
