(import :std/sugar
        ../handling)


(defhandler "textDocument/hover"
  (lambda (params)
    (let-hash params
      (hash ("contents"
              (hash ("value" "Mark**down** _text_ with\n\n```\n(syntax \"highlighting\" 4 'u)\n```\n\nNeato!") ("kind" "markdown")))))))
