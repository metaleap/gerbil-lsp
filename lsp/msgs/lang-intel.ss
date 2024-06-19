(import :std/sugar
        :std/format
        :std/logger
        :std/text/json
        ../handling
        ./types)


(defhandler "textDocument/hover" (lambda (params)
  (using (hp (make-HoverParams params) :- HoverParams)
    (let (markdown (format "Da hover for L**~a**,C**~a** in **~a**!"
                              (Position-line hp.position)
                              (Position-character hp.position)
                              (TextDocumentIdentifier-uri hp.textDocument)))
      (hash ("contents"
              (hash ("value" markdown)
                    ("kind" "markdown"))))))))
