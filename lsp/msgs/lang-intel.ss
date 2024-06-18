(import :std/sugar
        :std/format
        :std/logger
        :std/text/json
        ../handling
        ./types)


(defhandler "textDocument/hover"
  (lambda (params)
    (using (hp (trivial-json-object->class HoverParams::t params) : HoverParams)
      (let (markdown (format "The hover for L**~a**,C**~a** in **~a**!"
                                        (Position-line hp.position)
                                        (Position-character hp.position)
                                        (TextDocumentIdentifier-uri hp.textDocument)))
      (debugf ">>~a<<" markdown)))
      (debugf ">>>>~a<<<<" (json-object->string hp))
      (hash ("contents"
              (hash ("value" markdown)
                    ("kind" "markdown"))))))))
