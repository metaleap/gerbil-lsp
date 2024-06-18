(import :std/sugar
        :std/format
        :std/logger
        :std/text/json
        ../handling
        ./types)


(defhandler "textDocument/hover"
  (lambda (params)
    (using (hover_params (trivial-json-object->class TextDocumentPositionParams::t params) : TextDocumentPositionParams)
      (let (markdown (format "The hover for L**~a**,C**~a** in **~a**!"
                                        (Position-line hover_params.position)
                                        (Position-character hover_params.position)
                                        (TextDocumentIdentifier-uri hover_params.textDocument)))
      (debugf ">>~a<<" markdown)
      (hash ("contents"
              (hash ("value" markdown)
                    ("kind" "markdown"))))))))
