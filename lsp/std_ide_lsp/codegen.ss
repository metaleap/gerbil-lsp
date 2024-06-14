(import :std/error
        :std/format
        :std/io
        :std/text/json
        :std/sugar)
(export lsp-generate-ss)


(defclass LspSchema (metaData))
(defclass LspMetaData (version))


(def (lsp-generate-ss input-json-file-path output-ss-file-path)
  (def input-json-file-reader #f)
  (try
    (set! input-json-file-reader (open-file-reader input-json-file-path))
    (def buf-reader (open-buffered-reader input-json-file-reader))
    (def input-json (read-json buf-reader))
    (def lsp-schema (json-object->trivial-class LspSchema input-json))
    (displayln "AHOY")
    (finally
      (when input-json-file-reader
        (Reader-close input-json-file-reader))
    )
  )
)
