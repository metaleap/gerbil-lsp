(export lsp-handler lsp-req! +new-reqs+ lsp-handle)

(import :std/sugar
        :std/logger
        :std/net/json-rpc)


(def +handlers+ (make-hash-table))
(def +new-reqs+ (make-hash-table))

(def (lsp-handle method params)
  (def exists (hash-key? +handlers+ method))
  ((if exists debugf errorf) "=== lsp-handler '~a' ~a" method (if exists "found" "NOT found"))
  (unless (not exists)
    (let ((handler (hash-ref +handlers+ method method-not-found)))
      (try
        (handler params)
      (catch (e)
        (errorf "=== lsp-handler '~a' FAILED: ~a" method e)
        (raise e)))))) ; rethrow so Request's Response will contain the error details


;; used by `./msgs/*.ss` modules to define LSP message handlers
(def (lsp-handler method handler)
  (hash-put! +handlers+ method handler))


;; used by `./msgs/*.ss` modules to enqueue LSP requests to the client
(def (lsp-req! method params on-resp)
  (def req-id (symbol->string (gensym)))
  (hash-put! +new-reqs+ req-id (cons (json-rpc-request
                                        jsonrpc: json-rpc-version
                                        method: method
                                        params: params
                                        id: req-id)
                                      on-resp)))
