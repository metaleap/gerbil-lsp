(export #t)

(import :std/logger
        :std/net/json-rpc)


(def +handlers+ (make-hash-table))
(def +new-reqs+ (make-hash-table))


;; See :std/net/json-rpc
(def (lsp-handler method params)
  (debugf "=== lsp-handler (~a)" method)
  (let ((handler (hash-ref +handlers+ method method-not-found)))
    (handler params)))


;; used by all `./msgs/*.ss` modules
(def (lsp-handle method handler)
  (hash-put! +handlers+ method handler))

(def (lsp-req! method params on-resp)
  (def req-id (gensym))
  (def req (json-rpc-request  jsonrpc: json-rpc-version
                              method: method
                              params: params
                              id: (symbol->string req-id)))
  (hash-put! +new-reqs+ req-id req))
