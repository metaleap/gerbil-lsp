(export #t)

(import :std/sugar
        :std/logger
        :std/net/json-rpc
        (only-in :gerbil/gambit pretty-print))


(def +handlers+ (make-hash-table))
(def +new-reqs+ (make-hash-table))
(def pretty pretty-print)

;; See :std/net/json-rpc
(def (lsp-handler method params)
  (debugf "=== lsp-handler (~a)" method)
  (let ((handler (hash-ref +handlers+ method method-not-found)))
    (handler params)))


;; used by `./msgs/*.ss` modules to define LSP message handlers
(def (lsp-handle method handler)
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
