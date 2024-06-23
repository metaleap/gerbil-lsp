(export #t)

(import :std/sugar
        :std/logger
        :std/misc/func
        :std/net/json-rpc)


(def … compose1)
(def +lsp-handlers+ (make-hash-table))
(def +lsp-new-outgoing-reqs+ (make-hash-table))

(def (lsp-handle method params)
  (def exists (hash-key? +lsp-handlers+ method))
  ((if exists debugf errorf) "=== lsp-handler '~a' ~a" method (if exists "found" "NOT found"))
  (unless (not exists)
    (let ((handler (hash-ref +lsp-handlers+ method method-not-found)))
      (try
        (handler params)
      (catch (e)
        (errorf "=== lsp-handler '~a' FAILED: ~a" method e)
        (raise e)))))) ; rethrow so Request's Response will contain the error details


;; used by `./msgs/*.ss` modules to define LSP message handlers
(def (lsp-handler method handler)
  (hash-put! +lsp-handlers+ method handler))


;; used by `./msgs/*.ss` modules to enqueue LSP requests to the client
(def (lsp-request! method params on-resp)
  (def req-id (symbol->string (gensym)))
  (hash-put! +lsp-new-outgoing-reqs+  req-id
                                      (cons (json-rpc-request
                                              jsonrpc: json-rpc-version
                                              method: method
                                              params: params
                                              id: (if on-resp req-id (void)))
                                            on-resp)))


;; used by `./msgs/*.ss` modules to enqueue LSP requests to the client
(def (lsp-notify! method params)
  (hash-put! +lsp-new-outgoing-reqs+ (gensym) (cons (json-rpc-request
                                                      jsonrpc: json-rpc-version
                                                      method: method
                                                      params: params
                                                      id: (void))
                                                  #f)))
