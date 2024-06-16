(export #t)

(import :std/logger
        :std/net/json-rpc)


(def +handlers+           (make-hash-table))
(def +server-initialized+ #f)


;; See :std/net/json-rpc
(def (lsp-processor method params)
  (debugf "=== lsp-processor (~a)" method)
  ;; Special handling required by the LSP protocol
  (if (not +server-initialized+)
    (pre-init-handler method params)
    ((method-handler method) params)))


(def (pre-init-handler method params)
  (debugf "=== pre-init-handler")
  ;; Notifications are dropped at the JSON-RPC layer
  (if (equal? "initialize" method)
    ((method-handler "initialize") params)
    (json-rpc-error code: -32002 data: (void) message: "Server is not initialized.")))


;; Handlers (in ./lsp-*.ss) must return JSON-serializable objects, either an
;; instance of the Request/Notification class they are handling OR a JSON-RPC error.
;; Subclass JSON to be JSON-serializable.

(def (method-handler method)
  (debugf "=== method-handler")
  (hash-ref +handlers+ method method-not-found))

(defrule (defhandler method handler)
  (hash-put! +handlers+ method handler))
