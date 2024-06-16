(export #t)

(import :std/logger
        :std/net/json-rpc)


(def +handlers+           (make-hash-table))


;; See :std/net/json-rpc
(def (lsp-processor method params)
  (debugf "=== lsp-processor (~a)" method)
  ((method-handler method) params))


;; Handlers (in ./lsp-*.ss) must return JSON-serializable objects, either an
;; instance of the Request/Notification class they are handling OR a JSON-RPC error.
;; Subclass JSON to be JSON-serializable.

(def (method-handler method)
  (debugf "=== method-handler")
  (hash-ref +handlers+ method method-not-found))

(defrule (defhandler method handler)
  (hash-put! +handlers+ method handler))
