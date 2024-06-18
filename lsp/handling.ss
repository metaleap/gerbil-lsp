(export #t)

(import :std/logger
        :std/net/json-rpc)


(def +handlers+           (make-hash-table))


;; See :std/net/json-rpc
(def (lsp-handler method params)
  (debugf "=== lsp-handler (~a)" method)
  (let ((handler (hash-ref +handlers+ method method-not-found)))
    (handler params)))


;; macro used by all `./msgs/*.ss` modules
(defrule (defhandler method handler)
  (hash-put! +handlers+ method handler))
