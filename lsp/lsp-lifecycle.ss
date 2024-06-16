(import :std/cli/getopt
        :std/io
        :std/sugar
        :std/logger
        :std/os/socket
        :std/text/json
        :std/net/json-rpc
        ./handling)

(def +server-name+        "gxlsp")
(def +server-version+     "0.0.1")
(def +server-info+        (hash ("name" +server-name+) ("version" +server-version+)))


(defstruct LspClient  ( client-name client-version ;; obtained from InitializeParams
                        initializationOptions capabilities workspaceFolders ;; dito
                        initialized ;; set on receipt of the Initialized notification
                      ) final: #t)
;; gxlsp is a single-client server for now.
(def +lsp-client+ (make-LspClient #f #f #f #f #f #f))



;; Initialize Request handler
(defclass (InitializeResult JSON) (capabilities serverInfo)
  constructor: :init!)
(defmethod {:init! InitializeResult}
  (lambda (self capabilities serverInfo)
    (class-instance-init! self capabilities: capabilities serverInfo: serverInfo)))


(defhandler "initialize"
  (lambda (params)
    (debugf "=== initialize handler")
    (if +server-initialized+
      (internal-error "Server already initialized.")
      (initialize-server params))))


;; A client should complete initialization by sending an 'initialized' Notification.
;; For us, this is a no-op.
(defhandler "initialized"
  (lambda (params)
    (LspClient-initialized-set! +lsp-client+ #t)))


(def (initialize-server params)
  (debugf "=== initialize-server")
  ;; Just crash on void params
  (let/cc return
    (using (lsp-client +lsp-client+ :- LspClient)
      (try
       (let-hash params
         ;; Use the new .$ accessor for string keys
         (when .$clientInfo
           (set! lsp-client.client-name    (hash-get .$clientInfo "name"))
           (set! lsp-client.client-version (hash-get .$clientInfo "version")))
         (set! lsp-client.initializationOptions .$initializationOptions)
         (set! lsp-client.capabilities          .$capabilities)
         (set! lsp-client.workspaceFolders      .$workspaceFolders)
         (let (capabilities (generate-capabilities params))
           (set! +server-initialized+ #t)
           (debugf "=== set up lsp-client struct")
           (return capabilities)))
       (catch (e)
         (errorf "=== exception in initialize-server ~a" e)
         (raise e))))))


(def (generate-capabilities params)
  (InitializeResult (hash
                      ("positionEncoding" "utf-16")
                      ("workspace" (hash ("workspaceFolders" (hash ("supported" #t)))))
                      ("hoverProvider" #t)
                    ) +server-info+))
