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
                      ) final: #t)
;; gxlsp is a single-client server for now.
(def +lsp-client+ (make-LspClient #f #f #f #f #f))



(defhandler "initialize"
  (lambda (params)
    (debugf "=== initialize handler")
    (using (lsp-client +lsp-client+ :- LspClient)
      (let-hash params
        (when .$clientInfo
          (set! lsp-client.client-name    (hash-get .$clientInfo "name"))
          (set! lsp-client.client-version (hash-get .$clientInfo "version")))
        (set! lsp-client.initializationOptions .$initializationOptions)
        (set! lsp-client.capabilities          .$capabilities)
        (set! lsp-client.workspaceFolders      .$workspaceFolders)
        (hash ("serverInfo"   +server-info+)
              ("capabilities" (hash
                                  ("positionEncoding" "utf-16") ; sadly mandatory
                                  ("workspace" (hash ("workspaceFolders" (hash ("supported" #t)))))
                                  ("hoverProvider" #t)
                                )))))))


;; A client should complete initialization by sending an 'initialized' Notification.
;; For us, this is a no-op for now.
(defhandler "initialized"
  (lambda (params)
    (void)))
