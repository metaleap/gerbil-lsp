;;; -*- Gerbil -*-
;;; © belmarca 2024
;;; The Gerbil LSP Server

(import (only-in :gerbil/gambit pretty-print)
        :std/cli/getopt
        :std/io
        :std/io/dummy
        :std/error
        :std/actor
        :std/interface
        :std/sugar
        :std/logger
        :std/contract
        :std/os/socket
        :std/text/json
        :std/net/json-rpc
        (only-in :std/net/httpd/handler read-request-headers read-request-body))

(export main)

;;;
;;; Globals
;;;

(deflogger gxlsp)

(def +lsp-version+        "3.17")
(def +server-name+        "gxlsp")
(def +server-version+     "0.0.1")
(def +server-info+        (hash ("name" +server-name+) ("version" +server-version+)))
(def +initialized+        #f)  ;; Is the server initialized?

(def +request-timeout+    60)
(def +response-timeout+   120)
(def +input-buffer-size+  (expt 2 13))
(def +output-buffer-size+ (expt 2 15))

(def +handlers+           (make-hash-table))

;;;
;;; Misc
;;;

;; Same as in :std/net/json-rpc
(def (bytes->json b) ;; Don't intern JSON keys, using strings
  (parameterize ((json-symbolic-keys #f)) (bytes->json-object b)))

;; IO helper
(def CR (char->integer #\return))
(def LF (char->integer #\linefeed))
(def (write-crlf obuf)
  (using (obuf :- BufferedWriter)
    (obuf.write-u8-inline CR)
    (obuf.write-u8-inline LF)))

;;;
;;; Server API
;;;


; Multiple transports: currently implemented below are stdio and server-socket
; (the latter single-client and blocking-while-listening-until-accept).
; If users ever really request it, further impls could include pipes or client-socket
; (where LSP client is the listener) or a more refined / multi-client server-socket.

(defstruct Transport ((reader : Reader) (writer : Writer)) final: #t)
(def (transport-stdio)
  (make-Transport (Reader (make-raw-binary-input-port (current-input-port)))
                  (Writer (make-raw-binary-output-port (current-output-port)))))
(def (transport-server-socket addr-and-port)
  (using ((srv (tcp-listen addr-and-port) :- ServerSocket)
          (sock (srv.accept) :- StreamSocket))
            (sock.set-input-timeout! +request-timeout+)
            (sock.set-output-timeout! +response-timeout+)
            (make-Transport (StreamSocket-reader sock)
                            (StreamSocket-writer sock))))

;; Start the server.
(def (start! addr-and-port)
  (start-logger!)
  (infof "Gerbil LSP started...")
  (thread-yield!)
  ; TODO: transport choice via CLI arg!
  (lsp-server (transport-stdio)))
  ; (lsp-server (transport-server-socket addr-and-port)))

;; Structures for handling state
(defstruct LspReqResp (buf (transport : Transport) json)
  final: #t)
(defstruct LspClient (  client-name client-version ;; obtained from InitializeParams
                        initializationOptions capabilities workspaceFolders ;; dito
                        initialized) ;; set on receipt of the Initialized notification
  final: #t)

;; gxlsp is a single-client server so far.
(def +lsp-client+ (make-LspClient #f #f #f #f #f #f))

;; Main entry point.
;; transport <- Transport
;; Processes requests through `handle-request!`.
(def (lsp-server (transport : Transport))
  (debugf "=== lsp-server")
  (def ibuf (open-buffered-reader transport.reader +input-buffer-size+))
  (def obuf (open-buffered-writer transport.writer +output-buffer-size+))
  (let ((req (make-LspReqResp ibuf transport #f))
        (res (make-LspReqResp obuf transport #f)))
    (using ((req :- LspReqResp)
            (res :- LspReqResp))
      (def ok #t)
      (while ok ; break out on transport-disconnected or incoming non-JSON payload (ie. not an LSP client) — both cases conveniently caught by a throwing read attempt
        (try
          (set! ok (read-request! req))
          ;; Try to handle the Notification/Request
          (serve-json-rpc! res lsp-processor req.json)
          ;; Only respond to Requests, but not Notifications
          (if (hash-get req.json "id")
            (write-response! res))
        (catch (e)
          (errorf "=== exception raised in lsp-server ~a" e)
          (internal-error e))
        (finally
          ;; Reset the buffers but don't close the transport
          (set! req.json #f)
          (set! res.json #f)
          (displayln ">>>RESET-REQ") (thread-yield!)
          (&BufferedReader-reset! req.buf transport.reader #f)
          (displayln ">>>RESET-RESP") (thread-yield!)
          (&BufferedWriter-reset! res.buf transport.writer #f)           (displayln ">>>RESET-DONE") (thread-yield!) ))))))

;; Same as in :std/net/json-rpc but store the result in a struct
;; res <- LspReqResp
(def (serve-json-rpc! (res : LspReqResp) processor request-json)
  (set! res.json (serve-json-rpc processor request-json)))

;; Perform buffered io on the request and parse the body
;; as json, storing it in the req object
;; req <- LspReqResp
(def (read-request! (req : LspReqResp))
  (debugf "=== read-request!")
  (try
    (let* ((ibuf req.buf)
          (headers (read-request-headers ibuf))
          (json (bytes->json (read-request-body ibuf headers))))
      (debugf "=== Request ~a" (json-object->string json))
      (set! req.json json)
      #t)
    (catch (e)
      (debugf "Exception raised in read-request!: ~a" e)
      (internal-error e)
      #f)))

;; Buffered IO on the transport
;; res <- LspReqResp
(def (write-response! (res : LspReqResp))
  (def Content-Length (string->bytes "Content-Length: "))
  (def (content-length buf)
    (string->bytes (number->string (u8vector-length buf))))
  (using ((obuf res.buf :- BufferedWriter))
    (let ((out (json-object->bytes res.json)))
      (debugf "=== Response ~a" (bytes->string out))
      ;; Headers
      (obuf.write Content-Length)
      (obuf.write (content-length out))
      (write-crlf obuf)
      (write-crlf obuf)
      ;; Body
      (obuf.write (json-object->bytes res.json))
      (obuf.flush))))

;; See :std/net/json-rpc
(def (lsp-processor method params)
  (debugf "=== lsp-processor (~a)" method)
  ;; Special handling required by the LSP protocol
  (if (not +initialized+)
    (pre-init-handler method params)
    ((method-handler method) params)))

;;; Handlers

;; Handlers must return JSON-serializable objects, either an instance
;; of the Request/Notification class they are handling OR a JSON-RPC error.
;; Subclass JSON to be JSON-serializable.

(def (method-handler method)
  (debugf "=== method-handler")
  (hash-ref +handlers+ method method-not-found))

(defrule (defhandler method handler)
  (hash-put! +handlers+ method handler))

;; Initialize Request handler
(defclass (InitializeResult JSON) (capabilities serverInfo)
  constructor: :init!)
(defmethod {:init! InitializeResult}
  (lambda (self capabilities serverInfo)
    (class-instance-init! self capabilities: capabilities serverInfo: serverInfo)))

(defhandler "initialize"
  (lambda (params)
    (debugf "=== initialize handler")
    (if +initialized+
      (internal-error "Server already initialized.")
      (initialize-server params))))

;; A client should complete initialization by sending an 'initialized' Notification.
;; For us, this is a no-op.
(defhandler "initialized"
  (lambda (params)
    (LspClient-initialized-set! +lsp-client+ #t)))

(def (pre-init-handler method params)
  (debugf "=== pre-init-handler")
  ;; Notifications are dropped at the JSON-RPC layer
  (if (equal? "initialize" method)
    ((method-handler "initialize") params)
    (json-rpc-error code: -32002 data: (void) message: "Server is not initialized.")))

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
           (set! +initialized+ #t)
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

;;; CLI

;; Argument handling
(def address-port-optional-argument
  (optional-argument 'address-port
                     help: "the address:port to bind to"
                     default: "127.0.0.1:12512"))

(def loglevel-option
  (option 'loglevel "-l" "--loglevel"
          help: "logger loglevel"
          value: string->number
          default: 0))

(def (main . args)
  (call-with-getopt gxlsp-main args
    program: "gxlsp"
    help: "The Gerbil LSP Server"
    address-port-optional-argument
    loglevel-option))

(def (gxlsp-main opt)
  (let-hash opt
    (current-logger-options .?loglevel)
    (start! .?address-port)))
