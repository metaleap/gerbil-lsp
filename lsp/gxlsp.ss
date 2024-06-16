;;; -*- Gerbil -*-
;;; © 2024 belmarca, metaleap
;;; The Gerbil LSP Server

(export main)

(import :std/cli/getopt
        :std/io
        :std/sugar
        :std/logger
        :std/os/socket
        :std/text/json
        :std/net/json-rpc
        ./handling
        ; ./lsp-lifecycle
        (only-in :std/net/httpd/handler read-request-headers read-request-body))


(deflogger gxlsp)
(def +lsp-version+ "3.17")


;;;
;;; Misc
;;;

;; Same as in :std/net/json-rpc
(def (bytes->json b) ;; Don't intern JSON keys, using strings
  (parameterize ((json-symbolic-keys #f))
    (bytes->json-object b)))

;; IO helper
(def (write-crlf obuf)
  (def CR (char->integer #\return))
  (def LF (char->integer #\linefeed))
  (using (obuf :- BufferedWriter)
    (obuf.write-u8-inline CR)
    (obuf.write-u8-inline LF)))


;;;
;;; Serving
;;;

; Multiple transports: currently implemented below are stdio and server-socket
; (the latter single-client and blocking-while-listening-until-accept).
; If users ever really request it, further impls could include pipes or client-socket
; (where LSP client is the listener) or a more refined / multi-client server-socket.
(defstruct Transport ((reader : BufferedReader) (writer : BufferedWriter)) final: #t)
(def (transport-stdio)
  (make-Transport (open-buffered-reader (current-input-port))
                  (open-buffered-writer (current-output-port))))
(def (transport-server-socket addr-and-port)
  (using ((srv (tcp-listen addr-and-port) :- ServerSocket)
          (sock (srv.accept) :- StreamSocket))
            ; we dont use timeouts for this kind of connection: it's long-lived & single-client
            (make-Transport (open-buffered-reader (StreamSocket-reader sock))
                            (open-buffered-writer (StreamSocket-writer sock)))))


(defstruct LspReqResp ((transport : Transport) json) final: #t)

;; The serving loop: processes requests through `handle-request!`.
(def (lsp-serve (transport : Transport))
  (debugf "=== lsp-serve")
  (let ((req (make-LspReqResp transport #f))
        (res (make-LspReqResp transport #f)))
    (using ((req :- LspReqResp)
            (res :- LspReqResp))
      (def done #f)
      (while (not done)
        ; the below (before the `try`) never throws
        (let (ok-or-err (read-request! req))
          (if (eqv? #t ok-or-err) ; either way, we set `res.json`
              (set! res.json (serve-json-rpc lsp-processor req.json))
              (using (err ok-or-err :- JSON-RPCError)
                (set! res.json err #;(trivial-class->json-object err)))))
        ;; only respond to Requests, but not Notifications
        (when (hash-get req.json "id")
          (try
            (write-response! res)
          (catch (e) ; if write throws, we are irreparably disconnected
            (errorf "=== exception raised in lsp-serve ~a" e)
            (set! done #t))))))))


(def (read-request! (req : LspReqResp))
  (debugf "=== read-request!")
  (try
    (using ((ibuf (Transport-reader req.transport) :- BufferedReader))
      (let* ( (headers (read-request-headers ibuf))
              (json (bytes->json (read-request-body ibuf headers))))
                (debugf "=== Request ~a" (json-object->string json))
                (set! req.json json)
                #t))
    (catch (e)
      (debugf "Exception raised in read-request!: ~a" e)
      (internal-error e))))


(def (write-response! (res : LspReqResp))
  (def Content-Length (string->bytes "Content-Length: "))
  (def (content-length buf)
    (string->bytes (number->string (u8vector-length buf))))
  (using ((obuf (Transport-writer res.transport) :- BufferedWriter))
    (let ((out (json-object->bytes res.json)))
      (debugf "=== Response ~a" (bytes->string out))
      ;; Headers
      (obuf.write Content-Length)
      (obuf.write (content-length out))
      (write-crlf obuf)
      (write-crlf obuf)
      ;; Body
      (obuf.write out)
      (obuf.flush))))



;;;
;;; CLI
;;;



(def (main . args)
  (call-with-getopt gxlsp-main args
    program: "gxlsp"
    help: "The Gerbil LSP Server"

    (option 'addr "--addr"
              help: "The address:port to TCP-listen at, eg. 127.0.0.1:12512."
              default: "")
    ;; https://github.com/microsoft/vscode-languageserver-node/blob/33cbe5c87032cb53cf30c2b90f29d22a45ee6800/client/src/node/main.ts#L453-L462
    ; (option 'pipe
    ;           help: "Not implemented. Uses named pipe of the specified pipe name.")
    ; (option 'socket
    ;           help: "Not implemented. TCP socket connection, where the LSP client is the listener at the specified port.")
    (flag   'stdio "--stdio"
              help: "If present, forces standard-streams transport.")
    (option 'loglevel "-l" "--loglevel"
              help: "logger loglevel: 0 (error), 1 (warn), 2 (info), 3 (debug) or 4 (verbose)"
              value: string->number
              default: 0)))



(def (gxlsp-main opt)
  (let-hash opt
    (current-logger-options .?loglevel)
    (start-logger!)
    (infof "Gerbil LSP started...")
    (def transport (cond
        (.?stdio
          (transport-stdio))
        ((and .?addr (fx>0? (string-length .?addr)))
          (transport-server-socket .?addr))
        (else
          (transport-stdio))))
    (lsp-serve transport)))
