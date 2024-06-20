(export lsp-transport-stdio
        lsp-transport-server-socket
        lsp-serve)

(import :std/cli/getopt
        :std/io
        :std/sugar
        :std/logger
        :std/os/socket
        :std/text/json
        :std/net/json-rpc
        ./handling
        ./msgs/lifecycle
        ./msgs/lang-intel
        (only-in :std/net/httpd/handler read-request-headers read-request-body))



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

(def +pending-reqs+ (make-hash-table))

; Multiple transports: currently implemented below are stdio and server-socket
; (the latter single-client and blocking-while-listening-until-accept).
; If users ever really request it, further impls could include pipes or client-socket
; (where LSP client is the listener) or a more refined / multi-client server-socket.
(defstruct Transport ((reader : BufferedReader) (writer : BufferedWriter)) final: #t)
(def (lsp-transport-stdio)
  (make-Transport (open-buffered-reader (current-input-port))
                  (open-buffered-writer (current-output-port))))
(def (lsp-transport-server-socket addr-and-port)
  (using ((srv (tcp-listen addr-and-port) :- ServerSocket)
          (sock (srv.accept) :- StreamSocket))
            ; we dont use timeouts for this kind of connection: it's long-lived & single-client
            (make-Transport (open-buffered-reader (sock.reader))
                            (open-buffered-writer (sock.writer)))))


(defstruct LspReqResp ((transport : Transport) json) final: #t)

;; The serving loop: processes requests through `handle-request!`.
(def (lsp-serve (transport : Transport))
  (debugf "=== lsp-serve")
  (let ((incoming (make-LspReqResp transport #f))
        (outgoing (make-LspReqResp transport #f)))
    (using ((incoming :- LspReqResp)
            (outgoing :- LspReqResp))
      (def done #f)
      (while (not done)
        (let (ok-or-err (read-msg! incoming))
          (let ((msg-id (hash-get incoming.json "id"))
                (msg-method (hash-get incoming.json "method")))
            (if (not (eqv? #t ok-or-err)) ; either way, we set `res.json`
              ; received an error message
              (using (err ok-or-err :- JSON-RPCError)
                (set! outgoing.json err #;(trivial-class->json-object err)))
              ; else, received a normal message
              (if msg-method
                ; msg is an incoming request or notification
                (set! outgoing.json (serve-json-rpc lsp-handler incoming.json))
                ; else, msg is an incoming response
                (let (handler (hash-get +pending-reqs+ msg-id))
                  (when handler
                    (hash-remove! +pending-reqs+ msg-id)
                    (try
                      (handler (hash-get incoming.json "result"))
                      (catch (e)
                        (debugf "response handler failed on response '~a': ~a"
                                  (json-object->string incoming.json) e)))))))))
        ; if any writes throw, we are irreparably disconnected
        (try
          ; only respond to Requests, but not Notifications or Responses
          (when (hash-get incoming.json "id")
            (write-msg! outgoing))
          ; send out requests that have piled up, if any
          (hash-for-each (lambda (req-id req-and-handler)
              (hash-put! +pending-reqs+ req-id (cdr req-and-handler))
              (write-msg! (make-LspReqResp transport (car req-and-handler)))
            ) +new-reqs+)
          (hash-clear! +new-reqs+)
          (catch (e)
            (errorf "=== exception raised in lsp-serve ~a" e)
            (set! done #t)))))))


(def (read-msg! (incoming : LspReqResp))
  (try
    (using ((ibuf (Transport-reader incoming.transport) :- BufferedReader))
      (begin
        (def headers (read-request-headers ibuf))
        (def json (bytes->json (read-request-body ibuf headers)))
        (debugf "=== RECV ~a" (json-object->string json))
        (set! incoming.json json)
        #t))
    (catch (e)
      (debugf "Exception raised in read-msg!: ~a" e)
      (internal-error e))))


(def (write-msg! (outgoing : LspReqResp))
  (using ((obuf (Transport-writer outgoing.transport) :- BufferedWriter))
    (let ((out (json-object->bytes outgoing.json)))
      (debugf "=== SEND ~a" (bytes->string out))
      ;; Headers
      (obuf.write (string->bytes "Content-Length: "))
      (obuf.write (string->bytes (number->string (u8vector-length out))))
      (write-crlf obuf)
      (write-crlf obuf)
      ;; Body
      (obuf.write out)
      (obuf.flush))))
