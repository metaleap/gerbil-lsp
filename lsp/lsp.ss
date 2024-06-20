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



; continuations waiting for responses
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



;; The serving loop: processes requests through `handle-request!`.
(def (lsp-serve (transport : Transport))
  (debugf "=== lsp-serve")
  (def json-outgoing #f)
  (def json-incoming #f)
  (def done #f)
  (while (not done)
    (set! json-outgoing #f)
    (set! json-incoming (read-msg! transport))
    (let ((msg-id (hash-get json-incoming "id"))
          (msg-method (hash-get json-incoming "method")))
      (if (json-rpc-error? json-incoming)
        ; received an error message
        (using (err json-incoming :- JSON-RPCError)
          (set! json-outgoing err #;(trivial-class->json-object err)))
        ; else, received a normal message
        (if msg-method
          ; msg is an incoming request or notification
          (set! json-outgoing (serve-json-rpc lsp-handler json-incoming))
          ; else, msg is an incoming response
          (let (handler (hash-get +pending-reqs+ msg-id))
            (when handler
              (hash-remove! +pending-reqs+ msg-id)
              (try
                (handler (hash-get json-incoming "result"))
                (catch (e)
                  (debugf "response handler failed on response '~a': ~a"
                            (json-object->string json-incoming) e))))))))

    (try ; if any writes throw, we are irreparably disconnected
      ; only respond to Requests, but not Notifications or Responses
      (when (and json-outgoing (not (void? json-outgoing)) (hash-get json-incoming "id"))
        (write-msg! transport json-outgoing))
      ; send out requests that have piled up, if any
      (hash-for-each (lambda (req-id req-and-handler)
          (hash-put! +pending-reqs+ req-id (cdr req-and-handler))
          (write-msg! transport (car req-and-handler))
        ) +new-reqs+)
      (hash-clear! +new-reqs+)
      (catch (e)
        (errorf "=== connection lost, exiting: ~a" e)
        (set! done #t)))))


(def (read-msg! (transport : Transport))
  (try
    (using ((ibuf (Transport-reader transport) :- BufferedReader))
      (begin
        (def headers (read-request-headers ibuf))
        (def json-obj (parameterize ((json-symbolic-keys #f))
                        (bytes->json-object (read-request-body ibuf headers))))
        (debugf "=== RECV ~a" (json-object->string json-obj))
        json-obj))
    (catch (e)
      (debugf "Exception raised in read-msg!: ~a" e)
      (internal-error e))))


(def (write-msg! (transport : Transport) json-obj)
  (def CR (char->integer #\return))
  (def LF (char->integer #\linefeed))
  (using ((obuf (Transport-writer transport) :- BufferedWriter))
    (let ((out (json-object->bytes json-obj)))
      (debugf "=== SEND ~a" (bytes->string out))
      ;; Headers
      (obuf.write (string->bytes "Content-Length: "))
      (obuf.write (string->bytes (number->string (u8vector-length out))))
      (obuf.write-u8-inline CR)
      (obuf.write-u8-inline LF)
      (obuf.write-u8-inline CR)
      (obuf.write-u8-inline LF)
      ;; Body
      (obuf.write out)
      (obuf.flush))))
