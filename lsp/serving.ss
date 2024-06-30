(export lsp-transport-stdio
        lsp-transport-server-socket
        lsp-serve)

(import :std/io
        :std/format
        :std/sugar
        :std/logger
        :std/misc/list
        :std/os/socket
        :std/text/json
        :std/net/json-rpc
        (only-in :std/net/httpd/handler read-request-headers read-request-body)
        ./handling
        ./interfaces
        ; below import _required_ to trigger all its top-level `lsp-handler` registration calls
        ./msgs/incoming)


; continuations waiting for Responses by the LSP client to Requests we sent to it
(def +pending-reqs+ (make-hash-table))



; Multiple transports: currently implemented below are stdio and server-socket
; (the latter single-client and immediately-blockingly-listening-until-accept).
; If users ever really request it, further impls could include pipes or client-socket
; (where LSP client is the listener) or a more refined server-socket.
; (Keep in mind LSP is expressly defined as a single-client-single-server protocol.)
(defstruct Transport
  ( (reader : BufferedReader)
    (writer : BufferedWriter))
  final: #t)

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
(def (lsp-serve impl (transport : Transport))
  (unless (is-LanguageServer? impl)
    (raise (format "~a does not implement LanguageServer")))
  (using (impl :- LanguageServer)
    (set! lsp-impl impl)
    (debugf (format "=== lsp-serve ~a v~a" {impl.server-name} {impl.server-version})))

  (def json-outgoing #f)
  (def json-incoming #f)
  (def done #f)
  (while (not done)
    (set! json-outgoing #f)
    (set! json-incoming (read-msg! transport))
    (let ((msg-id (hash-get json-incoming "id"))
          (msg-method (hash-get json-incoming "method"))
          (msg-err-code (hash-get json-incoming "code")))
      (if (json-rpc-error? json-incoming)
        ; then: received a broken, not-JSON-parseable message
        (using (err json-incoming :- JSON-RPCError)
          (set! json-outgoing err))
        ; else: received a proper JSON message
          (if msg-err-code
            ; then: received an error Response
            (begin
              (hash-remove! +pending-reqs+ msg-id)
              (errorf "=== LSP client sent ERROR Response: ~a" (json-object->string json-incoming)))
            ; else: a normal LSP message
            (if msg-method
              ; then: msg is an incoming Request or Notification
              (set! json-outgoing (serve-json-rpc lsp-handle json-incoming))
              ; else, msg is an incoming Response
              (let (handler (hash-get +pending-reqs+ msg-id))
                (hash-remove! +pending-reqs+ msg-id) ; even if handler==#f, because void-returning reqs use that
                (when handler
                  (try
                    (handler (hash-get json-incoming "result"))
                  (catch (e)
                      (errorf "=== response handler ~a FAILED on Response '~a' with: ~a"
                                msg-id (json-object->string json-incoming) e)))))))))

    (thread-yield!) ; somehow flushes logger prints I'm told
    (try ; if any writes throw, we are irreparably disconnected
      ; only respond to Requests, but not Notifications or Responses
      (when (and json-outgoing (not (void? json-outgoing)) (hash-get json-incoming "id"))
        (write-msg! transport json-outgoing))
      ; send out requests that have piled up, if any
      (hash-for-each (lambda (req-id req-and-handler)
          (using (req (car req-and-handler) :- json-rpc-request)
            (let (handler (cdr req-and-handler))
              (unless (or (not req.id) (void? req.id))
                (hash-put! +pending-reqs+ req-id handler))
              (write-msg! transport req)))
        ) +lsp-new-outgoing-reqs+)
      (hash-clear! +lsp-new-outgoing-reqs+)
      (catch (e)
        (errorf "=== connection lost, exiting: ~a" e)
        (set! done #t))
      (finally ; flush logger prints
        (thread-yield!)))))


(def (read-msg! (transport : Transport))
  (try
    (def msg-headers  (read-request-headers transport.reader))
    (def msg-body     (read-request-body transport.reader msg-headers))
    (debugf "=== RECV ~a" (bytes->string msg-body))
    (parameterize ((json-symbolic-keys #f))
      (bytes->json-object msg-body))
    (catch (e)
      (errorf "=== FAILED to read-msg!: ~a" e)
      (internal-error e))))


(def write-msg!
  (let ((str-content-length (string->bytes "Content-Length: "))
        (str-crlf-crlf (string->bytes "\r\n\r\n")))
    (lambda ((transport : Transport) json-obj)
      (using ((outbuf transport.writer :- BufferedWriter))
        (unless (hash-table? json-obj)
            (set! json-obj (trivial-class->json-object json-obj)))
        ; remove an #f or (void) "id" if present: msg is then a Notification which must _lack_ that JSON field
        (for-each! ["id" 'id id:] (lambda (key)
          (let (val (hash-get json-obj key))
            (when (or (not val) (void? val))
              (hash-remove! json-obj key)))))
        ; now send outgoing bytes
        (let ((outmsg (json-object->bytes json-obj)))
          (debugf "=== SEND ~a" (bytes->string outmsg))
          ;; Headers
          (outbuf.write str-content-length)
          (outbuf.write (string->bytes (number->string (u8vector-length outmsg))))
          (outbuf.write str-crlf-crlf)
          ;; Body
          (outbuf.write outmsg)
          (outbuf.flush))))))
