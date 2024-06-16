;;; -*- Gerbil -*-
;;; © 2024 belmarca, metaleap
;;; The Gerbil LSP Server

(export main)

(import :std/cli/getopt
        :std/sugar
        :std/logger
        ./lsp)


(deflogger gxlsp)



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
