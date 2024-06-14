(import :std/error
        :std/sugar
        :std/cli/getopt
        :std/format

        ./std_ide_lsp/server
        ./std_ide_lsp/codegen
        ./std_ide_lsp/gerbil/gxlsp
  )
(export main)
(include "../manifest.ss")


(def lsp-version "3.17")


(def (main	.	args)
  (call-with-getopt lsp-main args
    program: "lsp"
    help: "Experimental-exploratory standalone LSP prototype"
    (command 'codegen help: "regenerate lsp_generated.ss from lsp-*.metaModel.json")
    (command 'serve help: "serve the language-server protocol over stdio JSON-RPC")
    ))

(def (lsp-main cmd opt)
  (case cmd
    ((codegen)
      (lsp-generate-ss (format "spec/lsp-~a.metaModel.json" lsp-version) "lsp/std_ide_lsp/lsp_generated.ss"))
    ((serve)
      (lsp-serve-stdio))
  )
)
