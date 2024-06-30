#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("lsp/serving" "lsp/handling" "lsp/interfaces"
    "lsp/msgs/types-incoming" "lsp/msgs/types-outgoing" "lsp/msgs/outgoing" "lsp/msgs/incoming"

    "lsp/gerbil/common" "lsp/gerbil/lang-intel" "lsp/gerbil/workspace-sync"
    ; (exe: "lsp/gxlsp" bin: "gxlsp"))
     "lsp/gxlsp")
  optimize: #f parallelize: 4)
