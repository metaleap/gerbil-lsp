#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("lsp/lsp" "lsp/handling" "lsp/interfaces"
    "lsp/msgs/types-incoming" "lsp/msgs/types-outgoing" "lsp/msgs/outgoing.ss"
    "lsp/msgs/lifecycle" "lsp/msgs/lang-intel" "lsp/msgs/workspace-sync"
    ; (exe: "lsp/gxlsp" bin: "gxlsp"))
    "lsp/gerbil/common" "lsp/gerbil/lang-intel" "lsp/gerbil/workspace-sync"
     "lsp/gxlsp")
  optimize: #f parallelize: 4)
