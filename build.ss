#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("lsp/lsp" "lsp/handling"
    "lsp/msgs/types-incoming" "lsp/msgs/types-outgoing" "lsp/msgs/all-outgoing-messages.ss"
    "lsp/msgs/lifecycle" "lsp/msgs/lang-intel" "lsp/msgs/workspace-sync"
    (exe: "lsp/gxlsp" bin: "gxlsp"))
  optimize: #f parallelize: 4)
