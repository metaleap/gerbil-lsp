#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("lsp/lsp" "lsp/handling"
    "lsp/msgs/types" "lsp/msgs/lifecycle" "lsp/msgs/lang-intel" "lsp/msgs/workspace"
    "lsp/msgs/all-outgoing-requests.ss"
    (exe: "lsp/gxlsp" bin: "gxlsp"))
  optimize: #f parallelize: 4)
