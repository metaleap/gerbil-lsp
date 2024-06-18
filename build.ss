#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("lsp/lsp" "lsp/handling" "lsp/msgs/lifecycle" "lsp/msgs/lang-intel"
    (exe: "lsp/gxlsp" bin: "gxlsp")))
