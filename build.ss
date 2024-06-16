#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("lsp/lsp" "lsp/handling" "lsp/lsp-lifecycle"
    (exe: "lsp/gxlsp" bin: "gxlsp")))
