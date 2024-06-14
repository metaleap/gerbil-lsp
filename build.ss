#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("lsp/lib"
    (exe: "lsp/gxlsp" bin: "gxlsp")))
