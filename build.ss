#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("lsp/std_ide_lsp/gerbil/gxlsp" "lsp/std_ide_lsp/server" "lsp/std_ide_lsp/codegen"
    (exe: "lsp/main" bin: "lsp")))
