;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceFeatures

(export on-workspace-folders-changed)

(import :std/sugar
        ../handling
        ./types)


(def workspace-folders [])


(def (on-workspace-folders-changed added removed)
  (let (sans-removed (filter (lambda (old) #t) workspace-folders))
  (set! workspace-folders (append sans-removed added))))
