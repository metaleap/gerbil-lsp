;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceFeatures

(export on-workspace-folders-changed workspace-folders)

(import :std/sugar
        ../handling
        ./types)


(def workspace-folders [])


(def (on-workspace-folders-changed added removed)
  (def (not-removed old-folder) (not (member old-folder removed)))
  (let (sans-removed (if (null? removed) workspace-folders (filter not-removed workspace-folders)))
    (let (not-already (lambda (new-folder) (not (member new-folder sans-removed))))
      (set! workspace-folders (append sans-removed (filter not-already added))))))
