# The `lsp` package

can be used to implement an LSP language server.

Gerbil's own LSP language server implementation documents how to do so:
- `lsp/gerbil` [implements](gerbil) all the LSP message handlings needed for Gerbil's own LSP language serving,
- and the `gxlsp` [executable](gxlsp.ss) initializes an `lsp` server with the implementation from `lsp/gerbil`.

# LSP version basis: 3.17

The full LSP 3.17 feature set is **not** implemented: so far, only the LSP 3.17
features required by `lsp/gerbil` are exposed and supported by `lsp`.

Should further LSP features be needed, the `lsp` package needs to be extended accordingly, with such
features to be brought in to `interfaces.ss` and `msgs/*.ss` in the same manner as the existing ones.
