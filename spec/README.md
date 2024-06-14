This folder contains local copies of `https://raw.githubusercontent.com/microsoft/language-server-protocol/gh-pages/_specifications/lsp/$LSP_VERSION/metaModel/metaModel.json`.

## Updating to a future newer LSP version

Whenever https://microsoft.github.io/language-server-protocol/specifications/specification-current sports as `(Current)` (__not__ `(Upcoming)` or `(Previous)`) a **newer** version than is present in here as an `lsp-$VERSION.metaModel.json` file:

- fetch the appropriate `https://raw.githubusercontent.com/microsoft/language-server-protocol/gh-pages/_specifications/lsp/$LSP_VERSION/metaModel/metaModel.json` and save it in here as `lsp-$VERSION.metaModel.json`
- regenerate Gerbil code by running `(TODO not impl'd yet)`
