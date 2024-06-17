From the LSP vantage, functionality that would be desirable in the `std/ide` lib, a summarized list.

All namings used here are provisional / placeholder identifiers, not me prescribing how exports from `ide` should be called.  =)

# 1. Workspace syncing

Since there'll be a sort of an _"ongoing / long-lived interpreter session on all the currently-opened project folders (aka 'root folders') with their sub-folders and source files"_ running `ide`-side, it should expose funcs to notify it about the following events, such that it can update its internal state about the codebase-in-session:

Necessary:

- **source-files-deleted** with a list of file paths
- **source-files-created** with a list of file paths (might also trigger on file pastes, so it's not necessarily to be assumed they're empty)
- **source-files-renamed** with a list of old-path,new-path pairs
  - if `ide` prefers, this can be omitted, with callers representing "renames" via two consecutive calls: first _deleted_, followed by _created_
  - even if not so omitted, it can be assumed that some clients / editors send file renames that way
  - if file extension changes from `.ss`/`.scm`/etc. to something non-Scheme, that would mean removing it from live interp session context — perhaps another good reason to omit a special _renamed_ altogether
- **source-file-changed** (this is not-yet-saved live edits — the full new buffer contents would be passed)
- **root-folders-changed** with a list of newly-added and a list of newly-removed root folders — this would also be used for the initial-list-of-root-dirs shortly after the session starts or when a new "workspace" / project (list of root dirs) is opened in the editor

Optional, if it makes sense for (or is of interest to) `ide`:

- **source-file-saved** (the full, current file contents could be passed)
- **source-file-opened**
- **source-file-closed**

# 2. Actual Language Intel

These features are roughly ordered such that work on later ones will likely (best-guess basis) benefit from / build on / leverage work already done for earlier ones.

**Important:** most of these will receive and/or return _positions_ (line/col pair) and/or _"ranges"_ (pair of start position and end position).
  - Handling those (vs. perhaps underlying byte-buffer indices that AST nodes refer to on the `ide`) may need to take into account different EOL markers in the source file (`\r\n` or `\n` or `\r`) and/or the file encoding.
  - The `lsp` user of `ide` will receive, [as per protocol mandate](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#position), all positions / ranges in the "PositionEncodingKind" UTF-16. If translations need doing, we need to decide where they're done and what `ide` itself mandates to its callers (if anything).

## defs-in-file

Inputs:
- a single Gerbil Scheme source file path.

Results:
- a tree hierarchy of symbol `def`s / decls occurring in the file. The root list representing top-level symbol decls, with their own subsequent local `def`s, `let`s, `using`s etc being descendant / sub-tree symbols.

Not just funcs and vars, but practically also all macro calls starting with `def` such as `defstruct`, `defclass`, interface etc.

Mandatory fields:
  - **name**
  - **children** — to make the hierarchy tree happen, zero or more direct descendant symbols (each same struct as the parent)

Desirable fields:
  - **kind**: one of `ide`-defined known-enumerants (function, var, struct, class, interface, macro etc)
    - some of [these](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#symbolKind) might be included where it makes sense (module? package? number? well, it's `ide`'s call though)
  - **deprecated** bool, if there's a "defacto-standard" notation for that
  - **description** for top-level symbols that are directly preceded by a multi-line comment (or block of single-line comments), which is itself preceded by empty line(s)?
  - **detail**: can be signature or type annotation or whatever else that's "good to know" and pertinent to and available for the def/decl
  - **range-full**: start and end position of the _whole form_ of the symbol def/decl, ie. from the opening `(` up-to-and-including the closing `)`
  - **range-name**: start and end position of the identifier only (ie the `foo` in `(def foo 123)`)

### Extra nice to have:

"Expansion" of custom `defrule`s, for example: although this [defhandler](https://github.com/metaleap/gerbil-lsp/blob/7443360986656e82ff2b3674a19afcd7680bee60/lsp/handling.ss#L24) macro would be listed as a symbol of `handling.ss`, its _uses_ such as [`(defhandler "initialize")`](https://github.com/metaleap/gerbil-lsp/blob/7443360986656e82ff2b3674a19afcd7680bee60/lsp/lsp-lifecycle.ss#L25) in other (or not) source files would then be listed as symbols in _those_ source files

## defs-search

Inputs:
- a "query" (usually incoming as substring-of or full symbol identifier)

Results:
- a hashtable / alist where:
  - the _value_ is a flat list (no tree hierarchy) of the (top-level-only) matching defs/decls (result struct type just like above in `defs-in-file`, but with `children` empty) found in a tracked Gerbil source file existing somewhere in the currently-opened "root folders"
  - the _key_ is the path of that source file

## completions

Inputs:
- the current source file path
- the current _position_ (see note at intro of part 2. above) at which auto-completion proposals will pop up

Results:
- a flat list of items that include:
  - **name**: the full name, not partial (ie if position is right after `ha` then `name` is still `hash-ref` rather than `sh-ref`)
  - **description**: markdown doc or, for non-documented top-level defs the preceding multi-line comment or block of single-line comments
  - **detail**: can be signature or type annotation or whatever else that's "good to know" and pertinent-to plus available-for the item
  -
