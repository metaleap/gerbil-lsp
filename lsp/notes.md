From the LSP vantage, functionality that would be desirable in the `std/ide` lib, a summarized list.

# Workspace syncing

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

Optional, if it makes sense for `ide`:

- **source-file-saved** (the full, current file contents could be passed)
- **source-file-opened**
- **source-file-closed**

# Actual Language Intel

## defs-in-file

Inputs:
- a single source file path.

Results:
- a tree hierarchy of symbol `def`s / decls in the file. The root list representing top-level symbol decls, with their local `def`s, `let`s, `using`s etc being descendant / sub-tree symbols.

Not just funcs and vars, but practically also macro calls starting with `def` such as `defstruct`, `defclass` etc.

Mandatory fields:
  - **name**

Desirable fields:
  - **kind**: one of `ide`-defined known-enumerants (function, var, struct, class, macro etc)
  - **deprecated** bool, if there's a "defacto-standard" notation for that
  - **description** for top-level symbols that are directly preceded by a multi-line comment (or block of single-line comments), which is itself preceded by empty line(s)?
  - **signature** for such symbols as can be detected to have one (funcs, macros)

### Extra nice to have:

"Expansion" of custom `defrule`s, for example: although this [defhandler](https://github.com/metaleap/gerbil-lsp/blob/7443360986656e82ff2b3674a19afcd7680bee60/lsp/handling.ss#L24) macro would be listed as a symbol of `handling.ss`, its _uses_ such as [`(defhandler "initialize")`](https://github.com/metaleap/gerbil-lsp/blob/7443360986656e82ff2b3674a19afcd7680bee60/lsp/lsp-lifecycle.ss#L25) in other (or not) source files would then be listed as symbols in _those_ source files
