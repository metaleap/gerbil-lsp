From the LSP vantage, functionality that'll be desirable for `std/ide` to expose.

All namings used here are provisional / placeholder identifiers, not me prescribing how exports from `ide` should be called.  =)

# 1. Workspace syncing

Since there'll be a sort of an _"ongoing / long-lived interpreter session on all the currently-opened project folders (aka 'root folders') with their sub-folders and source files"_ running `ide`-side, it should expose funcs to notify it about the following events, so that it can on-the-fly update its internal representations about the codebase-in-session:

Necessary:

- **source-files-deleted** with a list of Scheme source file paths
- **source-files-created** with a list of Scheme source file paths (it's not necessarily to be assumed they're empty, might be on file paste or on the buffer's first Save)
- **source-files-renamed** with a list of old-path,new-path pairs (_both_ in each pair are guaranteed by the caller to have Scheme file extensions)
- **source-file-edited**: this is not-yet-saved live edits — the full new buffer contents would be passed (unless for some reason `ide` would prefer list-of-atomic-edit-steps-applied? Under LSP would be just as easily doable, so `ide`'s choice.)
- **source-file-changed** for on-disk source file content changes, whether through Save or from _outside the editor with the file not being opened inside it_ (because `ide` will also want to reload _such_ file changes into the session)
- **root-folders-changed** with a list of newly-added and a list of newly-removed root folders — this would _also_ be used for the initial-list-of-root-dirs shortly after the session starts, and whenever a new "workspace" / project (list of root dirs) is opened in the editor

Optional, if it makes sense for (or is of interest to) `ide`:

- **source-file-opened**
- **source-file-closed**

**All of the above means that neither `lsp` nor `ide` needs to implement and maintain a file-watcher!** Such a responsibility, complexity and resource mgmt should be outside of both (imho), whether that's LSP clients / editors (they do that already) or any other `ide` consumers.

# 2. Actual Language Intel

**These features are "sorted in order of dev dependency"** such that work on later ones will likely (best-guess basis) _substantially_ benefit from / build upon / reuse / leverage work already done for earlier ones.

**Important:** most of these will receive and/or return _positions_ (line/col pair) and/or _"ranges"_ (pair of start _position_ and end _position_).
  - Handling those (in sync with perhaps underlying byte-buffer indices that AST nodes might refer to on the `ide` side — dunno) may need to take into account which EOL markers are used in the source file (`\r\n` or `\n` or `\r`), as well as the file's text encoding.
  - The `lsp` side receives from (and sends to) its client (editor), [as per protocol mandate](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#position), all positions / ranges in the "PositionEncodingKind" UTF-16. If translations between that and `ide` need doing, we need to align on this — or alternatively, `ide` could adopt this "utf-16 position encoding standard" itself and mandate it to all `ide` users, which means `lsp` can pass positions/ranges right through between the editor side and the `ide`-lib side. Something to discuss and decide!

## _`defs-in-file`_

Args:
- a single Gerbil Scheme source file path
- a bool whether full tree hierarchy is wanted or just flat list of top-level-defs

Results:
- a tree hierarchy of symbol `def`s / decls occurring in the file. The root list representing top-level symbol decls, with their own subsequent local `def`s, `let`s, `using`s etc being descendant / sub-tree symbols (only computed and populated if the abovementioned bool arg wants it).

Not just funcs and vars, but practically also all macro calls starting with `def` such as `defstruct`, `defclass`, interface etc.

Mandatory fields:
  - **name**
  - **children** — to make the hierarchy tree happen, zero or more direct descendant symbols (each same struct as the parent)

Desirable fields:
  - **kind**: one of `ide`-defined known-enumerants (eg. `'function`, `'var`, `'struct`, `'class`, `'iface`, `'macro` etc)
    - some of [these](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#symbolKind) or [these](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind) might be adopted where it makes sense (that's `ide`'s call though)
  - **deprecated** bool, if there's a "defacto-standard" notation for that
  - **description**: markdown doc or, for non-documented top-level defs their preceding multi-line comment or block of single-line comments
  - **detail**: could be, for example:
    - signature or type annotation (note: put syntax and idents inside markdown inline-code tag (`` ` ``) or Scheme-syntax tag (`` ```scheme ``), such that eg. `*myglobal*`  does not falsely render as an italic, de-asterisk'd "_myglobal_")
    - or whatever else that's "good to know" and pertinent to and available for the def/decl
  - **range-full**: start and end position of the _whole form_ of the symbol def/decl, ie. from the opening `(` up-to-and-including the closing `)`
  - **range-name**: start and end position of the identifier only (ie the `foo` in `(def foo 123)`)

**Extra nice to have:**

"Expansion" of custom `defrule`s, for example: although this [defhandler](https://github.com/metaleap/gerbil-lsp/blob/7443360986656e82ff2b3674a19afcd7680bee60/lsp/handling.ss#L24) macro would be listed as a symbol of `handling.ss`, its _uses_ such as [`(defhandler "initialize")`](https://github.com/metaleap/gerbil-lsp/blob/7443360986656e82ff2b3674a19afcd7680bee60/lsp/lsp-lifecycle.ss#L25) in other (or not) source files would then be listed as symbols in _those_ source files

## _`defs-search`_

Args:
- a "query" (usually incoming as substring of, or full, symbol identifier)

Results:
- a hashtable / alist where:
  - the _value_ is a flat list (no tree hierarchy) of the (top-level-only) matching defs/decls (result struct type **just like above** in `defs-in-file`, but with `children` neither populated nor even computed) found in a tracked Gerbil source file existing _somewhere_ in the currently-opened "root folders"
  - the _key_ is the path of that source file

## _`completions`_

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above) at which auto-completion proposals will pop up

Results:
- a list of symbol items like returned also above in `defs-in-file` and `defs-search`, with these extra considerations:
  - **name**: the full name, not partial (ie if position is right after `ha` then `name` is the _full_ `hash-ref`, `hash-copy` etc and _not_ `sh-ref`, `sh-copy` etc)
  - **children**: not populated (or even computed)
  - **detail** to be augmented with the `import` where pertinent

**Everything that's in scope:**
- any top-level def/decl in the current file
- ancestor locals in scope
- any made available by the file's existing `import`s
- bonus stretch: any from any not-yet-imported `std/*` with an additional "import edit" to apply in-editor to the source (a text-edit being an insert-text,insert-position pair)
- might also want to include any `'quoted-ident` already occurring somewhere in this source file (since one is often slinging them around repeatedly)
  - of course, like all other completions, only if suitable in terms of the current typing context (text to the left of position)

**On "dot completions":** since this is pertinent only in certain scopes such as `using` or `{...}` and only one level deep AFAICT:
- all the valid "dot completions" (field or method names, ie right-hand-side operands) should be already "statically" known for any given left-hand-side operand
- hence these can be prepared as simple _full_-identifiers (ie. `mystruct.myfield` is proposed as its own completion-item right after `mystruct`), ie. "there _is_ no dot-completion"

## _`lookup`_

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above)
- a "lookup kind": one of `ide`-defined known-enumerants:
  - at least:
    - `'def` (go to definition) — be sure to catch locals or file-level defs shadowing imported ones
    - `'refs` (list references) — whether we're _on_ a def, or on a ref _to_ some def
  - optionally, if exciting-and-feasible:
    - `'type-def` (location of the defstruct/defclass/iface of a type-annotated ident)
    - `'iface-impl`
      - if on interface or interface method: known implicit implementations of that
      - if on class or class method: interfaces or interface methods implicitly implemented by that
    - any others later on if & as they come to mind in the community

Results:
- a list of zero more "locations" (pairs of source file path and _range_)

## _`occurrences`_

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above)

Results:
- a list of zero more location _ranges_ (ie. start-end-pair) in the current file

Usually used by editors to highlight all occurrences of the current ident (whether we're on a def or on a ref), this will perhaps be a specialized list-references (see above, eg. `lookup path pos 'refs`) to look up refs _only_ in the current file.

Other non-Lispy languages use it also for such situations as highlighting the func or loop of the current `break` / `continue` / `return` but does not seem applicable to us. (But _if_ other, non-occurrence "highlight ideas" should ever come up, would want to rename this to eg. `highlights`. =)

## _`doc-tips`_

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above)

Results:
- optionally, if easy to do, the _range_ (start-and-end-pos in the source file) of the actual form / symbol that the doc-tips apply to
- a list of markdown or plain-text info strings, which may for example surface:
  - if a symbol: the `description` as described above in `defs-in-file` / `defs-search` / `completions`
  - if a symbol: the `detail` as described above in `defs-in-file` / `defs-search` / `completions`
  - if a string literal: the byte length and rune length (can be handy)
  - if a fixnum or char literal: the value in the base of decimal, octal, hex
  - any other infos already lying around, human-language phrased
