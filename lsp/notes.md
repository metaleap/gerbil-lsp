List of feature suggestions that'll be most-desirable for `std/ide` to expose (to consumers such as `lsp`, and others).

**Namings** used in here are just provisional / placeholder idents, not me prescribing how exports from `ide` should be called.  =)

**Paths:** for simplicity's sake, perhaps all paths consumed or produced `ide`-side should be absolute. Callers of `ide` can then (if they even need to at all) translate in both directions from/to whatever their thing is: project-dir-relative, current-dir-relative etc...

**Language intel quick jumps:**
- [_Essentials_](#2-language-intel-the-essentials), in "presumed dev-dependency order":
  - [defs-in-file](#defs-in-file), [defs-search](#defs-search), [lookup](#lookup), [occurrences](#occurrences), [completions](#completions), [doc-tips](#doc-tips), [can-rename](#can-rename), [rename](#rename), [on-file-issues-changed](#on-file-issues-changed), [signatures](#signatures)
- [_Bonus_](#3-language-intel-bonus--icing-on-the-cake):
  - [ast-parents](#ast-parents), [TODO: callers], [TODO: callees], [TODO: super-types], [TODO: sub-types]

# 1. Workspace syncing

**No file-watchings in `ide` or `lsp`!**

Since there'll be a sort of an _"ongoing / long-lived interpreter(ish) session on all the currently-opened project folders (aka 'root folders') with their sub-folders and source files"_ running `ide`-side, it should expose funcs to notify it about the following events, so that it can on-the-fly update its internal representations about the codebase-in-session, (re)analyse changed / new files etc:

Necessary:

- **on-source-files-deleted** with a list of Scheme source file paths
- **on-source-files-created** with a list of Scheme source file paths (it's not necessarily to be assumed they're empty, might be on file paste or on the buffer's first Save)
- **on-source-files-renamed** with a list of old-path,new-path pairs (_both_ in each pair are guaranteed by the caller to have Scheme file extensions)
- **on-source-file-edited**: this is not-yet-saved live edits — the full new buffer contents would be passed (unless for some reason `ide` would prefer list-of-atomic-edit-steps-applied? Under LSP would be just as easily doable, so `ide`'s choice.)
- **on-source-file-changed** for on-disk source file content changes, whether through Save or from _outside the editor with the file not being opened inside it_ (because `ide` will also want to reload _such_ file changes into the session)
- **on-root-folders-changed** with a list of newly-added and a list of newly-removed root folders — this would _also_ be used for the initial-list-of-root-dirs shortly after the session starts, and whenever a new "workspace" / project (list of root dirs) is opened in the editor

Optional, if it makes sense for (or is of interest to) `ide`:

- **on-source-file-opened**
- **on-source-file-closed**

**All of the above means that _neither_ `lsp` _nor_ `ide` has to implement and maintain a file-watcher!** Such a responsibility, complexity and resource mgmt should be outside of both (imho) and hence "client-side", whether that's LSP-speaking text editors (they do that already) or any other `ide` consumers.

# 2. Language intel: the essentials

**These features are "sorted in order of dev dependency"** such that work on later ones will most-likely _substantially_ benefit from / build upon / reuse / leverage work already done for earlier ones.

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
  - **children** — to make the hierarchy tree happen, zero or more direct-descendant symbol defs aka. locals (each same struct as this parent)

Desirable fields:
  - **kind**: one of `ide`-defined known-enumerants (eg. `'function`, `'var`, `'struct`, `'class`, `'iface`, `'macro` etc)
    - some of [these](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#symbolKind) or [these](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind) might be adopted where it makes sense (that's `ide`'s call though)
  - **deprecated** bool, if there's a "defacto-standard" notation for that
  - **description**: `#f` or `""` or existing markdown doc or, for non-documented top-level defs their preceding multi-line comment or block of single-line comments, if any
  - **detail**: `#f` or `""` or could be, for example:
    - signature or type annotation (note: put syntax and idents inside markdown inline-code tag (`` ` ``) or Scheme-syntax tag (`` ```scheme ``), such that eg. `*myglobal*`  does not falsely render as an italic, de-asterisk'd "_myglobal_")
    - or whatever else that's "good to know" and pertinent to and available for the def/decl
  - **range-full**: start and end position of the _whole form_ of the symbol def/decl, ie. from the opening `(` up-to-and-including the closing `)`
  - **range-name**: start and end position of the identifier only (ie the `foo` in `(def foo 123)`)

**Extra nice to have:**

"Expansion" of custom `defrule`s, for example: although [this 'defhandler' defrule](https://github.com/metaleap/gerbil-lsp/blob/7443360986656e82ff2b3674a19afcd7680bee60/lsp/handling.ss#L24) would be listed as a symbol of `handling.ss`, its _uses_ such as [`(defhandler "initialize")`](https://github.com/metaleap/gerbil-lsp/blob/7443360986656e82ff2b3674a19afcd7680bee60/lsp/lsp-lifecycle.ss#L25) in other (or not) source files would then be listed as symbols in _those_ source files

## _`defs-search`_

Args:
- a "query": usually the substring of a (or the full) def name

Results:
- a hashtable / alist where for each entry:
  - the _key_ is the path of a tracked Gerbil source file existing _somewhere_ in the currently-opened "root folders" that the _value_ applies to
  - the _value_ is a flat list (no tree hierarchy) of all the (top-level-only) matching defs/decls (result struct type **just like above** in `defs-in-file`, but with `children` neither populated nor even computed) found in that source file

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

Results' source file paths _can_ include files outside the currently opened workspace aka. root folders, and _should_ include any finds in `std/*` or `gerbil/*` (presumably often somewhere in `/opt/gerbil/src`).

## _`occurrences`_

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above)

Results:
- a list of zero or more location _ranges_ (ie. start-end-pair) in the current file

Usually used by editors to highlight all occurrences of the current ident (whether we're on a def or on a ref), internally (`ide`-side) this might perhaps be done as a specialized "list references" (see above, ie. `(lookup path pos 'refs)`) to look up refs _only_ in the current file but not the rest of the codebase.

(Other non-Lispy language servers use it also for such situations as highlighting the func or loop of the current `break` / `continue` / `return` but that does not seem applicable to Scheme.)

## _`completions`_

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above) at which auto-completion proposals will pop up

Results:
- a list of symbol-info structures like also returned above in `defs-in-file` and `defs-search`, with these extra considerations:
  - **name**: the full name, not partial (ie if position is right after `ha` then `name` is the _full_ `hash-ref`, `hash-copy` etc and _not_ `sh-ref`, `sh-copy` etc)
  - **children**: not populated (or even computed)
  - **detail** to be augmented with the `import` where pertinent

**Everything that's in scope:**
- any top-level def/decl in the current file
- ancestor locals in scope
- any made available by the file's existing `import`s
- bonus stretch goals:
  - any from any not-yet-imported `std/*` / `gerbil/*` etc or not-yet-imported workspace-local source files with an additional "import edit" to be applied in-editor to the current source (such a text-edit being simply an (insert-text,insert-position) pair)
- might also want to include any `'quoted-symbol` already occurring somewhere in this source file (since one is often slinging them around repeatedly, at least in the use-case of enumerants)
  - of course, like all other completions, only if suitable in terms of the current typing context (text to the left of position)

**On "dot completions":** imho, since this is pertinent only in certain scopes such as `using` or `{...}` and only one level deep AFAICT:
- all the valid "dot completions" (field or method names, ie right-hand-side operands) should be already "statically" known for any given left-hand-side operand
- hence these can be prepared as simple _full_-identifiers (ie. `mystruct.myfield` is proposed as its own full completion-item entry right next to `mystruct`), ie. "there _is_ no 'dot-completion' (special handling on dot)"

## _`doc-tips`_

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above)

Results:
- a list of markdown info strings, which could for example surface:
  - if a symbol: the `description` as described above in `defs-in-file` / `defs-search` / `completions`
  - if a symbol: the `detail` as described above in `defs-in-file` / `defs-search` / `completions`
  - if a string literal: the byte length and rune length (can be handy)
  - if a fixnum or char literal: the value in the base of decimal, octal, hex
  - any other infos / metadata already lying around for free
- the _range_ (start-and-end-pos in the source file) of the actual form / AST node that the above doc-tips apply to

## _`can-rename`_

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above)

Results:
- `#f` if neither a def nor a ref (to a def located somewhere _inside_ the workspace) at the position
- else: the _range_ of the identifier at the position

## _`rename`_

Workspace-wide def rename.

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above)
- the new name (throw if syntactically invalid ident)

Results:
- `#f` or `(void)` if new name is identical to old name (clients will likely check usually, but never trust clients!) or if the above `can-rename` would `#f` for these args
- else, a hash-table or alist where for each entry:
  - the _key_ is the source file path that the _value_ applies to
  - the _value_ is a list of _ranges_ representing _those_ occurrences of the old name in that file that _are_ references to the def-being renamed (don't want to rename shadowings etc)

## _`on-file-issues-changed`_

Args:
- a function (detailed below) passed by the caller that is to be invoked (by `ide`'s ongoing long-running background "interpreter(ish) whole-codebase session") whenever source files in the opened workspace / root folders are re-parsed / re-compiled / re-interpreted(-sans-effectful-top-level-blocks-presumably)

Results:
- `(void)`

The callback func passed in by the caller would receive:
- a source file path
- a list of zero-or-more "issues" with that file, each such issue entry being a structure with:
  - the _range_ in the source file that the issue applies to
  - an `ide`-defined severity/category enumeration such as eg. `'err`, `'warn`, `'hint`, `'info` (as applicable)
  - the (error / warning / info / etc) message itself
  - error / warning / etc code (number or ident) — if there's such a thing in Gerbil
  - tags: optional list of such categorization tags as commonly yield special UI renditions other than squigglies, such as `'deprecated` (yields strike-thru font style) or `'unused` (yields faded text color)

Of note, the list-of-issues for a file may be empty upon its re-analysis when previously it wasn't — or vice versa — either way, any re-analysis should invoke the callback with the now-current list-of-issues, empty or not.

**The challenge:** a change in any one source file may well affect its direct or indirect (!) importers and those, too, should also be re-analyzed and have their list-of-issues re-announced over the callback arg (one such call per file).

## _`signatures`_

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above)

Results:
- a list of zero-or-more signature-info structs containing:
  - **signature**: the syntactical form clarifying the signature, ie. `(name arg arg)` or `(name arg . rest)` etc.
  - **description**: the same as first described above in `defs-in-file`

Call forms might not only be on resolved lambda-valued defs but also macros and native/primitive/special forms. Might be neat to have 'em all! But func calls of course the most important.

**Should (imho) return empty list whenever** the form _at current position_ is not a call, even if ancestor forms are — because in Lisp/Scheme they all are. So that pressing eg. space-key deep inside some vector / list / pair literals hierarchy does not continually re-popup some signature tooltip of a way-outer call form.

# 3. Language intel: bonus / icing on the cake

## _`ast-parents`_

Args:
- the current source file path
- a _position_ (see note at intro of part 2. above)

Results:
- a struct or pair with
    1. a range
    2. a parent (which is itself also such a range/parent struct-or-pair)

  such that the first _range_ is the AST node of the symbol at position, and its "parent" field then contains the _range_ of its parent AST node as well as that one's parent etc.

To clarify by example:
  - if the input position given is the `z` in the top-level form `(def foo (bar baz))`
  - the result would be this hierarchy:

      ```scheme
      ((here-goes-range-of "baz") . ((here-goes-range-of "(bar baz)") . ((here-goes-range-of "(def foo (bar baz))") . #f)))
      ```

      or, in curly / JSONy terms:

      ```js
      {
        "range": hereGoesRangeOf('baz'),
        "parent": {
          "range": hereGoesRangeOf('(bar baz)'),
          "parent": {
            "range": hereGoesRangeOf('(def foo (bar baz))'),
            "parent": null
          }
        }
      }
      ```

## _`callers`_

Args:
-
