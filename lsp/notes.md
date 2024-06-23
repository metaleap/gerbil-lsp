List of feature suggestions that'll be most-desirable for `std/ide` to expose (to consumers such as `lsp`, and others).

**Namings** used in here are just provisional / placeholder idents, not me prescribing how exports from `ide` should be called.  =)

**Paths:** all paths consumed or produced `lsp`-side are absolute.

**Language intel quick jumps:**
- [_Essentials_](#2-language-intel-the-essentials), in "presumed dev-dependency order":
  - [defs-in-file](#defs-in-file), [defs-search](#defs-search), [lookup](#lookup), [occurrences](#occurrences), [completions](#completions), [info-tips](#info-tips), [can-rename](#can-rename), [rename](#rename), [on-file-notices-changed (diagnostics)](#on-file-notices-changed), [signatures](#signatures)
- [_Bonus_](#3-language-intel-bonus--icing-on-the-cake):
  - [ast-parents](#ast-parents), [callers](#callers), [callees](#callees), [super-types], [sub-types]

# 1. "Workspace" / source-file tracking & sync

**No file-watchings in `ide` or `lsp`!**

Since `ide` will have running some kind of _"ongoing / long-lived interpreter(ish) session"_, `ide` shall expose funcs for its consumers (incl. `lsp`) to notify it about the following events, so that it can on-the-fly update its internal live representations about the codebase-in-session, (re)analyze changed / new files (or their importers, or still-existing importers of just-removed files!) etc:

Necessary:

- **on-source-file-changes** with 3 lists of Scheme source file paths, to be processed in this order:
  - _removed_ &mdash; source file deletions, or source file removals from the editor-side "workspace" / project
  - _added_ &mdash; source file creations (they're not necessarily empty), or source file additions to the editor-side "workspace" / project
  - _changed_ &mdash; on-disk source file modifications, whether through Save or from outside the editor
- **on-source-file-edited**: this is not-yet-saved live edits — the source file path and full current editor-side buffer contents will be passed

Optional, **if** it is of any practical interest to `ide` (for example to "prioritize" analyses / refreshes of opened files vs. all the others or some such)

- **on-source-file-opened**
- **on-source-file-closed**

(All file and folder _rename_ events result in `on-source-file-changes` calls with corresponding _removed_ / _added_ combinations, because VSCode alone showed that LSP clients cannot be relied upon to furnish sufficiently-robust rename watching and reporting.)

**All of the above means that _neither_ `lsp` _nor_ `ide` has to implement and maintain a file-watcher!** Such a responsibility, complexity and resource mgmt should be outside of both (imho) and hence "client-side", whether that's LSP-speaking text editors (they do that already) or any other `ide` consumers.

# 2. Language intel: the essentials

**These features are "sorted in presumed order of dev dependency"** such that work on later ones will most-likely _substantially_ benefit from / build upon / reuse / leverage work already done for earlier ones.

**Important:** most of these will receive and/or return _positions_ (line/col pair) and/or _"ranges"_ (pair of start _position_ and end _position_).
  - Handling those (in sync with perhaps underlying byte-buffer indices that AST nodes might refer to on the `ide` side — dunno) requires taking into account which EOL markers are used in the source file (`\r\n` or `\n` or `\r`), as well as the file's text encoding.
  - The `lsp` side receives from (and sends to) its client (editor), [as per protocol mandate](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#positionEncodingKind), all positions / ranges in the "PositionEncodingKind" UTF-16. If translations between that and `ide` need doing, we need to align on this — or alternatively, `ide` could adopt this "utf-16 position encoding standard" itself and mandate it to all `ide` users, which means `lsp` can pass positions/ranges right through between the editor side and the `ide`-lib side. Something to discuss and decide!

## _`defs-in-file`_

[Demo scenario 1](https://user-images.githubusercontent.com/12821956/57658641-7fb9bf80-7594-11e9-8bdc-b86118acdf48.png) · [Demo scenario 2](https://code.visualstudio.com/assets/docs/editor/editingevolved/breadcrumb-symbol-dropdown.png)

Args:
- a single Gerbil Scheme source file path
- a bool whether full tree hierarchy is wanted or just flat list of top-level-defs

Results:
- a tree hierarchy of symbol `def`s / decls / symbol names occurring in the file. The root list representing top-level symbol decls, with their own subsequent local `def`s, `let`s, `using`s etc being descendant / sub-tree symbol defs (only computed and populated if the abovementioned bool arg wants it).

Not just funcs and vars, but practically also all macro calls starting with `def` such as `defstruct`, `defclass`, `interface` etc.

Mandatory fields per list item:
  - **name**
  - **children** — to make the hierarchy tree happen, zero or more descendant symbol defs, ie. locals (each same struct as this parent)

Desirable fields, as feasible / applicable:
  - **kind**: one of `ide`-defined known-enumerants (eg. `'function`, `'var`, `'struct`, `'class`, `'iface`, `'macro`, `'other` etc)
    - some of [these](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#symbolKind) or [these](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind) might be adopted here where it makes sense (that's `ide`'s call though)
  - **deprecated** bool (if there's a "defacto standard" notation for that in Gerbil)
  - **detail**: `#f` or `""` or could be a non-markdown, plain text of extra information, for example:
    - signature or type information (annotation, inference) etc
    - failing both: for locals, next-higher ancestor name (def or let or using)
    - or whatever else that's "good to know" and pertinent to and available for the def/decl
  - **range-full**: start and end position of the _whole form_ of the symbol def/decl, ie. from the opening `(` up-to-and-including the closing `)`
  - **range-name**: start and end position of the identifier only (ie the `foo` in `(def foo 123)`)

### Macro-related subleties:

- Most desirable, perhaps indispensible, in Scheme land: "expansion" (to symbol-defs to yield here) of custom `defrule`s or custom macros starting with the `def`-prefix, for example: although [this defrule named 'defhandler'](https://github.com/metaleap/gerbil-lsp/blob/7443360986656e82ff2b3674a19afcd7680bee60/lsp/handling.ss#L24) would be listed as a symbol of `handling.ss`, its _callers_ such as eg. [`(defhandler "initialize")`](https://github.com/metaleap/gerbil-lsp/blob/7443360986656e82ff2b3674a19afcd7680bee60/lsp/lsp-lifecycle.ss#L25) in other (or not) source files would then be listed as defs in _those_ source files
- More generalized, many macros are designed to introduce caller-specified identifiers into their bodies' scopes that need to be captured in the tree hierarchy, ie. whether `let`s reduce to `lambda`s or not, even some `my-custom-let` doing so introduces a _macro-caller-specified_ identifier to its body and so it should show up in the returned symbol hierarchy.
- At the same time, _some_ macros follow a fashion of also introducing into their bodies scopes their own well-known (not caller-specified) magic identifiers (`it` as a commonly-known example) &mdash; while we **would** want those to be listed in [completions](#completions), we would **not** want them in `defs-in-file` (or `defs-search`).

## _`defs-search`_

[Demo scenario](https://user-images.githubusercontent.com/8097890/185075014-e113ed17-1849-4a8c-ba5d-a6bbc3431cdf.png)

Args:
- a "query": usually the substring of a (or the full) def name

Results:
- a hashtable / alist where for each entry:
  - the _key_ is the path of a tracked Gerbil source file existing _somewhere_ in the currently-opened "root folders" that the _value_ applies to
  - the _value_ is a flat list (no tree hierarchy) of all the (top-level-only) matching defs/decls (result struct type **just like above** in `defs-in-file`, but with `children` neither populated nor even computed) found in that source file

## _`lookup`_

[Demo scenario](https://user-images.githubusercontent.com/11874914/28750773-d4d38782-74f4-11e7-98a0-a6d4b041e59c.png)

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above)
- a "lookup kind": one of `ide`-defined known-enumerants:
  - at least:
    - `'def` (go to definition)
      - whether `def` or `let` or `using` or `my-macro` introducing the identifier at position, whether in current file or cross-file import or package dep import or `std/*` / `gerbil/*` import etc &mdash; gotta find and yield the location  =)
    - `'refs` (list references)
      - whether we're positioned right _on_ an identifier decl (def / let / using / other), or merely on a ref _to_ one
  - optionally, if exciting-and-feasible:
    - `'type-def` (location of the defstruct/defclass/iface of a type-annotated / type-inferred ident)
    - `'iface-impl`
      - if on interface def-or-ref or interface method def-or-ref: known implicit implementations of that interface or method
      - if on class def-or-ref or class method def-or-ref: interfaces or interface methods implicitly implemented by that
    - any others later on if & as they come to mind in the community
      - but note, [callers](#callers) and [callees](#callees) are already in here and separate from `lookup`, as they return hierarchies rather than a flat list

Results:
- a list of zero more "locations" (pairs of source file path and _range_)

Results' source file paths _may_ include files other than the `on-source-file-changes`-introduced (ie. editor-side) ones, depending on the given lookup kind:
- for `'def`, _should_ include any find(s) eg. in `std/*`, `gerbil/*` etc (paths located presumably usually somewhere in `/opt/gerbil/src`)
- **but** not so for `'refs`: even if the def-referred-to-at-position is in `std/*`, `gerbil/*` etc., we'll only want such refs to it as are located in the "editor-side source files" (`on-source-file-changes`-introduced ones), so as to not also have to stare at a listing of hundreds of refs from inside `std/*`, `gerbil/*` etc.
  - to illustrate: when I look up references to `string-append`, I want to see "my currently-opened projects' uses" of it only, not other ones that `ide` somehow may also happen to know about

## _`occurrences`_

[Demo scenario](http://anon.to/?https://i.sstatic.net/3gNYO.png)

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above)

Results:
- a list of zero or more location _ranges_ (ie. start-end-pair) in the current file

Usually used by editors to highlight all occurrences of the current ident (whether we're on a def or on a ref) &mdash; internally (`ide`-side) this might perhaps best be done as a specialized "list references" call (see [lookup](#lookup), ie. `(lookup path pos 'refs)`) to look up refs _only_ in the current file but not the rest of the codebase.

(Other non-Lispy language servers use it also for such situations as highlighting the func or loop of the current `break` / `continue` / `return` but that does not seem applicable to Scheme.)

## _`completions`_

[Demo scenario](https://code.visualstudio.com/assets/docs/nodejs/working-with-javascript/lodash-typings.png)

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above) at which auto-completion proposals will pop up

Results:
- empty list if in a num / char / string literal (but not quoted-symbol "literal" =)
- a list of symbol-info structures like also returned above in `defs-in-file` and `defs-search`, with these extra considerations:
  - **name**: the full name, not partial (ie if position is right after `ha` then `name` is the _full_ `hash-ref`, `hash-copy` etc and _not_ `sh-ref`, `sh-copy` etc)
  - **children**: not populated (or even computed)
  - **detail** as described but further augmented with the `import` where pertinent
  - **description** as an additional field:
    - `#f` or `""` or
    - existing markdown doc or
    - for non-documented top-level defs their preceding multi-line comment or block of single-line comments, if any (stripped of comment delimiters)

**Only what's in scope at position:**
- any top-level def/decl in the current file
- ancestor locals in scope
- any made available by the file's existing `import`s
- generally macro-introduced identifiers in scope, see [considerations in `defs-search`](#macro-related-subleties)
- bonus stretch goals:
  - any from any not-yet-imported `std/*` / `gerbil/*` / pkg deps etc or not-yet-imported project/package-local source files, together with an additional "import edit" struct to be applied in-editor to the current source to import that &mdash; such a text-edit being simply an (insert-text,insert-position) pair
- plus any `'quoted-symbol` already occurring somewhere in this source file (since one is often slinging them around repeatedly / reusingly, at least in the use-case of enumerants / tags)

**Further filtering apart from in-scope-ness:**
- none if a non-identifier-compatible char (braces parens whitespace etc) is just before current-position
- else, filtered by the identifier-compatible text fragment directly before the current-position
  - prefix matches first, substring matches afterwards (they matter, too, as auto-complete is often a search attempt)

**On "dot completions":** imho, since this sugar is pertinent only in certain scopes such as `using` or `{...}` and only one level deep AFAICT:
- all the valid "dot completions" (field or method names, ie right-hand-side operands) should be already "statically" known for any given left-hand-side operand
- hence these can be prepared as simple _full_-identifiers (ie. `mystruct.myfield` is proposed as its own full completion-item entry right next to `mystruct`, same for methods), ie. "there _is_ no 'dot-completion' (special handling of dots)"

## _`info-tips`_

[Demo scenario](https://user-images.githubusercontent.com/33861896/91145912-78012a00-e6b6-11ea-9e98-3dd8d04e89c6.png)

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above)

Results:
- a list of contextual information bites, structured three-fielded like so:
  - **name** — one of `ide`-defined well-known enumerants such as eg. `description`, `signature`, `type`, `name`, etc.
  - **format** — one of `ide`-defined well-known enumerants such as eg. `plaintext`, `markdown`, `scheme`
  - **value** — the actual info-bite as a string
- the _range_ (start-and-end-pos in the source file) of the actual form / AST node that the specified current position is on

Result info-bit **ideas** to represent and return:
- If on a symbol ref or def:
  - `description`, format `markdown`: as per the `description` described above in `completions`
  - `signature`, format `scheme`: if known callable
  - `type`, format `scheme`: if known (annotated or inferred) for that def or ref'd def
  - `expansion`, format `scheme`, if identifier is a macro ref
    - that's reader-intuitive "immediately-next" expansion of the whole macro call — not the likely-illegible "final full expansion" say into IR or nothing-but-lambdas =)
  - `import`, format `plaintext` or `scheme`: **whenever** identifier defined outside the current source file
  - any other meta-data / info-bites that are potentially truly handy-to-discover in an info-tip hover UX
    - but excluding stuff obtainable via [lookup](#lookup) or [occurrences](#occurrences) calls
    - also excluding contextual "hints and tips" or code warnings / lints: covered by [diagnostics](#on-file-notices-changed)
- If on a string literal
  - `byte-length` (format: `plaintext`)
  - `utf8-rune-length` (format: `plaintext`)
- If on a fixnum or char literal
  - `hex` (format: `scheme`)
  - `octal` (format: `scheme`)
  - `decimal` (format: `scheme`)

Any other ideas? Just bring them into `ide`'s `info-tips` and let `lsp` and other well-known users know about and adopt them.

## _`can-rename`_

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above)

Results:
- `#f` if position is not on an identifier to begin with
- `#f` if identifier is not a def, or ref to a def, that is located in one of the `on-source-file-changes`-introduced source files
- else: the _range_ of the identifier at the position

## _`rename`_

[Demo scenario](https://code.visualstudio.com/assets/docs/editor/refactoring/rename.png)

Project-wide def rename across all `on-source-file-changes`-introduced tracked source files.

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above)
- the new name (throw if syntactically invalid ident)

Results:
- `#f` or `(void)` if new name is identical to old name (clients will likely check usually, but never trust clients!) or if the above `can-rename` would `#f` for these args
- else, a hash-table or alist where for each entry:
  - the _key_ is the source file path that the _value_ applies to
  - the _value_ is a list of _ranges_ representing _those_ occurrences of the old name in that file that _are_ references to the def-being renamed (don't want to rename shadowings etc)

## _`on-file-notices-changed`_

[Demo scenario 1](https://code.visualstudio.com/assets/docs/editor/editingevolved/errors.png) · [Demo scenario 2](https://code.visualstudio.com/assets/docs/editor/editingevolved/errorsinline.png)

This is the sole API surface to handle any and all warnings, errors, hints, lints, etc.

Args:
- a function (detailed below) passed by the caller that is to be invoked (by `ide`'s ongoing long-running background "interpreter(ish) whole-codebase session") whenever tracked source files (but only the `on-source-file-changes`-introduced ones) are re-parsed / re-compiled / re-interpreted(-sans-effectful-top-level-blocks-presumably) / **re-analyzed**.

Results:
- `(void)`

The callback func passed in by the caller would receive:
- a source file path
- a list of zero-or-more "notices" about that file, each such notice entry being a structure with:
  - the _range_ in the source file that the notice applies to
  - an `ide`-defined severity/category enumeration such as eg. `'err`, `'warn`, `'hint`, `'info` (as applicable)
  - the (error / warning / info / etc) message itself
  - error / warning / etc code (number or ident) — if there's such a thing in Gerbil
  - tags: optional list of such categorization tags as commonly induce special UI renditions other than squigglies, such as `'deprecated` (may induce strike-thru font-style rendition) or `'unused` (may induce faded text-color rendition)

Of note, the list-of-notices for a file may be empty upon its re-analysis when previously it wasn't — or vice versa — either way, any re-analysis should invoke the callback with the now-current list-of-notices if changed at all (and ideally, only then) &mdash; whether it's empty or not.

**A challenging necessity:** any change in any one source file can always potentially affect its direct importers (_and_ importers of _those_ too) &mdash; so all such direct or indirect dependent source files should hence also be re-analyzed and have their own list-of-notices re-announced (if changed) over the callback arg.

## _`signatures`_

[Demo scenario](https://user-images.githubusercontent.com/1078012/43570032-3b1b4a30-9631-11e8-962b-73bb491ccaf2.png)

Args:
- the current source file path
- the current _position_ (see note at intro of part 2. above, as usual)

Results:
- a list of zero-or-more signature-info structs containing:
  - **signature**: the syntactical form clarifying the signature, ie. `(name arg arg)` or `(name arg . rest)` etc.
    - This should not be a direct source extract of the func/macro def's name-and-args (might include commented-forms, might be multi-line etc) but instead pieced together readably a-fresh from the relevant AST
  - **description**: markdown, same as first described above in `completions`

Call forms might not only refer to resolved func-valued defs but also macros and native/primitive/special forms. Would be neat to have 'em all! But func calls of course the most important.

**Should (imho) return empty list whenever** the form _at current position_ is not a call, even if the parent or any ancestor forms are — because in Lisp/Scheme, they all are. So that pressing eg. space-key deep inside some vector / list / pair literals hierarchy does not continually re-popup some signature tooltip of a way-outer call form.

# 3. Language intel: bonus / icing on the cake

## _`ast-parents`_

[Demo scenario](https://code.visualstudio.com/assets/docs/editor/codebasics/expandselection.gif)

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

[Demo scenario](http://anon.to/?https://i.sstatic.net/AxfS2.gif)

...todo

## _`callees`_

[Demo scenario](http://anon.to/?https://i.sstatic.net/HKU3h.png)

...todo
