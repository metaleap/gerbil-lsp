From the LSP vantage, functionality that would be desirable in the `std/ide` lib, a summarized list.

### Workspace

Since there'll be a sort of an _"ongoing / long-lived interpreter session on all the currently-opened project folders (aka 'root folders') with their sub-folders and source files"_ running `ide`-side, it should expose funcs to notify it about the following events, such that it can update its internal state about the codebase-in-session:

Necessary:

- **source-files-deleted** with a list of file paths
- **source-files-created** with a list of file paths (might also trigger on file pastes, so it's not necessarily to be assumed they're empty)
- **source-files-renamed** with a list of old-path,new-path pairs
  - if `ide` prefers, this can be omitted, with callers representing "renames" via two consecutive calls: first _deleted_, followed by _created_
  - even if not so omitted, it can be assumed that some clients / editors send file renames that way
- **source-file-changed** (this is not-yet-saved live edits — the full new buffer contents would be passed)
- **root-folders-changed** with a list of newly-added and a list of newly-removed root folders — this would also be used for the initial-list-of-root-dirs shortly after the session starts or when a new "workspace" / project (list of root dirs) is opened in the editor

Optional, if it makes sense for `ide`:

- **source-file-saved** (the full, current file contents could be passed)
- **source-file-opened**
- **source-file-closed**

###
