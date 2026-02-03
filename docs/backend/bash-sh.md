
# bash.sh

**Path:** `build/bash.sh` | **Language:** Bash | **Lines:** ~1,250

> Shell helper function library for gwern.net wiki operations

---

## Overview

`bash.sh` is a comprehensive shell helper library intended to be sourced in `~/.bashrc`. It provides utility functions for the day-to-day maintenance and development of gwern.net, spanning file management, PDF manipulation, caching, search, and content migration.

The library reflects a "batteries included" philosophy—rather than requiring separate tools, it bundles everything needed for common wiki operations. Functions handle path normalization between URLs and local files, wrap common operations with sensible defaults, and provide specialized workflows like HTTP→HTTPS migration and annotation tagging.

The design emphasizes interactive use from the command line, with tab-completion support for common arguments (directory paths, tags), colored output for warnings/errors, and integration with external tools like `exiftool`, `pdftk`, and `parallel`.

---

## Public API

### Path Conversion

#### `path2File(args...) -> string`

Converts URLs or wiki paths to absolute file paths under `~/wiki/`. Handles multiple input formats: relative paths, `~/` prefixes, `https://gwern.net/` URLs, and `/doc/` paths.

**Called by:** Most other functions in the library
**Calls:** `sed`, `echo`

#### `file2Path(path) -> string`

Converts a local file path to a website path (inverse of `path2File`).

**Called by:** Internal utilities
**Calls:** `sed`, `echo`

---

### Cache Management

#### `cloudflare-expire(files...)`

Purges specified files from Cloudflare's CDN cache. Converts file paths to URLs and calls the Cloudflare API.

**Called by:** Manual invocation after content updates
**Calls:** `path2File`, `curl`, `jq`

#### `cloudflare-expire-all()`

Expires all Markdown and static files from the CDN cache. Used for major site updates.

**Called by:** Manual invocation
**Calls:** `find`, `parallel`, `cloudflare-expire`

---

### PDF Operations

#### `pdf(files...)`

Opens PDFs in evince viewer, handling path normalization.

#### `pdf-cut(files...)`

Removes the first page of a PDF (spam removal from JSTOR, etc.). Preserves metadata via `exiftool`.

**Alias:** `pdfcut`
**Calls:** `pdftk`, `exiftool`, `crossref`

#### `pdf-cut-last(file)`

Removes the last page of a PDF. Similar to `pdf-cut` but for trailing pages.

**Alias:** `pdfcut-last`
**Calls:** `pdftk`, `exiftool`, `crossref`

#### `pdf-cut-append(file)`

Moves the first page to the end instead of deleting it. Useful for preserving cover pages while changing display order.

**Alias:** `pdfcut-append`
**Calls:** `pdftk`, `exiftool`, `crossref`

#### `pdf-append(target_pdf, input_files...)`

Concatenates multiple files (PDFs, documents, spreadsheets, images) into a single PDF. Converts non-PDF inputs automatically. Preserves metadata from the first file.

**Calls:** `pdftk`, `exiftool`, `libreoffice`, `doc2pdf`, `img2pdf`, `convert`, `crossref`

#### `doc2pdf(input, [output])`

Converts Word/ODT documents to PDF via LibreOffice headless mode.

**Calls:** `soffice`

#### `e(file, exiftool_args...)`

PDF/image metadata editor wrapper. Strips encryption, normalizes PDFs through `pdftk`, and cleans up title formatting.

**Calls:** `exiftool`, `pdftk`

---

### Image Operations

#### `crop(files...)`

Trims whitespace from around JPG/PNG images using ImageMagick.

**Calls:** `convert`, `identify`, `parallel`

#### `crop_one(file)`

Single-file version of crop with size warning for large images.

#### `pad(files...)`

Adds 30px white border around images (for images cropped too tightly).

**Alias:** `pad-white`
**Calls:** `mogrify`

#### `pad-black(files...)`

Adds 30px dark gray border (`#161616`) matching the site's dark mode background.

**Calls:** `mogrify`

#### `crop-pad(files...)` / `crop-pad-black(files...)`

Combined crop and pad operations.

#### `png2JPGQualityCheck(files...)`

Checks if PNGs can be converted to JPG with minimal quality loss. Uses PSNR metric and size reduction threshold.

**Calls:** `convert`, `compare`, `stat`, `bc`

---

### Search Functions

#### `gw(query)`

Primary site-wide search. Searches Markdown files, Haskell sources, JavaScript, CSS, and configs. Falls back to IRC logs if no results.

**Called by:** Manual invocation
**Calls:** `find`, `grep`, `gwl`

#### `gwf(query)`

Searches file names only (not content).

#### `gwn(query)`

Searches newsletter files only.

#### `gwa(query)` / `gwal(query)`

Searches annotations. `gwal` pipes through `less` with highlighting.

**Defined in:** `~/wiki/static/build/gwa`

#### `gwl(query)`

Searches #lesswrong IRC logs.

---

### Content Migration

#### `gwsed(old, new)` (referenced, defined elsewhere)

Site-wide search and replace.

#### `gwhttp(domain)`

HTTP→HTTPS migration helper. Extracts domain and calls `gwsed`.

#### `gwmv(old, new)`

Moves files with full site integration:
- Git-tracks the move
- Syncs to server via rsync
- Updates all references via `gwsed`
- Updates nginx redirects
- Handles PNG→JPG conversion with history preservation

**Called by:** `gwmvdir`, manual invocation
**Calls:** `git mv`, `rsync`, `gwsed`, `stringReplace`, `crossref`

#### `gwmvdir(old, new)`

Moves entire directories by iterating `gwmv` over contents.

**Calls:** `gwmv`

#### `gwtag(url, tags...)`

Adds or modifies annotation tags via the `changeTag` Haskell tool.

**Aliases:** `gwt`, `t`
**Calls:** `changeTag`

---

### Link Archiving

#### `mvuri(encoded_path)`

Moves a browser-saved HTML snapshot to the correct archive location. Handles:
- `file://` URI decoding
- Direct URL insertion with database update
- Waiting for downloads to complete
- Large file optimization via `deconstruct_singlefile.php`

**Calls:** `linkArchive.sh`, `ghci`, `inotifywait`, `is_downloading`

#### `is_downloading(file, [min_size_kb])`

Waits for a file to become quiescent (finished downloading). Checks modification time and minimum size.

---

### Testing

#### `lorem_update()`

Updates gold test snapshots from live site.

**Calls:** `lorem_download`, `get_lorem_pages`

#### `run_gold_test()`

Compares current site against stored snapshots for regression detection.

**Calls:** `compare_page`, `get_lorem_pages`

---

### Utility Functions

#### `bold(text)` / `red(text)` / `green(text)` / `yellow(text)`

ANSI color output helpers.

#### `wrap(command, warning)`

Executes command and displays red-highlighted warning if output is non-empty.

#### `ge` / `gev` / `gf` / `gfv` / `gec` / `gfc`

Grep wrappers: extended regex, fixed strings, inverted match, colored variants.

#### `everyNDays(n, [offset])`

Returns true every N days based on day-of-year modulus. Used for scheduling periodic tasks without randomness.

#### `sort_by_lastmodified()`

Sorts directories (from stdin) by most recently modified git-tracked file. Useful for prioritizing build operations.

#### `length()`

Sorts lines by character length.

---

## Internal Architecture

### Path Normalization Pipeline

All file-handling functions route through `path2File` for consistent path handling:

```
Input (any format)
    ↓
path2File()
    ↓ strips wiki/ prefix
    ↓ expands ~/
    ↓ removes https://gwern.net
    ↓ removes anchors (#...)
    ↓ cleans double-slashes
    ↓
/home/gwern/wiki/path/to/file
```

### PDF Processing Pipeline

PDF operations follow a consistent pattern:
1. Normalize path via `path2File`
2. Create temp file
3. Process with `pdftk`
4. Preserve metadata with `exiftool -TagsFromFile`
5. Move temp to original
6. Trigger background `crossref` update

### File Move Integration

`gwmv` coordinates multiple subsystems:
```
gwmv old new
    ├─ git mv (version control)
    ├─ rsync (server sync, background)
    ├─ gwsed (reference updates)
    ├─ stringReplace (nginx config)
    ├─ echo >> nginx.conf (add redirect)
    └─ rm annotations (cleanup)
```

---

## Key Patterns

### Parallel Processing

Functions use GNU `parallel` for batch operations:
```bash
find ... | parallel cloudflare-expire
ls $(path2File "$@") | parallel crop_one
```

### Background Jobs

Long-running operations spawn background processes:
```bash
(crossref "$ORIGINAL" &)
rsync ... gwern@server:path &
```

### Defensive Path Handling

Multiple layers of validation prevent accidental damage:
```bash
if [[ -a ~/wiki$NEW ]]; then
    red "Target exists! Will not overwrite."
    return 5
fi
```

### Tab Completion

Dynamic completion based on directory structure:
```bash
GWERNNET_DIRS_FULL="$(find wiki/doc/ -type d | ...)"
complete -W "$GWERNNET_DIRS_FULL ..." gwtag upload
```

---

## Configuration

### Environment Variables

| Variable | Purpose |
|----------|---------|
| `N` | Default Haskell parallelism (default: 14) |
| `CLOUDFLARE_TARGET` | Zone ID for cache purging |
| `CLOUDFLARE_CACHE_TOKEN` | API auth token |

### Shell Options

```bash
set -e  # Exit on error (global)
shopt -s nocasematch  # Case-insensitive matching (local to pdf-append)
```

### External Dependencies

- `pdftk` - PDF manipulation
- `exiftool` - Metadata editing
- `parallel` - Parallel execution
- `libreoffice` - Document conversion
- `img2pdf` / `convert` - Image to PDF
- `inotifywait` - File system watching
- `curl` / `jq` - API calls

---

## Integration Points

### With sync.sh

Functions like `everyNDays`, `sort_by_lastmodified`, `wrap`, and color helpers are used by the build orchestrator.

### With Haskell Tools

- `changeTag` - annotation management
- `crossref` - PDF metadata enrichment
- `linkArchive.sh` - archive database updates
- `ghci` LinkArchive.hs - direct database insertion

### With External Services

- Cloudflare API for cache management
- InvertOrNot.com API for dark-mode image reporting

### Shared State

- `~/wiki/` - wiki content directory
- `~/wiki/metadata/annotation/` - annotation files
- `~/wiki/static/redirect/*.conf` - nginx redirect configs
- `~/.bashrc` - sources this file

---

## See Also

- [sync.sh](/backend/sync-sh) - Build orchestrator that sources this library
- [hakyll.hs](/backend/hakyll-hs) - Core site generator invoked by sync.sh
- [LinkArchive.hs](/backend/link-archive-hs) - Archive URL database used by mvuri
- [changeTag.hs](/backend/change-tag-hs) - Tag modification tool invoked by gwtag
- [GTX.hs](/backend/gtx-hs) - GTX file format used by annotation functions
