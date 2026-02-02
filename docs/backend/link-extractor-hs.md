
# link-extractor.hs

**Path:** `build/link-extractor.hs` | **Language:** Haskell (runghc script) | **Lines:** ~54

> CLI tool for extracting hyperlinks from Markdown and HTML files

---

## Overview

`link-extractor.hs` is a command-line utility that parses Markdown or HTML files and outputs all hyperlinks found within them. It's designed for bulk URL extraction across the gwern.net corpus, enabling link analysis, frequency counting, and dead-link detection workflows.

The tool handles several gwern.net-specific concerns: it rewrites interwiki links (like `!W`) to their full URLs, converts anchor-only links (like `#section`) to absolute paths based on the source filename, and can optionally prefix each URL with its source file for tracking provenance. It delegates parsing to the `Query` module, which uses Pandoc under the hood.

The script is designed for process-level parallelization—you can run hundreds of instances via GNU `parallel` to process the entire site quickly. This external parallelization strategy keeps the script simple while still achieving high throughput.

---

## Public API

### CLI Interface

```bash
link-extractor.hs [--print-filenames] [file...]
```

**Arguments:**
- `--print-filenames` — Prefix each URL with its source file path (format: `filepath:url`)
- `file...` — One or more Markdown (`.md`) or HTML (`.html`) files to process

**Stdin mode:** If no files are provided, reads Markdown from stdin (falls back to HTML parsing if Markdown yields no links).

**Output:** Newline-delimited list of URLs to stdout.

---

### `main :: IO ()`

Entry point. Parses arguments, dispatches to stdin or file processing mode.

**Called by:** Shell invocation
**Calls:** `printURLs`, `Query.extractLinks`

---

### `printURLs :: Bool -> FilePath -> IO ()`

Reads a single file, extracts its links, rewrites anchor-only links to include the source filename, and prints results.

**Parameters:**
- `printfilename` — Whether to prefix URLs with source filepath
- `file` — Path to the file to process

**Called by:** `main`
**Calls:** `Query.extractLinks`, `System.Directory.doesFileExist`

---

## Internal Architecture

### Control Flow

```
main
 ├─ No args → read stdin as Markdown
 │            └─ if no links found → retry as HTML
 │            └─ print links
 │
 └─ With args → for each file:
                 └─ printURLs
                      ├─ verify file exists
                      ├─ read file content
                      ├─ extractLinks (Markdown if .md, else HTML)
                      ├─ rewrite anchor-only links (#foo → /basename#foo)
                      └─ print (optionally with filename prefix)
```

### Anchor Rewriting

The tool transforms self-referential anchor links into absolute paths:

```haskell
-- Input: #discriminator-ranking in file ~/wiki/face.md
-- Output: /face#discriminator-ranking
let converted' = map (\u -> if T.head u /= '#' then u
                            else "/" `T.append` T.pack (takeBaseName file) `T.append` u) converted
```

This enables meaningful frequency analysis—you can see how often sections are cross-referenced both within and across pages.

---

## Key Patterns

### Markdown-First with HTML Fallback

When reading from stdin, the tool tries Markdown parsing first. If that yields no URLs, it retries as HTML:

```haskell
let links = extractLinks True stdin  -- Try Markdown
let links' = if links /= [] then links else extractLinks False stdin  -- Fallback to HTML
```

This handles the common case where users pipe in annotation snippets that might be either format.

### File Extension Detection

For file arguments, the parser is chosen based on extension:

```haskell
let converted = extractLinks (".md"`isSuffixOf`file) input
```

Files ending in `.md` are parsed as Markdown; everything else is treated as HTML.

---

## Configuration

No configuration files. Behavior is controlled entirely via CLI flags.

| Flag | Effect |
|------|--------|
| `--print-filenames` | Output format becomes `filepath:url` instead of just `url` |

---

## Integration Points

### Dependencies

- **Query.hs** — Provides `extractLinks` for Pandoc-based URL extraction
- **Interwiki** (via Query) — Resolves `!W`, `!Wikipedia` and similar shorthand links
- **Pandoc** — Underlying Markdown/HTML parser

### Typical Usage in Build Pipeline

```bash
# Extract all links from Markdown files, parallelized
find . -name '*.md' | parallel --max-args=500 --jobs=30 \
    runghc -i./build ./build/link-extractor.hs --print-filenames

# Pipe clipboard content for quick link extraction
xclip -o | runghc -i./build ./build/link-extractor.hs
```

### Output Format

**Default mode:**
```
/local/file.pdf
https://example.com/page
/face#discriminator-ranking
```

**With `--print-filenames`:**
```
/home/gwern/wiki/face.md:/local/file.pdf
/home/gwern/wiki/face.md:https://example.com/page
/home/gwern/wiki/face.md:/face#discriminator-ranking
```

---

## See Also

- [link-suggester.hs](/backend/link-suggester-hs) - Advanced suggestion tool that uses extracted links
- [link-prioritize.hs](/backend/link-prioritize-hs) - Ranks unannotated links by frequency
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database that consumes link data
- [Query.hs](/backend/query-hs) - Core URL extraction logic using Pandoc
- [Interwiki.hs](/backend/interwiki-hs) - Resolves interwiki links during extraction
- [sync.sh](/backend/sync-sh) - Build orchestrator that may invoke this tool
