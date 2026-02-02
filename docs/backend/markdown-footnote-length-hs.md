
# markdown-footnote-length.hs

**Path:** `build/markdown-footnote-length.hs` | **Language:** Haskell | **Lines:** ~30

> Linter that warns about overly long footnotes in Markdown files

---

## Overview

This is a standalone lint script that scans Markdown files for footnotes exceeding a character threshold. It's part of gwern.net's quality assurance tooling, helping maintain readable footnote lengths.

The script parses Markdown using Pandoc, extracts all footnote blocks, converts them back to plain text to measure their rendered length, and prints warnings for any that exceed 2,400 characters. This threshold was chosen empirically—footnotes longer than this typically overflow the visible screen area on gwern.net's layout.

The tool is designed to be run via `find` across the entire wiki directory during content review or as part of build-time linting.

---

## Public API

### CLI Usage

```bash
runghc markdown-footnote-length.hs <file.md>

# Typical batch usage:
find ~/wiki/ -name "*.md" -exec runghc markdown-footnote-length {} \;
```

**Arguments:** Single Markdown file path

**Output:** For each footnote exceeding 2,400 characters, prints `<filepath>: <footnote-content>` to stdout

**Exit:** Always exits 0 (warnings only, non-blocking)

---

## Internal Architecture

### Processing Pipeline

```
File path
    │
    ▼
TIO.readFile ─────► Raw Markdown Text
    │
    ▼
readMarkdown ─────► Pandoc AST (or parse error)
    │
    ▼
bottomUpM ────────► Traverse AST, checking each Inline
    │
    ▼
footNoteCheck ────► For Note nodes: convert to plain text, check length
    │
    ▼
TIO.putStrLn ─────► Print warning if > 2400 chars
```

### Key Functions

| Function | Purpose |
|----------|---------|
| `main` | Entry point, reads file from args |
| `processLint` | Parses Markdown, initiates AST traversal |
| `footNoteCheck` | Checks individual footnote length |

### Data Flow

1. **Parse**: `readMarkdown` with full `pandocExtensions` converts Markdown to Pandoc AST
2. **Traverse**: `bottomUpM` walks the AST bottom-up, applying `footNoteCheck` to each `Inline` node
3. **Check**: When a `Note` inline is found, its block content is rendered back to plain text via `writePlain`
4. **Warn**: If plain text length > 2,400 characters, print the file path and footnote content

---

## Key Patterns

### Bottom-Up Monadic Traversal

The script uses Pandoc's `bottomUpM` for AST traversal, which processes children before parents. For footnote checking, traversal order doesn't matter—each `Note` is independent—but `bottomUpM` provides a convenient way to visit every `Inline` node with monadic effects (IO for printing warnings).

```haskell
bottomUpM (footNoteCheck f) x'
```

### Plain Text Rendering for Length Measurement

Rather than measuring the raw Markdown source (which includes syntax characters), the script renders footnote content to plain text first. This gives a more accurate measure of the *displayed* length:

```haskell
let md = runPure $ writePlain def (Pandoc nullMeta cntnts)
```

---

## Configuration

### Length Threshold

| Constant | Value | Location | Purpose |
|----------|-------|----------|---------|
| Character limit | 2,400 | Hardcoded in `footNoteCheck` | Maximum footnote length before warning |

The threshold is not configurable via command line—it's embedded in the source at line 27:

```haskell
when (T.length md' > 2400) $ ...
```

To change it, modify the source directly.

---

## Integration Points

### Build System

- Not currently integrated into `sync.sh` or the main build pipeline
- Designed for manual/periodic content review
- Can be added to pre-commit hooks or CI

### Dependencies

- **Pandoc library**: For Markdown parsing and plain text rendering
- **Text.Pandoc.Definition**: `Inline(Note)` pattern for footnote matching

### Input/Output

- **Input**: Any `.md` file with Pandoc-compatible footnotes
- **Output**: Warnings to stdout (not stderr), no file modifications
- **Side effects**: None beyond console output

---

## See Also

- [hakyll.hs](/backend/hakyll-hs) - Main build system with footnote processing
- [Typography.hs](/backend/typography-hs) - AST transforms including footnote formatting
- [markdown-lint.sh](/shell/markdown-lint) - Linting script that invokes this tool
- [markdown-length-checker.hs](/backend/markdown-length-checker-hs) - Related line length validator
- [sync.sh](/backend/sync-sh) - Build orchestrator (potential integration point)
