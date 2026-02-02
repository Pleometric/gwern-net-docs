
# markdown-length-checker.hs

**Path:** `build/markdown-length-checker.hs` | **Language:** Haskell (runghc script) | **Lines:** ~36

> Validates that code blocks in Markdown files don't contain overly long lines

---

## Overview

This is a simple linting tool that scans Markdown files for code blocks containing lines longer than 110 characters. The rationale is that excessively long lines in code examples indicate source code that could benefit from refactoring for clarity and readability.

The script uses Pandoc to parse Markdown into an AST, then queries all `CodeBlock` elements to find lines exceeding the threshold. It's designed for batch processing via `find -exec`, making it easy to scan an entire documentation tree.

This fits into gwern.net's quality assurance tooling—ensuring that code examples in essays and documentation remain readable without horizontal scrolling, especially important given the site's focus on long-form content consumption.

---

## Public API

### `main :: IO ()`

Entry point. Reads a single filename from command-line arguments, processes it, and prints any violations to stdout.

**Usage:**
```bash
runghc markdown-length-checker.hs path/to/file.md
# Or batch:
find ~/wiki/ -name "*.md" -exec runghc markdown-length-checker.hs {} \;
```

**Output format:** `filepath: CodeBlock (attr) "offending content..."`

---

### `processLint :: FilePath -> T.Text -> T.Text`

Core processing function. Parses Markdown content and returns formatted violation messages.

**Called by:** `main`
**Calls:** `lineCheck`, Pandoc's `readMarkdown`

**Returns:** Empty text if no violations, otherwise newline-separated list of violations prefixed with filename.

---

## Internal Architecture

### Processing Pipeline

```
File content
    ↓
readMarkdown (Pandoc parser with full extensions)
    ↓
topDown longCodeLines (transform: mark blocks with long lines)
    ↓
queryWith clean (extract: collect only CodeBlocks)
    ↓
Format violations with filename prefix
```

### Key Functions

| Function | Purpose |
|----------|---------|
| `lineCheck` | Combines transformation and extraction into single query |
| `longCodeLines` | Transforms CodeBlocks: keeps block if any line ≥110 chars, replaces with `Plain []` otherwise |
| `clean` | Filter that extracts only CodeBlock elements from AST |

### Data Flow

The `topDown longCodeLines` traversal visits every `Block` in the document. For `CodeBlock` nodes, it checks each line's length—if any line reaches 110+ characters, the block is preserved; otherwise it's replaced with an empty `Plain []` sentinel. The subsequent `queryWith clean` collects only the surviving `CodeBlock` elements.

---

## Key Patterns

### Pandoc AST Querying

Uses Pandoc's generic traversal combinators:
- `topDown` for transformation (modifying nodes while traversing)
- `queryWith` for extraction (collecting matching nodes)

This two-phase approach (transform then query) is slightly unusual—a single `queryWith` with filtering would be more direct—but it works correctly.

### Threshold Hardcoding

The 110-character limit is hardcoded in `longCodeLines`. This value is a reasonable default for code readability, roughly matching common terminal widths and editor configurations.

### Minimal Error Handling

Parse failures are reported but don't halt batch processing. The error message includes the problematic file path and Pandoc's error details.

---

## Configuration

| Setting | Location | Default | Notes |
|---------|----------|---------|-------|
| Line length threshold | `longCodeLines` function | 110 chars | Hardcoded; edit source to change |
| Pandoc extensions | `processLint` | `pandocExtensions` | Full extension set for gwern.net compatibility |

---

## Integration Points

### Build System

Not directly integrated into [sync-sh](sync-sh)—this is a manual/ad-hoc linting tool rather than a build-blocking check.

### Typical Usage

```bash
# Check all Markdown files
find ~/wiki/ -name "*.md" -exec runghc markdown-length-checker.hs {} \;

# Check single file
runghc markdown-length-checker.hs essay.md
```

### Output Interpretation

Empty output means all code blocks pass. Non-empty output shows the full `CodeBlock` AST node for each violation, which includes:
- Block attributes (language annotation, classes, key-value pairs)
- The complete code content (not just the offending lines)

---

## See Also

- [hakyll.hs](/backend/hakyll-hs) - Main site generator using Pandoc AST
- [sync.sh](/backend/sync-sh) - Build orchestrator (potential integration point)
- [markdown-lint.sh](/shell/markdown-lint) - Linting script that may invoke this tool
- [markdown-footnote-length.hs](/backend/markdown-footnote-length-hs) - Related footnote length checker
- [Typography.hs](/backend/typography-hs) - Pandoc AST transformations
