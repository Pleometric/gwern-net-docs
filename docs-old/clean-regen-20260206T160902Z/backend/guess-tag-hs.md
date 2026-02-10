
# guessTag.hs

**Path:** `build/guessTag.hs` | **Language:** Haskell | **Lines:** ~11

> CLI tool for expanding short or partial tag names to full tag paths

---

## Overview

`guessTag.hs` is a minimal command-line utility that expands abbreviated, partial, or misspelled tag names into their canonical full-path form. For example, inputting `"sr1"` produces `"darknet-market/silk-road/1"`, and `"gpt-4"` expands to `"ai/nn/transformer/gpt/4"`.

The tool enables ergonomic tag entry during annotation workflows—authors can type shorthand like `"rl"`, `"silk-road"`, or even typos like `"psycholoyg"`, and the system resolves them to proper directory-based tag paths. This bridges the gap between human convenience and the strict filesystem-based tag hierarchy that gwern.net uses.

All the intelligence lives in the `Tags` module; this executable is simply a thin CLI wrapper that reads available tags from disk and delegates to `guessTagFromShort`.

---

## Public API

### `main :: IO ()`

Entry point. Reads all valid tags from the `/doc/` hierarchy, takes the first command-line argument as input, and prints the expanded tag to stdout.

**Called by:** Shell scripts, editor integrations, annotation tooling
**Calls:** `Tags.listTagsAll`, `Tags.guessTagFromShort`

---

## Internal Architecture

The executable has no internal state or complex logic:

```haskell
main = do
  tags <- listTagsAll           -- Get all valid tag directories
  (arg:_) <- getArgs            -- Read first CLI argument
  putStr (guessTagFromShort [] tags arg)  -- Expand and print
```

The real work happens in `Tags.guessTagFromShort`, which implements a cascading fallback strategy:

1. **Exact match** — If input matches a tag exactly, return it
2. **Petname lookup** — Check `tagsShort2Long` for explicit shortcuts (e.g., `"sr1"` → `"darknet-market/silk-road/1"`)
3. **Suffix match** — Find tags ending with `"/" ++ input` (e.g., `"road/1"` → `"darknet-market/silk-road/1"`)
4. **Infix path segment** — Match middle path segments (e.g., `"transformer"` → `"ai/nn/transformer"`)
5. **Partial segment match** — More liberal infix/suffix matching
6. **Dot-to-slash rewrite** — Try replacing `.` with `/` (e.g., `"ai.dataset"` → `"ai/dataset"`)
7. **Levenshtein distance** — Fuzzy match typos within 3 edits (e.g., `"sunkcost"` → `"sunk-cost"`)

The function iterates to a fixed point, allowing chained rewrites.

---

## Key Patterns

### Disambiguation via Blacklist

Hopelessly ambiguous short tags (single letters, common words) are blacklisted:

```haskell
shortTagBlacklist = ["a", "an", "the", "to", "if", "is", ...]
```

Attempting to use these as input raises an error, forcing explicit full tags.

### Test Suite Embedded in Config

`Config.Tags` includes ~300 test cases (`shortTagTestSuite`) that verify tag expansion correctness. Each known abbreviation maps to its expected output, and every valid tag must round-trip to itself.

### Typo Tolerance

The Levenshtein-based fallback uses a maximum distance of 3 edits (`tagTypoMaxDistance`), catching common typos without false positives on unrelated tags.

---

## Configuration

All configuration lives in `Config/Tags.hs`:

| Constant | Purpose |
|----------|---------|
| `tagTypoMaxDistance` | Max edit distance for fuzzy matching (default: 3) |
| `tagsShort2Long` | Explicit petname → full tag mappings (~500 entries) |
| `tagsLong2Short` | Display abbreviations (used elsewhere for rendering) |
| `shortTagBlacklist` | Inputs that error instead of guessing |
| `shortTagTestSuite` | Test cases for validation |

---

## Integration Points

### Filesystem Dependency

`listTagsAll` scans `/doc/*/index.md` files to discover valid tags. The working directory must be set correctly (handled by `Config.Misc.cd`).

### Annotation Workflow

Used during annotation entry to normalize user-typed tags before storing in the metadata database. Likely invoked by editor plugins or the `changeTag` tool.

### Related Tools

- `changeTag.hs` — Bulk tag renaming, also uses `guessTagFromShort`
- `LinkMetadata.hs` — Stores normalized tags in the annotation database

---

## See Also

- [Tags.hs](/backend/tags-hs) - Core tag manipulation logic (where `guessTagFromShort` is defined)
- [Config.Tags](/backend/config-tags-hs) - Tag abbreviations, aliases, and configuration tables
- [changeTag.hs](/backend/change-tag-hs) - Related CLI tool for bulk tag add/remove operations
- [LinkMetadata.hs](/backend/link-metadata-hs) - Stores normalized tags in the annotation database
- [Hakyll.hs](/backend/hakyll-hs) - Site generator using tag expansion
- [GTX.hs](/backend/gtx-hs) - Annotation format storing tag assignments
