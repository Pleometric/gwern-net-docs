
# Metadata.Title

**Path:** `build/Metadata/Title.hs` | **Language:** Haskell | **Lines:** ~76

> Title extraction and cleanup for web page annotations

---

## Overview

Metadata.Title handles the extraction and cleaning of titles from external web pages. Since `<title>` tags are notoriously inconsistent—containing error pages, site branding, Unicode garbage, and boilerplate—this module implements a multi-stage filtering pipeline that combines shell script extraction, regular expression cleanup, and LLM-based refinement.

The module serves the annotation system by providing clean, human-readable titles for links. Titles flow through: (1) HTML download and parsing via `download-title.sh`, (2) separator-based truncation to remove site names, (3) blocklist filtering for known-bad patterns, and (4) optional AI cleanup via `title-cleaner.py` for edge cases that survive rule-based filtering.

A key design decision is the strict substring requirement for AI-cleaned titles: any cleaned title must be a strict substring of the original, preventing LLM confabulation or rewrites. The module also provides `tooltipToMetadata` for reverse-parsing citation tooltips back into structured (title, author, date) tuples, and `wikipediaURLToTitle` for converting Wikipedia URLs into readable titles.

---

## Public API

### `htmlDownloadAndParseTitleClean :: String -> IO String`

Main entry point for fetching and cleaning a URL's title. Downloads HTML, extracts `<title>`, removes site separators and branding, filters against blocklists, and optionally invokes LLM cleanup.

**Called by:** LinkMetadata annotation scrapers
**Calls:** `htmlDownloadAndParseTitle`, `cleanTitleWithAI`, `cleanAbstractsHTML`

### `htmlDownloadAndParseTitle :: String -> IO String`

Low-level title fetch. Shells out to `download-title.sh` which uses curl and Perl's HTML::TreeBuilder to extract the raw `<title>` content.

**Called by:** `htmlDownloadAndParseTitleClean`
**Calls:** `runShellCommand` (to `download-title.sh`)

### `wikipediaURLToTitle :: String -> String`

Converts Wikipedia URLs to human-readable titles. Handles URL decoding, underscore-to-space conversion, and section anchors (converting `#` to `§`).

```haskell
wikipediaURLToTitle "https://en.wikipedia.org/wiki/Foo_Bar#Section"
-- → "Foo Bar § Section"
```

**Called by:** Wikipedia annotation handlers
**Calls:** `urlDecode`, `trimTitle`, `cleanAbstractsHTML`

### `tooltipToMetadata :: String -> String -> (String, String, String)`

Reverse-parses citation tooltips (like `"'Title', Author 2020"`) back into structured (title, author, date) tuples. Used when recovering metadata from existing annotations.

**Called by:** Metadata recovery routines
**Calls:** `filterMeta`, `pageNumberParse`, `sed`

### `cleanTitleWithAI :: String -> IO String`

Invokes `title-cleaner.py` to clean titles using GPT-4o-mini. Returns empty string on failure.

**Called by:** `htmlDownloadAndParseTitleClean`
**Calls:** `runShellCommand` (to `title-cleaner.py`)

---

## Internal Architecture

### Title Cleaning Pipeline

```
URL
  │
  ▼
┌─────────────────────────┐
│  download-title.sh      │  curl + HTML::TreeBuilder
│  (Perl HTML parsing)    │
└───────────┬─────────────┘
            │ raw <title> content
            ▼
┌─────────────────────────┐
│  Separator truncation   │  Remove text after —·|
│  (site name removal)    │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  String replacements    │  Fix encoding issues (Â, â, etc)
│  (C.stringReplace)      │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  Suffix/prefix deletion │  Remove " - YouTube", etc
│  (C.stringDelete)       │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  Blocklist filter       │  Reject "404", "Page Not Found", etc
│  (C.badStrings)         │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  Length validation      │  Reject if <5 or >500 chars
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  title-cleaner.py       │  GPT-4o-mini for edge cases
│  (LLM cleanup)          │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│  Substring validation   │  LLM result must be substring
│  (anti-confabulation)   │  of original
└───────────┴─────────────┘
```

### Configuration Data Structures

All filtering rules live in `Config.Metadata.Title`:

```haskell
-- Site separator characters (—·|)
separators :: String

-- Exact match blocklist (~180 entries)
badStrings :: [String]

-- Substring pattern blocklist
badStringPatterns :: [String]

-- Encoding fix replacements
stringReplace :: [(String, String)]

-- Suffix/prefix deletions (~200 entries)
stringDelete :: [String]
```

---

## Key Patterns

### Separator-Based Site Name Removal

Many sites append their name after a separator. The module finds the last separator and truncates:

```haskell
-- "Article Title — Site Name" → "Article Title"
if any (`elem` C.separators) title
then reverse $ tail $ dropWhile (`notElem` C.separators) $ reverse title
else title
```

This handles patterns like:
- `"Article — New York Times"`
- `"Post · Medium"`
- `"Page | Site Name"`

### Strict Substring Anti-Confabulation

The LLM cleanup has a safety check: cleaned titles must be substrings of the original:

```haskell
if titleCleaned /= title' && titleCleaned `isInfixOf` title'
then titleCleaned
else title'  -- fall back to rule-based result
```

This prevents the LLM from rewriting or adding content—it can only delete.

### Mixed Prefix/Suffix Deletion

`stringDelete` entries use trailing/leading spaces to indicate deletion type:

```haskell
-- Trailing space = prefix deletion
"GitHub - "        -- removes "GitHub - " from start

-- Leading space = suffix deletion
" - YouTube"       -- removes " - YouTube" from end
```

This is processed by `Utils.deleteMixedMany`.

---

## Configuration

### Config.Metadata.Title

| Constant | Type | Description |
|----------|------|-------------|
| `separators` | `String` | Characters used to split site names: `"—·\|"` |
| `badStrings` | `[String]` | ~180 exact-match titles to reject (404 pages, error messages, site names) |
| `badStringPatterns` | `[String]` | Substring patterns to reject ("Redirecting to", "404 ", etc.) |
| `stringReplace` | `[(String,String)]` | Encoding fixes (Â° → °, etc.) |
| `stringDelete` | `[String]` | ~200 site-specific prefixes/suffixes to strip |

### title-cleaner.py

The LLM script uses GPT-4o-mini with:
- Temperature: 0 (deterministic)
- ~400 few-shot examples covering edge cases
- Tasks: Remove boilerplate, fix encoding, convert `*italic*` to `<em>`, identify error pages

---

## Integration Points

### External Scripts

| Script | Purpose |
|--------|---------|
| `build/download-title.sh` | HTML fetch + Perl parsing |
| `build/title-cleaner.py` | LLM-based title cleanup |

### Dependencies

- **Metadata.Format:** `filterMeta`, `pageNumberParse`, `trimTitle`, `cleanAbstractsHTML`
- **Utils:** `delete`, `replace`, `sed`, `anyInfix`, `trim`, `replaceMany`, `deleteMixedMany`
- **Network.HTTP:** `urlDecode` for Wikipedia URLs
- **Data.FileStore.Utils:** `runShellCommand` for shell invocation

### Shared State

- Requires `CM.cd` to change to project root before shell commands
- Uses `OPENAI_API_KEY` environment variable (via title-cleaner.py)

---

## See Also

- [Config/Metadata/Title.hs](/backend/config-metadata-title-hs) - Configuration lists for title filtering
- [title-cleaner.py](/python/title-cleaner) - LLM-based title cleanup script
- [LinkMetadata.hs](/backend/link-metadata-hs) - Main annotation manager that calls title extraction
- [Annotation.hs](/backend/annotation-hs) - URL-to-scraper dispatcher
- [Metadata/Author.hs](/backend/metadata-author-hs) - Companion author processing module
- [Metadata/Date.hs](/backend/metadata-date-hs) - Companion date processing module
- [Metadata/Format.hs](/backend/metadata-format-hs) - Companion string cleaning module
- [italicizer.py](/python/italicizer) - Adds italics to book/movie titles
