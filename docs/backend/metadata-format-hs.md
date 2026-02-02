
# Metadata/Format.hs

**Path:** `build/Metadata/Format.hs` | **Language:** Haskell | **Lines:** ~141

> String sanitization and HTML cleanup for annotation metadata

---

## Overview

Metadata/Format.hs provides string-munging utilities for cleaning and standardizing annotation metadata. It handles malformed HTML from academic paper abstracts, normalizes formatting inconsistencies, enforces house style conventions, and sanitizes problematic input from web scrapers.

The module centers on `cleanAbstractsHTML`, a fixed-point rewrite engine that applies hundreds of pattern transformations until the output stabilizes. This handles the diverse mess that comes from scraping PDFs, Arxiv, PubMed, Wikipedia, and other sources—each with their own encoding quirks, broken markup, and formatting oddities.

Supporting utilities handle DOI normalization, URL canonicalization, bracket balancing checks, title cleanup, and a custom double-to-string formatter that avoids scientific notation for the inflation-adjuster JavaScript.

---

## Public API

### `cleanAbstractsHTML :: String -> String`

Main entry point for HTML sanitization. Applies fixed-point iteration of rewrites until the string stops changing.

**Called by:** LinkMetadata.hs (when processing scraped abstracts)
**Calls:** `fixedPoint`, `sedMany`, `replaceMany`, `trim` (from Utils)

### `cleanAbstractsHTMLTest :: [(String, String, String)]`

Returns any rewrite patterns that would cause infinite loops. Empty list means safe.

**Called by:** Test harness
**Calls:** `testInfixRewriteLoops` (from Cycle)

### `linkCanonicalize :: String -> String`

Normalizes URLs to canonical form. Converts `https://gwern.net/` to `/`, fixes Arxiv PDF→abstract URLs, normalizes Twitter/LessWrong domains.

**Called by:** LinkMetadata.hs, annotation pipeline
**Calls:** `replace`, `sedMany`, `delete`

### `trimTitle :: String -> String`

Cleans title strings: removes trailing periods, fixes Elsevier's underscore-for-colon substitution, normalizes whitespace.

**Called by:** Metadata.Title, annotation scrapers
**Calls:** `sedMany`, `replaceMany`, `trim`

### `filterMeta :: String -> String`

Filters out garbage metadata (PDF tool names, OCR artifacts, nonsense strings). Returns empty string if input matches any blacklist.

**Called by:** Metadata extraction pipeline
**Calls:** `anyInfix`, `elem`

### `balanced :: String -> String`

Checks if brackets and quotes are balanced. Returns empty string if balanced, otherwise returns the substring starting from the first unbalanced character.

**Called by:** Validation/sanity checks
**Calls:** Pure recursion

### `printDouble :: Int -> Double -> String`

Formats doubles without scientific notation, with comma-separated thousands. Used for inflation-adjusted dollar amounts.

**Called by:** Inflation.hs, JavaScript data generation
**Calls:** `showFFloat`, custom formatting

### `processDOI :: String -> String`

Normalizes DOI strings: removes `doi:` prefix and `https://doi.org/` prefix, fixes dash variants.

**Called by:** Annotation scrapers
**Calls:** `replaceMany`, `sed`

### `processDOIArxiv :: String -> String`

Converts Arxiv URLs to DOI format (10.48550/arXiv.XXXX.XXXXX).

**Called by:** Arxiv annotation handler
**Calls:** `sed`

### `pageNumberParse :: String -> String`

Extracts page number from PDF fragment URLs (e.g., `foo.pdf#page=50` → `"50"`).

**Called by:** PDF annotation handlers
**Calls:** `sed`

### `checkURL :: String -> IO ()`

Validates URL for common errors (double URLs from copy-paste accidents). Throws error on failure.

**Called by:** Annotation pipeline entry points
**Calls:** Regex match

---

## Internal Architecture

### Rewrite Pipeline

The `cleanAbstractsHTML` function applies transformations in three phases:

```
Input → sedMany regexpBefore → replaceMany fixed → sedMany regexpAfter → trim → (repeat until stable)
```

1. **Regexp Before** (`htmlRewriteRegexpBefore`): Regex patterns applied first
2. **Fixed Rewrites** (`htmlRewriteFixed`): Literal string→string substitutions (~700 patterns)
3. **Regexp After** (`htmlRewriteRegexpAfter`): Regex patterns applied last (~130 patterns)

The `fixedPoint` wrapper re-applies the entire pipeline until output equals input, handling cascading transformations.

### Configuration Storage

All rewrite patterns live in `Config.Metadata.Format`:

| Collection | Type | Purpose |
|------------|------|---------|
| `htmlRewriteFixed` | `[(String, String)]` | Literal substitutions |
| `htmlRewriteRegexpBefore` | `[(String, String)]` | Pre-processing regexes |
| `htmlRewriteRegexpAfter` | `[(String, String)]` | Post-processing regexes |
| `filterMetaBadSubstrings` | `[String]` | Author field blacklist (partial match) |
| `filterMetaBadWholes` | `[String]` | Author field blacklist (exact match) |
| `cleanAuthorsRegexps` | `[(String, String)]` | Author name normalization |
| `cleanAuthorsFixedRewrites` | `[(String, String)]` | Author credential removal |

---

## Key Patterns

### Fixed-Point Iteration

The rewrite engine uses `fixedPoint` to handle cascading transformations:

```haskell
cleanAbstractsHTML = fixedPoint cleanAbstractsHTML'
 where cleanAbstractsHTML' = trim . sedMany regexpAfter . replaceMany fixed . sedMany regexpBefore
```

A single pass might produce `<p><p>text</p></p>`, which needs another pass to collapse to `<p>text</p>`.

### Math Inline Conversion

Hundreds of patterns convert Pandoc's LaTeX math spans to HTML:

```haskell
("<span class=\"math inline\">\\(\\alpha\\)</span>", "α")
("<span class=\"math inline\">\\(O(n^2)\\)</span>", "𝒪(<em>n</em><sup>2</sup>)")
```

### Statistical Significance Disambiguation

Rewrites "significant" → "statistically-significant" in statistical contexts:

```haskell
(" significant difference", " statistically-significant difference")
(" significantly associated", " statistically-significantly associated")
```

This prevents confusion between "clinically significant" and "statistically significant".

### JATS/XML Cleanup

Handles malformed academic XML (JATS format from PubMed/PMC):

```haskell
("</jats:p>", "</p>")
("<jats:italic>", "<em>")
("<jats:sec>", "")
```

### Typography Normalization

Fixes common formatting issues:

```haskell
(" - ", "—")           -- spaced hyphen → em dash
("×10[-−–—]([0-9]+)", " × 10<sup>−\\1</sup>")  -- scientific notation
(" 0\\.([0-9])", " 0.\\1")  -- restore missing leading zeros
```

---

## Configuration

All patterns are in `Config.Metadata.Format.hs` (~1800 lines). Key collections:

| Collection | Size | Purpose |
|------------|------|---------|
| `htmlRewriteFixed` | ~700 | Literal string substitutions |
| `htmlRewriteRegexpAfter` | ~130 | Post-processing regexes |
| `filterMetaBadSubstrings` | ~170 | OCR/tool name blacklist |
| `filterMetaBadWholes` | ~180 | Exact-match garbage strings |
| `printDoubleTests` | ~50 | Test cases for number formatting |
| `balancedBracketTestCases` | ~25 | Test cases for bracket balancing |

---

## Integration Points

### Input Sources

- **LinkMetadata.hs**: Passes scraped abstracts through `cleanAbstractsHTML`
- **Annotation scrapers**: Use `processDOI`, `trimTitle`, `filterMeta`
- **Inflation.hs**: Uses `printDouble` for currency formatting

### Shared Utilities

Relies heavily on Utils.hs functions:
- `sedMany`: Apply multiple regex substitutions
- `replaceMany`: Apply multiple literal substitutions
- `fixedPoint`: Iterate until stable
- `trim`: Whitespace cleanup
- `anyInfix`: Substring matching

### Loop Detection

Uses `Cycle.testInfixRewriteLoops` to detect potential infinite loops in rewrite patterns. The test suite checks that no pattern combination can cause unbounded expansion.

---

## See Also

- [Config/Metadata/Format.hs](/backend/config-metadata-format-hs) - Massive collection of rewrite rules
- [LinkMetadata.hs](/backend/link-metadata-hs) - Main consumer of cleanAbstractsHTML
- [Metadata/Author.hs](/backend/metadata-author-hs) - Author name normalization
- [Metadata/Date.hs](/backend/metadata-date-hs) - Date parsing and normalization
- [Metadata/Title.hs](/backend/metadata-title-hs) - Title extraction and cleaning
- [Annotation/Arxiv.hs](/backend/annotation-arxiv-hs) - Scraper that uses these utilities
- [paragraphizer.py](/python/paragraphizer) - LLM-based paragraph splitting
