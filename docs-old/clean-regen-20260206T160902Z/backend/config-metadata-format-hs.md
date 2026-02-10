
# Config.Metadata.Format

**Path:** `build/Config/Metadata/Format.hs` | **Language:** Haskell | **Lines:** ~1,936

> Massive collection of HTML rewrite rules for cleaning and normalizing abstract text

---

## Overview

Config.Metadata.Format is a configuration module containing over 1,400 string rewrite rules for cleaning, normalizing, and improving the typography of scraped abstract HTML. When abstracts are scraped from sources like arXiv, bioRxiv, PubMed, or PDFs, they contain inconsistent formatting, malformed HTML, LaTeX artifacts, and typographic errors. This module defines the patterns to fix them all.

The rewrites fall into three categories: regex patterns applied before fixed-string rewrites, the fixed-string rewrites themselves (the bulk of the rules), and regex patterns applied after. This three-phase approach allows for order-dependent transformations—for example, removing JEL classification codes before they get incorrectly interpreted as scientific notation.

The module also contains author-cleaning rules (for normalizing author names scraped from PDFs) and blacklists for filtering out junk metadata (like "Adobe InDesign" or "Unknown" that PDF tools sometimes put in author fields). The sheer size of this file reflects years of accumulated edge cases encountered when processing academic papers at scale.

---

## Public API

### `htmlRewriteFixed :: [(String, String)]`

~1,200 fixed string replacements for HTML cleanup. Applied as simple find-and-replace operations.

**Used by:** [Metadata.Format](metadata-format-hs) via `replaceMany`
**Examples:**
- `("</p></p>", "</p>")` — collapse double close tags
- `(" -- ", "—")` — convert ASCII dashes to em-dashes
- `("<jats:p>", "<p>")` — normalize JATS XML to standard HTML

### `htmlRewriteRegexpBefore :: [(String, String)]`

Regex patterns applied before fixed-string rewrites. Currently contains 2 rules.

**Used by:** [Metadata.Format](metadata-format-hs) via `sedMany`
**Example:** `("\\(JEL [A-Z][0-9][0-9]+\\)\\.?", "")` — remove JEL classification codes

### `htmlRewriteRegexpAfter :: [(String, String)]`

~140 regex patterns applied after fixed-string rewrites.

**Used by:** [Metadata.Format](metadata-format-hs) via `sedMany`
**Examples:**
- `("from ([0-9\\.]+) to ([0-9\\.]+)", "\\1 → \\2")` — "from 8 to 256" → "8 → 256"
- `("([0-9]+)[ -]fold", "\\1×")` — "10-fold" → "10×"

### `htmlRewriteTestCases :: [(String, String)]`

Test cases for verifying rewrite correctness.

**Used by:** [Test.hs](test-hs)

### `cleanAuthorsRegexps :: [(String, String)]`

Regex patterns for normalizing author name formatting.

**Used by:** [Metadata.Author](metadata-author-hs)
**Examples:**
- `("([A-Z]\\.)([A-Za-z]+)", "\\1 \\2")` — "A.Smith" → "A. Smith"
- `("^([A-Z][a-z]+), ([A-Z]\\.)$", "\\2 \\1")` — "Smith, J." → "J. Smith"

### `cleanAuthorsFixedRewrites :: [(String, String)]`

Fixed string replacements for author name cleanup.

**Used by:** [Metadata.Author](metadata-author-hs)
**Examples:** Remove degree suffixes like " PhD", " MD", " M.Sc.", etc.

### `filterMetaBadSubstrings :: [String]`

Substrings that indicate junk metadata to filter out.

**Used by:** [Metadata.Format](metadata-format-hs) via `filterMeta`
**Examples:** "Adobe", "Microsoft", "Unknown", "LaTeX", "pdftk"

### `filterMetaBadWholes :: [String]`

Exact strings that indicate junk metadata.

**Examples:** "user", "Owner", "Admin", "template"

### `balancedBracketTestCases :: [(String, String)]`

Test cases for bracket-balancing validation.

### `printDoubleTests :: [(Double, Int, String)]`

Test cases for numeric formatting (doubles to strings with commas).

---

## Internal Architecture

### Rewrite Pipeline

The cleaning pipeline in [Metadata.Format](metadata-format-hs) applies rules in this order:

```
input → sedMany htmlRewriteRegexpBefore
      → replaceMany htmlRewriteFixed
      → sedMany htmlRewriteRegexpAfter
      → trim
      → fixedPoint (repeat until stable)
```

The `fixedPoint` wrapper ensures rules are applied until the output stabilizes, handling cases where one rewrite creates conditions for another.

### Rule Categories

**HTML Normalization:**
- JATS XML → standard HTML (`<jats:p>` → `<p>`)
- Fix malformed tags (`<p><p>` → `<p>`)
- Normalize whitespace and line breaks

**Typography:**
- ASCII to Unicode symbols (`->` → `→`, `>=` → `≥`)
- Proper minus signs (`-0.5` → `−0.5` using U+2212)
- En-dashes for ranges (`1-10` → `1–10`)
- Em-dashes for parentheticals
- Superscripts/subscripts for math (`10^4` → `10<sup>4</sup>`)
- Thousand separators (`1000000` → `1,000,000`)

**Scientific Notation:**
- LaTeX math spans → HTML (`<span class="math inline">\(\alpha\)</span>` → `α`)
- Common equations formatted properly
- Big-O notation normalized (`O(n log n)` → `𝒪(<em>n</em> log <em>n</em>)`)

**Statistical Formatting:**
- p-values italicized (`p = 0.05` → `<em>p</em> = 0.05`)
- Sample sizes (`n = 100` → `<em>n</em> = 100`)
- Effect sizes (`r = 0.5` → `<em>r</em> = 0.5`)
- Heritability notation (`h2` → `<em>h</em><sup>2</sup>`)

**Spelling & Style:**
- British → American spelling (`colour` → `color`, `behaviour` → `behavior`)
- Hyphenation fixes (`long- term` → `long-term`)
- Word number → digit (`three` → `3`)
- "Significant" disambiguation (adds "statistically-" prefix where appropriate)

**Taxonomic Names:**
- Italicize species names (`Homo sapiens` → `<em>Homo sapiens</em>`)

**Abstract Structure:**
- Normalize section headers (`<strong>METHODS</strong>:` → `<strong>Method</strong>:`)
- Roman numerals → Arabic (`(iii)` → `(3)`)

---

## Key Patterns

### Fixed-Point Iteration

Some rewrites interact, requiring multiple passes:
```haskell
cleanAbstractsHTML = fixedPoint cleanAbstractsHTML'
```

For example, fixing `<p><p>` might create a new `<p><p>` after other cleanups.

### Cycle Detection

The test suite checks for infinite loops via `testInfixRewriteLoops`:
```haskell
cleanAbstractsHTMLTest = testInfixRewriteLoops C.htmlRewriteFixed cleanAbstractsHTML
```

### Ordered Application

Pre-regexes run first to prevent misinterpretation:
- JEL codes like "R2" would otherwise become R<sup>2</sup>

### Math Span Conversion

Hundreds of rules convert specific LaTeX math spans to Unicode/HTML:
```haskell
("<span class=\"math inline\">\\(\\alpha\\)</span>", "α")
("<span class=\"math inline\">\\(\\times\\)</span>", "×")
```

This is more reliable than generic LaTeX parsing for the common cases.

---

## Configuration

All rules are defined as constant lists at module level. To add a new rule:

1. Add to appropriate list (`htmlRewriteFixed`, `htmlRewriteRegexpAfter`, etc.)
2. Add test case to `htmlRewriteTestCases` if complex
3. Rebuild and run `test.hs` to check for cycles

Rule ordering within lists matters for fixed-string rewrites (earlier rules apply first).

---

## Integration Points

### Consumers

- **[Metadata.Format](metadata-format-hs):** Main consumer via `cleanAbstractsHTML`
- **[Metadata.Author](metadata-author-hs):** Uses author-cleaning rules
- **[Test.hs](test-hs):** Validates rules and checks for cycles

### Used Throughout Annotation Pipeline

All annotation scrapers eventually call `cleanAbstractsHTML`:
- [Annotation.Arxiv](annotation-arxiv-hs)
- [Annotation.Biorxiv](annotation-biorxiv-hs)
- [Annotation.PDF](annotation-pdf-hs)
- [Annotation.OpenReview](annotation-openreview-hs)
- [Annotation.Gwernnet](annotation-gwernnet-hs)

---

## See Also

- [Metadata/Format.hs](/backend/metadata-format-hs) - Applies these rules via cleanAbstractsHTML
- [Metadata/Author.hs](/backend/metadata-author-hs) - Applies author-cleaning rules
- [LinkMetadata.hs](/backend/link-metadata-hs) - Main consumer of cleaned abstracts
- [Annotation/Arxiv.hs](/backend/annotation-arxiv-hs) - Scraper that uses these rules
- [Annotation/Biorxiv.hs](/backend/annotation-biorxiv-hs) - Scraper that uses these rules
- [Config/Metadata/Author.hs](/backend/config-metadata-author-hs) - Companion author configuration
- [Config/Metadata/Title.hs](/backend/config-metadata-title-hs) - Companion title configuration
