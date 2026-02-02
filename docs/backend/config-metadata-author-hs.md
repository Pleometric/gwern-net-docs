
# Config.Metadata.Author

**Path:** `build/Config/Metadata/Author.hs` | **Language:** Haskell | **Lines:** ~3,362

> Configuration data for author name canonicalization and linking

---

## Overview

Config.Metadata.Author is a large configuration module containing lookup tables for author name normalization and hyperlinking. It holds no logic—only data structures that define how variant author names (Twitter handles, abbreviated names, misspellings, non-ASCII variants) map to canonical forms, and how canonical names map to biographical URLs.

The module enables a "write once, link everywhere" model for author metadata: define "Eliezer Yudkowsky" → `https://yudkowsky.net` once, and every annotation mentioning that author automatically gets a hyperlink. Author backlinks then emerge naturally—the author's bio page shows every annotation where they appear.

The design separates configuration data from logic. All the actual processing (canonicalization, link insertion, collapse formatting) lives in `Metadata.Author`. This module provides the raw mappings consumed by that logic.

---

## Public API

### `canonicals :: M.Map String String`

Maps alternative author names to their canonical form. Contains ~770 entries covering:
- Twitter handles → real names: `"karpathy" → "Andrej Karpathy"`
- Name variants → standard form: `"E. Yudkowsky" → "Eliezer Yudkowsky"`
- Pseudonyms → real names: `"Student" → "William Sealy Gosset"`
- Typos and encoding issues: `"Jurgen Schmidhuber" → "Jürgen Schmidhuber"`

**Called by:** `Metadata.Author.authorDB`
**Used for:** Pre-link canonicalization of author fields

### `canonicalsWithInitials :: [String]`

List of ~200 full names that should have abbreviation variants auto-generated. Rather than manually listing "K. Ong", "K. J. Ong", etc., the name "Ken K. Ong" here causes `Metadata.Author.name2Abbreviations` to generate all plausible abbreviations automatically.

**Called by:** `Metadata.Author.authorDB`
**Used for:** Generating exhaustive initial-based variants

### `authorLinkDB :: M.Map T.Text T.Text`

Maps canonical author names to biographical URLs. Contains ~1,400+ entries combining:
1. Auto-generated Wikipedia URLs from `authorWpLinkDB`
2. Override URLs for authors with better homepages than Wikipedia

Examples:
```haskell
("Gwern", "/index#abstract")
("Scott Alexander", "https://www.astralcodexten.com/")
("Michael Nielsen", "https://michaelnielsen.org/")
```

Disambiguation via anchor syntax:
```haskell
("George Washington#SS", "https://en.wikipedia.org/wiki/SS_George_Washington")
```

**Called by:** `Metadata.Author.linkify`, `link-prioritize.hs`
**Used for:** Converting author names to hyperlinks

### `authorWpLinkDB :: [T.Text]`

List of ~1,200 author names that have Wikipedia articles. Converted to Wikipedia URLs via `Interwiki.toWikipediaEnURL`. This is the bulk source for `authorLinkDB`.

**Called by:** `authorLinkDB` (internal)
**Used for:** Auto-generating Wikipedia author links

### `cleanAuthorsRegexps :: [(String, String)]`

Regex-based rewrite rules for normalizing author name formatting. Handles:
- Missing spaces after commas: `"Foo Bar,Quuz" → "Foo Bar, Quuz"`
- Inverted name order: `"Smith, J." → "J. Smith"`
- Collapsed initials: `"A.B. Smith" → "A. B. Smith"`

**Called by:** `Metadata.Author.cleanAuthors`
**Used for:** Pre-canonicalization cleanup

### `cleanAuthorsFixedRewrites :: [(String, String)]`

Fixed string replacements for author cleanup. Removes:
- Academic titles: `"Dr "`, `" PhD"`, `" MD"`, `"Prof "`
- Affiliation markers: `" MA,"`, `" MSc,"`
- Formatting noise: `" et al"`, `"Jr."` → `"Junior"`

**Called by:** `Metadata.Author.cleanAuthors`
**Used for:** Stripping academic credentials from names

### `authorLinkBlacklist :: [T.Text]`

Names that should never be auto-linked due to ambiguity or being too generic. Includes single letters (`"a"` through `"z"`), years (`"1922"`), and common ambiguous surnames (`"Anonymous"`, `"Qi"`, `"Rau"`).

**Called by:** `Metadata.Author.authorPrioritize`
**Used for:** Filtering link candidates

### `authorWhitelist :: [String]`

Author names that look like errors but are legitimate. Used by `LinkMetadata.readLinkMetadataAndCheck` to suppress false-positive warnings for names like `"K. U."` or `"0xType"`.

**Called by:** `LinkMetadata.readLinkMetadataAndCheck`
**Used for:** Suppressing validation warnings

### `authorCollapseTestCases :: [(String, [Inline])]`

Test cases for the author list collapse/display logic. Verifies that long author lists get properly wrapped in collapsible `<span class="collapse">` elements.

**Called by:** `Test.hs`
**Used for:** Unit testing

### `extractTwitterUsernameTestSuite :: [(String, String)]`

Test cases for Twitter/X URL → username extraction.

**Called by:** `Test.hs`
**Used for:** Unit testing

---

## Internal Architecture

### Data Flow

```
Raw author string from annotation
    ↓
cleanAuthors (regex + fixed rewrites)
    ↓
authorsCanonicalize (lookup in authorDB)
    ↓
linkify (lookup in authorLinkDB)
    ↓
Pandoc Inline with optional Link
```

### The Two-Database Pattern

Canonicalization uses a merged database (`authorDB` in `Metadata.Author`):
1. `canonicals` — hand-written variant → canonical mappings
2. Generated abbreviations from `canonicalsWithInitials` via `name2Abbreviations`

These must be disjoint; overlaps trigger a runtime error to catch semantic conflicts.

### Disambiguation Syntax

Author names can include `#anchor` suffixes for disambiguation:
- `"George Washington#SS"` links to the ship, not the president
- `"John Smith#genetics"` vs `"John Smith#VC"` for same-name authors

The `#` suffix is stripped before display but used for link lookup.

---

## Key Patterns

### Wikipedia as Default

The bulk of author links come from Wikipedia. The `authorWpLinkDB` list contains names known to have Wikipedia articles; these are converted to URLs via `toWikipediaEnURL`. Non-Wikipedia URLs in `authorLinkDB` override these defaults.

### Credential Stripping

Academic author names often include degrees and affiliations that clutter display:
```
"John Smith PhD, MD, FRCP" → "John Smith"
```
The `cleanAuthorsFixedRewrites` list removes 60+ such suffixes.

### Initial Expansion

Rather than manually listing every abbreviation of "Ingrid Sigfrid Melle", the `canonicalsWithInitials` mechanism generates:
- "I. Melle"
- "I. S. Melle"
- "I.S. Melle"
- "Ingrid Melle"
- "Ingrid S. Melle"
- etc.

This reduces maintenance burden for frequently-cited authors.

### Frequency-Based Prioritization

The `authorPrioritize` function (in `Metadata.Author`) identifies high-frequency unlinkified authors, weighted by backlink count. This guides manual effort toward the most impactful additions.

---

## Configuration

All configuration is inline in this file as Haskell data literals. To add:

**New canonical mapping:**
```haskell
canonicals = M.fromList [
  ...
  , ("twitterhandle", "Real Name")
  ...
]
```

**New author link:**
```haskell
authorLinkDB = M.fromList $
  zip authorWpLinkDB (map toWikipediaEnURL authorWpLinkDB) ++
  [ ...
  , ("Author Name", "https://homepage.example/")
  ...
  ]
```

**New auto-abbreviated name:**
```haskell
canonicalsWithInitials = [
  ...
  , "First Middle Last"
  ...
]
```

---

## Integration Points

### Consumed By

| Module | What it uses |
|--------|-------------|
| `Metadata.Author` | All exports (canonicalization, linking logic) |
| `LinkMetadata` | `authorLinkDB`, `authorWhitelist` (validation) |
| `link-prioritize.hs` | `authorLinkDB` (finding unlinked authors) |
| `Test.hs` | Test suites (validation) |

### Test Validation

`Test.hs` validates:
- All map keys are unique
- No canonicalization loops (A → B → A)
- All regexes are syntactically valid
- All `authorLinkDB` values are valid URLs

---

## See Also

- [Metadata/Author.hs](/backend/metadata-author-hs) - Processing logic that consumes this config
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation system using author data
- [GTX.hs](/backend/gtx-hs) - Where canonicalized authors are stored
- [Annotation.hs](/backend/annotation-hs) - Calls cleanAuthors during scraping
- [Config/Metadata/Format.hs](/backend/config-metadata-format-hs) - Companion format configuration
- [Config/Metadata/Title.hs](/backend/config-metadata-title-hs) - Companion title configuration
