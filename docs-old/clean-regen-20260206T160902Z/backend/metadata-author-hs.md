
# Metadata/Author.hs

**Path:** `build/Metadata/Author.hs` | **Language:** Haskell | **Lines:** ~290

> Author name canonicalization, linking, and display formatting

**Config:** `build/Config/Metadata/Author.hs` | **Lines:** ~3,360

---

## Overview

This module handles author metadata normalization and hyperlinking. The core problem: author names appear in many inconsistent forms ("E. Yudkowsky", "Eliezer S. Yudkowsky", "esyudkowsky") but should display consistently and link to the same biography page.

The solution has four layers: (1) clean up formatting noise (titles like "Dr", degree suffixes like "PhD", inconsistent spacing), (2) canonicalize variant spellings to a single form, (3) auto-link author names to their homepages/Wikipedia pages, and (4) collapse long author lists for display. Authors can override auto-linking by embedding HTML links directly in the annotation's author field.

The canonicalization database lives in `Config/Metadata/Author.hs`, a ~3,300 line file containing ~770 explicit rewrites (Twitter handles, name variants), ~150 names with auto-generated initial variants, and ~2,400 author→URL mappings dominated by Wikipedia links.

---

## Public API

### `cleanAuthors :: String -> String`

Normalizes author string formatting: fixes spacing around initials, removes academic titles/degrees, standardizes separators.

```haskell
-- Applies regex then fixed-string rewrites from config
cleanAuthors = trim . replaceMany CA.cleanAuthorsFixedRewrites . sedMany CA.cleanAuthorsRegexps
```

**Called by:** `GTX.hs`, `Annotation/PDF.hs`, `Annotation/Arxiv.hs`, `Annotation/Biorxiv.hs`, `Annotation/OpenReview.hs`, `Annotation/Gwernnet.hs`
**Calls:** `Utils.replaceMany`, `Utils.sedMany`, config lists

---

### `authorsCanonicalize :: String -> String`

Rewrites comma-separated author list to canonical forms ("esyudkowsky" → "Eliezer Yudkowsky").

```haskell
authorsCanonicalize = intercalate ", " . map authorCanonicalize . split ", "
-- where authorCanonicalize looks up in authorDB
```

**Called by:** `GTX.hs` (when processing annotations)
**Calls:** `authorDB` lookup

---

### `authorCollapse :: String -> [Inline]`

Converts author string to Pandoc Inlines with collapsible overflow. Shows first 3 authors; hides the rest in a `<span class="collapse">`.

```haskell
-- Returns: [Space, Span ("", ["author"], []) [...authors...]]
-- If >4 authors, wraps tail in Span ("",["collapse"],[])
```

**Called by:** `LinkMetadata.hs`, `generateDirectory.hs`
**Calls:** `linkify`

---

### `authorsLinkify :: T.Text -> [Inline]`

Splits author text on ", " and wraps each in a hyperlink (if in `authorLinkDB`) or plain Str.

**Called by:** `generateBacklinks.hs`
**Calls:** `linkify`

---

### `extractTwitterUsername :: String -> String`

Pulls username from X.com URLs. Handles both profile URLs and status URLs.

```haskell
extractTwitterUsername "https://x.com/grantslatton/status/123" → "grantslatton"
extractTwitterUsername "https://x.com/grantslatton" → "grantslatton"
```

**Called by:** `Annotation.hs`, `generateDirectory.hs`

---

### `authorsInitialize :: String -> [Inline]`

Abbreviates first names to initials for compact display. Preserves full name in title attribute for hover.

```haskell
authorsInitialize "John Smith, Jane Doe"
→ [Span ("", [], [("title", "John Smith")]) [Str "J. Smith"],
   Str ", ",
   Span ("", [], [("title", "Jane Doe")]) [Str "J. Doe"]]
```

**Note:** Does not handle Unicode names due to regex library limitations.

---

### `name2Abbreviations :: String -> [(String, String)]`

Generates all plausible abbreviated forms of a full name for automatic aliasing.

```haskell
name2Abbreviations "Ken J. Ong"
→ [("K J. Ong", "Ken J. Ong"), ("K. J. Ong", "Ken J. Ong"),
   ("K. Ong", "Ken J. Ong"), ("Ken Ong", "Ken J. Ong"), ...]
```

**Called by:** `authorDB` construction (combines with `canonicalsWithInitials` list)

---

### `isAuthor :: String -> Bool`

Returns True if name is in the canonicalization DB or link DB.

**Called by:** `authorsUnknown`

---

### `authorsUnknownPrint :: String -> IO ()`

Development helper: prints unknown authors and opens Wikipedia searches for possible matches.

**Called by:** `Annotation.hs` (at annotation processing time)

---

### `authorBrowseTopN :: Metadata -> Int -> IO ()`

Development helper: finds top N authors by weighted frequency (annotations × backlinks) without URLs, opens search queries in browser.

---

## Internal Architecture

### Data Flow

```
Raw author string
    ↓ cleanAuthors (formatting)
    ↓ authorsCanonicalize (name variants)
    ↓ authorCollapse/authorsLinkify (display + links)
    ↓ Pandoc Inlines
```

### Core Data Structures

**`authorDB :: M.Map String String`**
Combined canonicalization map. Built at compile time from:
1. `CA.canonicals` - explicit rewrites (~770 entries)
2. Generated abbreviations from `CA.canonicalsWithInitials` (~150 names × ~7 variants each)

**`CA.authorLinkDB :: M.Map T.Text T.Text`**
Author name → URL mapping (~2,400 entries). Wikipedia links dominate, generated from `authorWpLinkDB` list via `toWikipediaEnURL`.

---

## Key Patterns

### Hash Disambiguation

Author names can have `#fragment` suffixes to disambiguate:
- Same person, different context: `"Eliezer Yudkowsky#Twitter"` → `https://x.com/esyudkowsky`
- Different people, same name: `"John Smith#genetics"` vs `"John Smith#venture-capital"`

The `linkify` function strips the hash before display but uses it for URL lookup.

### Automatic Backlinks as Bibliography

Since author names become regular hyperlinks, the backlinks system automatically creates author bibliographies. If "Eliezer Yudkowsky" links to `yudkowsky.net`, the backlinks for that URL show all annotations authored by him.

### Abbreviation Generation

`name2Abbreviations` handles the combinatorial explosion of academic name formats. For "Ken J. Ong" it generates "K. J. Ong", "K J. Ong", "K. Ong", "Ken Ong", etc. This is only applied to names in `canonicalsWithInitials` to avoid noise.

---

## Configuration

All configuration lives in `Config/Metadata/Author.hs`:

### `cleanAuthorsRegexps` / `cleanAuthorsFixedRewrites`

Formatting cleanup rules. Regexes handle patterns like "Foo,Bar" → "Foo, Bar" or "Smith, J." → "J. Smith". Fixed rewrites remove titles ("Dr ", "Prof ") and degrees (" PhD", " M.D.").

### `canonicals :: M.Map String String`

Explicit name rewrites. Contains:
- Twitter handles → real names ("karpathy" → "Andrej Karpathy")
- Name variants ("E. Yudkowsky" → "Eliezer Yudkowsky")
- Pen names ("Student" → "William Sealy Gosset")
- Spelling corrections ("Yann LeCunn" → "Yann LeCun")

### `canonicalsWithInitials :: [String]`

Names to auto-generate abbreviation variants for. Contains ~150 full names like "Kenneth O. Stanley" that will have their initial forms generated.

### `authorLinkDB :: M.Map T.Text T.Text`

URL mappings. Built from:
1. `authorWpLinkDB` - list of names with Wikipedia articles (auto-converted to WP URLs)
2. Override entries for non-WP links (personal sites, Google Scholar, X.com)

### `authorLinkBlacklist :: [T.Text]`

Names to never auto-link (single letters, common words that aren't people).

### `authorWhitelist :: [String]`

Known-unusual names that pass validation despite looking like errors.

---

## Integration Points

### Annotation Processing

`cleanAuthors` and `authorsCanonicalize` are called when processing annotations in `GTX.hs`. The author field is normalized before storage.

### Display Rendering

`authorCollapse` is called when rendering annotations for display (via `LinkMetadata.hs`). Long author lists get collapsible spans.

### Backlinks

`authorsLinkifyAndExtractURLs` extracts the generated author URLs so they can be included in backlink calculations.

### Testing

`Test.hs` imports test functions: `authorCollapseTest`, `cleanAuthorsTest`, and runs validation on all config lists (uniqueness, no loops).

---

## See Also

- [Config/Metadata/Author.hs](/backend/config-metadata-author-hs) - Configuration data (canonicals, author links)
- [LinkMetadata.hs](/backend/link-metadata-hs) - Uses authorCollapse for annotation display
- [Annotation.hs](/backend/annotation-hs) - Calls cleanAuthors during scraping
- [GTX.hs](/backend/gtx-hs) - Where canonicalized authors are stored
- [Metadata/Date.hs](/backend/metadata-date-hs) - Companion date processing module
- [Metadata/Format.hs](/backend/metadata-format-hs) - Companion string cleaning module
- [Metadata/Title.hs](/backend/metadata-title-hs) - Companion title processing module
