
# Config.LinkSuggester

**Path:** `build/Config/LinkSuggester.hs` | **Language:** Haskell | **Lines:** ~1,260

> Configuration for link suggestion filtering and whitelisting in Emacs integration

---

## Overview

Config.LinkSuggester defines the filtering rules and whitelists that govern the link suggestion system used during annotation writing in Emacs. When writing annotations, the system can suggest relevant URLs based on anchor text—this module determines which suggestions are valid and which should be suppressed.

The module addresses two complementary problems: filtering out noise (URLs and anchor texts that shouldn't become links) and preserving signal (specific URL/anchor pairs that would be incorrectly filtered by heuristics). This results in a cleaner, more useful suggestion experience during the annotation workflow.

The extensive whitelist (`whiteList`) serves as a curated database of important links with their canonical anchor texts, effectively encoding editorial knowledge about which terms should link where on gwern.net.

---

## Public API

### `hitsMinimum :: Int`

Minimum number of occurrences required before suggesting a link (default: 4).

**Called by:** Link suggestion generation in formatting phase
**Calls:** N/A (constant)

### `anchorLengthMaximum :: Int`

Maximum character length for anchor text (default: 80).

**Called by:** `filterAnchors`
**Calls:** N/A (constant)

### `filterURLs :: T.Text -> Bool`

Returns `True` if a URL should be excluded from suggestions.

**Called by:** Link suggestion filtering pipeline
**Calls:** `anyPrefixT`, `T.isPrefixOf`, `T.isSuffixOf`

### `filterAnchors :: T.Text -> Bool`

Returns `True` if an anchor text should be excluded from suggestions.

**Called by:** Link suggestion filtering pipeline
**Calls:** `(=~)` regex matching, `anyInfixT`, `anyPrefixT`, `elem`

### `whiteListDB :: M.Map T.Text [T.Text]`

Map from URLs to lists of acceptable anchor texts, used to override filtering heuristics.

**Called by:** Link suggestion pipeline
**Calls:** `M.fromList`, `filter`, `isURI`

---

## Internal Architecture

### URL Filtering Logic

`filterURLs` blacklists URLs matching these patterns:

| Pattern | Reason |
|---------|--------|
| `$`, `₿`, `#` prefixes | Non-URL strings |
| `/static/img/`, `/metadata/` | Asset paths |
| `/newsletter/20` | Newsletter archives |
| `dropbox.com`, `https://www.harney.com` (prefix) | Specific domains |
| `https://www.reuters.com/idUSKCN1071KW` | Specific URL blacklist |
| `/doc/*/index`, `https://gwern.net/doc/*/index` | Index pages |

### Anchor Filtering Logic

`filterAnchors` rejects anchors matching:

1. **Length check**: Over 80 characters
2. **Regex patterns**: Numbers, page references, figure/table citations, chapter references
3. **Infix patterns**: `$`, `%`, `[`, `]`
4. **Prefix patterns**: `(`, `.`, `Wikipedia link about`
5. **Explicit blacklist**: ~800 strings in `badAnchorStrings`

The regex patterns cover:
- Pure numbers: `[0-9]+[kmgbt]?`
- Ranges: `[0-9]+[.,;–-][0-9]+`
- Page refs: `pg[0-9]+`, `p.[0-9]+`
- Hex suffixes: `[0-9]+[a-f]`
- Year phrases: `in [12][0-9][0-9][0-9]`
- Figures/tables: `[Ff]igure S?[0-9]+[a-f]?`, `[Tt]able S?[0-9]+[a-f]?`
- Chapters: `[Cc]hapter [0-9]+`

### Whitelist Structure

The whitelist is a list of `(URL, [anchors])` tuples covering:

- **Internal gwern.net paths**: `/crop#hands`, `/spaced-repetition`, `/twdne`
- **Wikipedia articles**: Hundreds of entries for common concepts
- **Academic papers**: DOIs and PDF paths with citation formats
- **External resources**: ArXiv, bioRxiv, specialized websites

---

## Key Patterns

### Negative Filtering vs Positive Whitelisting

The dual approach handles the spectrum of suggestion quality:
- Most suggestions need heuristic filtering (too noisy by default)
- Some filtered suggestions are actually valuable (whitelist rescues them)

### Unicode in Blacklist

`badAnchorStrings` includes Unicode characters:
- `₿` (Bitcoin symbol, U+20BF)
- Various Unicode dashes and multiplication signs
- Japanese text (anime-related terms)

### Whitelist Validation

`whiteListDB` filters the raw whitelist to ensure:
- Non-empty keys
- Keys that are either internal paths (`/`) or valid URIs

```haskell
filter (\(k,_) -> (k /= "") && (T.head k == '/' || isURI (T.unpack k)))
```

---

## Configuration

### Tuning Parameters

| Parameter | Value | Effect |
|-----------|-------|--------|
| `hitsMinimum` | 4 | Rare links won't be suggested |
| `anchorLengthMaximum` | 80 | Long phrases excluded |

### Extending the Whitelist

To add a new whitelist entry:

```haskell
, ("https://example.com/page", ["Anchor 1", "Anchor 2"])
```

The URL becomes the key, and any of the anchor texts will match.

### Adding to Blacklists

For URL filtering, add to the prefix/exact lists in `filterURLs`.
For anchor filtering, add to `badAnchorStrings` or extend the regex patterns.

---

## Integration Points

### Emacs Integration

The link suggester feeds into Emacs during annotation writing. The generated `linkSuggests-deleted.hs` database (mentioned in comments) captures suggestions that were filtered, allowing review of false negatives.

### Utils Module

Imports `anyInfixT` and `anyPrefixT` from Utils for efficient text matching.

### Build Pipeline

This is a pure configuration module—no side effects. It's imported by the actual link suggestion generation code during the build/formatting phase.

---

## See Also

- [link-suggester.hs](/backend/link-suggester-hs) - Main script that consumes this configuration
- [link-extractor.hs](/backend/link-extractor-hs) - Extracts URLs for suggestion processing
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database for URL metadata
- [LinkID.hs](/backend/link-id-hs) - Citation ID generation for link deduplication
- [Annotation.hs](/backend/annotation-hs) - Uses link suggestions during annotation processing
- [Typography.hs](/backend/typography-hs) - Text transformation that may interact with anchor formatting
