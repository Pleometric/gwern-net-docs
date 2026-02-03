
# link-prioritize.hs

**Path:** `build/link-prioritize.hs` | **Language:** Haskell | **Lines:** ~94

> Ranks unannotated links by usage frequency to prioritize manual annotation work

---

## Overview

link-prioritize.hs is a CLI utility that helps prioritize which links should receive manual annotations. The problem it solves: gwern.net has thousands of external links, and writing quality annotations is time-consuming. This tool identifies which unannotated links appear most frequently across the site, so annotation effort can focus on high-impact URLs.

The tool ignores stdin and instead counts URLs from the backlinks database, then cross-references them against both the manual annotation database (`*.gtx` files) and auto-generated annotations. A link is considered "annotated" if it has an abstract longer than 100 characters. Links that fail this check are counted by frequency and output in descending order.

The design filters out several categories that shouldn't be prioritized: Wikipedia links (which auto-populate well), author bio URLs (writing full biographies is impractical), and URLs without dots (likely anchors or special syntax).

---

## Public API

### `main :: IO ()`

Entry point. Reads a required numeric argument for limiting output, loads the metadata and backlinks databases, filters/ranks URLs, and prints results.

**Called by:** Shell invocation
**Calls:** `readLinkMetadata`, `readBacklinksDB`, `isAnnotated`

---

### `isAnnotated :: Metadata -> String -> Bool`

Checks if a URL has a meaningful annotation (abstract \>100 characters).

```haskell
isAnnotated :: Metadata -> String -> Bool
isAnnotated md target = ...
```

**Called by:** `main` (in filter predicate)
**Calls:** `M.lookup`, `replace`

Normalizes `https://gwern.net/` URLs to `/` paths before lookup. Returns `True` only if the annotation exists AND has an abstract exceeding 100 characters—short stubs don't count.

---

## Internal Architecture

### Data Flow

```
readBacklinksDB → frequency count → filter unannotated → sort → output
```

### Key Data Structures

**Backlinks DB** (`bdb`): Maps pages to their outbound links. The tool flattens this to get `[(url, count)]` pairs representing how many times each URL appears site-wide.

**Metadata DB** (`db`): The merged annotation database from `readLinkMetadata`. Used to check if URLs already have sufficient annotations.

**Author URLs** (`authorURLs`): List of URLs used as author bio links, extracted from `Config.Metadata.Author.authorLinkDB`. These are filtered out since writing comprehensive author biographies is out of scope.

### Processing Steps

1. Parse required `printN` argument (no default)
2. Load metadata DB, sanity-check it has \>1000 entries
3. Load backlinks DB
4. Extract author URLs to filter
5. Flatten backlinks to `(url, count)` pairs
6. Filter out:
   - Already-annotated URLs (\>100 char abstract)
   - URLs without dots (anchors, special syntax)
   - Wikipedia URLs (auto-annotate well)
   - Author bio URLs
7. Sort by count descending
8. Print top N results

---

## Key Patterns

### Threshold-Based Annotation Detection

The 100-character threshold for "is annotated" is a pragmatic heuristic. Very short abstracts (stubs, placeholders) don't provide reader value, so the tool treats them as unannotated for prioritization purposes.

### Backlinks as Proxy for Importance

Rather than counting raw link occurrences in source files, the tool uses the pre-computed backlinks database. This is more accurate since it reflects the actual rendered site structure after transclusions and includes are resolved.

### Defensive Database Check

```haskell
when (M.size db < 1000) $ error "..."
```

Guards against running with a corrupted or partially-loaded database, which would produce misleading results.

---

## Configuration

| Setting | Location | Effect |
|---------|----------|--------|
| Print limit | CLI arg 1 | Max URLs to output (required) |
| Annotation threshold | Hardcoded (100) | Min abstract length to count as "annotated" |

---

## Integration Points

### Inputs

- **stdin**: Ignored
- **Metadata DB**: Via `readLinkMetadata` from [LinkMetadata](link-metadata-hs)
- **Backlinks DB**: Via `readBacklinksDB` from LinkBacklink
- **Author DB**: `Config.Metadata.Author.authorLinkDB`

### Outputs

- **stdout**: Frequency-sorted list in format `COUNT URL`

### Typical Pipeline

```bash
# Top 50 links based on backlinks DB
link-prioritize.hs 50
```

---

## See Also

- [link-extractor.hs](/backend/link-extractor-hs) - Extracts URLs from Markdown files (typical input source)
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database manager
- [Annotation.hs](/backend/annotation-hs) - URL-to-scraper dispatcher for auto-annotations
- [link-suggester.hs](/backend/link-suggester-hs) - Related tool for link suggestion generation
- [LinkID.hs](/backend/link-id-hs) - Citation ID generation used in backlinks
- [link-titler.hs](/backend/link-titler-hs) - Adds titles to links that need annotation
