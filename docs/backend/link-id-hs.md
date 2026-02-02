
# LinkID.hs

**Path:** `build/LinkID.hs` | **Language:** Haskell | **Lines:** ~230

> Citation-style ID generation for links and annotations

---

## Overview

LinkID.hs generates unique identifiers for links and annotations on gwern.net. These IDs serve two purposes: creating human-readable citation-style anchors (like `#foo-2020` or `#bar-et-al-2023`) for scholarly references, and providing stable `/ref/` URLs for linking directly to any annotation.

The module implements a two-tier ID strategy. When sufficient metadata exists (author + date), it generates academic citation-style IDs following standard conventions: single author (`smith-2020`), two authors (`smith-jones-2020`), or three+ authors (`smith-et-al-2020`). When metadata is insufficient, it falls back to a 9-character SHA-1 hash ID prefixed with underscore (`_Mnw_2ofO`). This ensures every URL can always have a valid HTML anchor ID.

The ID system integrates tightly with the annotation database and backlink system. IDs are sharded into JSON files by first character for efficient client-side lookups, enabling the `/ref/` URL scheme where readers can link directly to annotations without knowing the underlying URL.

---

## Public API

### `generateID(md, url, author, date) → Text`

Core ID generation function. Produces a citation-style ID or hash fallback.

**Called by:** generateDirectory.hs, generateBacklinks.hs, LinkMetadata.hs
**Calls:** `authorsToCite`, `citeToID`, `url2ID`, Config.LinkID.linkIDOverrides

### `metadataItem2ID(md, path, item) → Text`

Convenience wrapper that extracts author/date from a MetadataItem.

**Called by:** Blog.hs, generateLinkBibliography.hs, `id2URLdb`
**Calls:** `generateID`

### `authorsToCite(url, author, date) → String`

Formats authors into citation string ("Foo 2020", "Foo & Bar 2020", "Foo et al 2020").

**Called by:** annotation-dump.hs, generateDirectory.hs, link-titler.hs, `generateID`
**Calls:** Utils.sedMany, Config.Misc.currentYear

### `url2ID(url) → Text`

SHA-1 hash fallback for URLs without metadata. Returns 9-char `_XXXXXXXX` format.

**Called by:** `generateID`, Test.hs
**Calls:** Crypto.Hash.SHA1, Data.ByteString.Base64.URL

### `isValidID(s) → Bool`

Validates whether a string is a legal ID (hash format or structured format).

**Called by:** Test.hs, GTX.hs, `generateURL`
**Calls:** (pure validation logic)

### `getDisambiguatedPairs(md) → [(Path, String)]`

Finds duplicate IDs in metadata and generates `-1`, `-2` suffixes for disambiguation.

**Called by:** LinkMetadata.hs
**Calls:** `metadataItem2ID`

### `writeOutID2URLdb(md) → IO ()`

Serializes ID→URL mappings to sharded JSON files for client-side lookup.

**Called by:** hakyll.hs
**Calls:** `id2URLdb`, `shardByCharPrefix`, Utils.writeUpdatedFile

### `generateURL(md, path, item) → String`

Returns the `/ref/` URL for an annotation (e.g., `https://gwern.net/ref/foo-2020`).

**Called by:** annotation-dump.hs
**Calls:** `generateID`, `isValidID`

---

## Internal Architecture

### ID Generation Flow

```
URL + Author + Date
        │
        ▼
┌───────────────────┐
│ Check overrides   │──found──▶ Return override
│ (Config.LinkID)   │
└───────────────────┘
        │ not found
        ▼
┌───────────────────┐
│ Check annotation  │──has id──▶ Return custom id
│ key-value pairs   │
└───────────────────┘
        │ no custom id
        ▼
┌───────────────────┐
│ Is /blog/ post?   │──yes──▶ "gwern-YYYY-slug"
└───────────────────┘
        │ no
        ▼
┌───────────────────┐
│ Is gwern.net page │──yes──▶ "gwern-pagename"
│ by Gwern?         │
└───────────────────┘
        │ no
        ▼
┌───────────────────┐
│ Has author+date?  │──no──▶ url2ID hash fallback
└───────────────────┘
        │ yes
        ▼
┌───────────────────┐
│ authorsToCite     │──▶ citeToID ──▶ "foo-et-al-2020"
└───────────────────┘
```

### Hash ID Format

For URLs without metadata, `url2ID` generates a 9-character ID:
- First char: always `_` (distinguishes hashes from citation IDs)
- Next 8 chars: URL-safe Base64 encoding of truncated SHA-1

```haskell
url2ID url = "_" <> take 8 (base64url (take 6 (sha1 url)))
```

The underscore prefix is critical: citation-style IDs never start with underscore, so the two formats are always distinguishable.

### Citation Suffix Handling

For duplicate documents (e.g., `/doc/.../1975-johnson.pdf` and `/doc/.../1975-johnson-2.pdf`), the module appends alphabetic suffixes:

```haskell
-- 1975-johnson.pdf   → "johnson-1975"
-- 1975-johnson-2.pdf → "johnson-1975a"
-- 1975-johnson-3.pdf → "johnson-1975b"
```

The suffix generator produces infinite alphabetic sequences: `a..z, aa..zz, aaa...`

---

## Key Patterns

### Affiliation/Annotation Stripping

Author fields may contain affiliations that break citation formatting:

```haskell
-- "Stephan Foo (Atlas Obscura)" → "Foo"
-- "John Bar [Ian Stewart]"      → "Bar"
authors = map (takeWhile (/= '#')) $
          split ", " $
          sedMany [(" \\([A-Za-z ]+\\)", ""),
                   (" \\[[A-Za-z ]+\\]", "")] author
```

### Page-Hash Extensions

When the same PDF is linked with different `#page=N` anchors, those generate distinct IDs:

```haskell
-- /doc/2013-kurzban.pdf         → "kurzban-2013"
-- /doc/2013-kurzban.pdf#page=14 → "kurzban-2013-page-14"
```

### ID Blacklist

A hardcoded blacklist prevents accidentally generating IDs that collide with reserved system strings:

```haskell
blacklistIDs i = if i `notElem`
    ["9jvwHKDX","CgaXg9w7",...] -- interwiki/inflation markers
    then i
    else error "forbidden ID generated!"
```

### Sharded JSON Output

The `id2URLdb` output is sharded by first character (64 shards for URL-safe Base64 alphabet) for efficient client-side lookups:

```
metadata/annotation/id/a.json  -- IDs starting with 'a' or '_a'
metadata/annotation/id/b.json  -- IDs starting with 'b' or '_b'
...
metadata/annotation/id/all.json -- All IDs (URL→ID, reversed)
```

---

## Configuration

### Config.LinkID.hs

**`linkIDOverrides :: [(String, Text)]`**

Manual overrides for tricky URLs where automatic ID generation fails:

```haskell
linkIDOverrides = [
  ("/gpt-2-music", "gwern-presser-2019-music")
]
```

**`affiliationAnchors :: [String]`**

List of organization anchors used in annotation processing (not directly in LinkID.hs but exported from Config.LinkID):

```haskell
affiliationAnchors = ["ai21", "adobe", "alibaba", ...]
```

### Annotation Key-Value Override

Individual annotations can specify custom IDs via the `id` key-value pair, bypassing the algorithm entirely.

---

## Integration Points

### Consumers

| Module | Usage |
|--------|-------|
| generateBacklinks.hs | IDs for backlink anchors |
| generateDirectory.hs | IDs in directory listings |
| generateLinkBibliography.hs | IDs for bibliography entries |
| Blog.hs | IDs for blog post links |
| hakyll.hs | Triggers JSON database generation |
| annotation-dump.hs | `/ref/` URL generation |
| link-titler.hs | Citation text formatting |
| GTX.hs | ID validation |
| Test.hs | Unit tests for ID generation |

### Outputs

- `metadata/annotation/id/*.json` - 64 sharded ID→URL maps + `all.json`
- HTML anchor IDs on links throughout the site
- `/ref/` URLs exposed in popup title bars

### Dependencies

- **LinkMetadataTypes** - Metadata, MetadataItem, Path types
- **Utils** - String manipulation helpers
- **Config.Misc** - currentYear, cd, authorL constants
- **Config.LinkID** - Override tables

---

## See Also

- [Config.LinkID](/backend/config-link-id-hs) - ID generation overrides and affiliation anchor lists
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database that provides author/date metadata
- [Annotation.hs](/backend/annotation-hs) - Scraper system that populates metadata
- [hakyll.hs](/backend/hakyll-hs) - Triggers ID database generation during builds
- [link-suggester.hs](/backend/link-suggester-hs) - Uses IDs for link suggestion deduplication
- [link-extractor.hs](/backend/link-extractor-hs) - Extracts URLs that get assigned IDs
