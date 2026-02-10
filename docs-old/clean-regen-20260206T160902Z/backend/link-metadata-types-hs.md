
# LinkMetadataTypes.hs

**Path:** `build/LinkMetadataTypes.hs` | **Language:** Haskell | **Lines:** ~40

> Core type definitions for the annotation and link metadata system

---

## Overview

LinkMetadataTypes.hs defines the fundamental data types used throughout gwern.net's annotation system. These types represent the structured metadata attached to links—titles, authors, dates, tags, abstracts, and more—that power the site's popup previews and link enrichment features.

This module is a dependency leaf: it imports only standard library modules plus a small utility module, making it safe to import anywhere without circular dependency issues. Nearly every Haskell module in the build system imports these types, making it the canonical source of truth for metadata representation.

The design prioritizes simplicity and compatibility with the GTX file format. Rather than using Haskell records with named fields, metadata is represented as tuples matching the line-by-line structure of GTX files, enabling straightforward serialization and parsing.

---

## Public API

### Type Aliases

### `Path`

```haskell
type Path = String
```

A URL or file path serving as the unique identifier for a metadata entry. Can be:
- Absolute URLs (`https://arxiv.org/abs/...`)
- Local paths (`/doc/ai/...`)
- Local paths with anchors (`/doc/ai/scaling#section-id`)

**Used by:** All modules that work with annotations

---

### `MetadataItem`

```haskell
type MetadataItem = (String, String, String, String, [(String,String)], [String], String)
                  -- (Title,  Author,  Date,   DateCreated, KeyValues,     Tags,     Abstract)
```

A 7-tuple containing all annotation fields:

| Position | Field | Description |
|----------|-------|-------------|
| 1 | Title | Document title (HTML allowed) |
| 2 | Author | Comma-separated author list |
| 3 | Date | Publication date (YYYY-MM-DD or partial) |
| 4 | DateCreated | When the metadata entry was created |
| 5 | KeyValues | Association list for DOI, ID, and other metadata |
| 6 | Tags | List of tag strings (must match `doc/*` directories) |
| 7 | Abstract | HTML content for popup display |

**Used by:** GTX.hs, LinkMetadata.hs, Annotation.hs, and 25+ other modules

---

### `Metadata`

```haskell
type Metadata = M.Map Path MetadataItem
```

The primary in-memory representation of the annotation database. Keyed by Path for O(log n) lookup.

**Used by:** LinkMetadata.readLinkMetadata, hakyll.hs

---

### `MetadataList`

```haskell
type MetadataList = [(Path, MetadataItem)]
```

List representation used during GTX parsing/writing and for streaming operations.

**Used by:** GTX.hs (readGTX, writeGTX)

---

### `ArchiveMetadataItem`

```haskell
type ArchiveMetadataItem = Either
  Integer           -- Age: first-seen date as ModifiedJulianDay
  (Maybe FilePath)  -- Archive path (Nothing = dead/skip, Just path = archived)
```

Tracks external URL archival status:
- `Left n`: URL first seen on day `n`, not yet archived
- `Right Nothing`: URL is dead or should be skipped
- `Right (Just path)`: Successfully archived to local path

**Used by:** LinkArchive.hs

---

### `ArchiveMetadata`

```haskell
type ArchiveMetadata = M.Map Path ArchiveMetadataItem
```

Map from external URLs to their archive status.

---

### `ArchiveMetadataList`

```haskell
type ArchiveMetadataList = [(Path, ArchiveMetadataItem)]
```

List form for serialization.

---

### `SizeDB`

```haskell
type SizeDB = M.Map FilePath (Int, Int)
                           -- (bytes, percentile)
```

Maps file paths to their size in bytes and a percentile rank within their category. Three categories are tracked separately:
- Essays (Markdown files)
- Local files (PDFs, HTML, etc.)
- Archived external URLs

**Used by:** LinkMetadata.annotationSizeDB, LinkMetadata.addSizeToLinks

---

### `Failure`

```haskell
data Failure = Temporary | Permanent deriving Show
```

Distinguishes between transient and permanent annotation lookup failures. Temporary failures may be retried; permanent failures are cached to avoid repeated lookups.

**Used by:** Annotation.hs (linkDispatcher)

---

## Utility Functions

### `isPagePath :: T.Text -> Bool`

Determines if a path represents a local essay (as opposed to a static file). Essays are local paths that:
- Start with `/`
- Are not in `/static/`
- Have no file extension

This exploits the fact that Markdown files have their `.md` extension stripped during compilation.

**Called by:** LinkMetadata.addPageLink, popups.js (client-side equivalent)

---

### `hasHTMLSubstitute :: FilePath -> Bool`

Returns `True` if the file has a high-quality HTML rendering suitable for in-browser viewing. Covers formats converted by LibreOffice:
- `.csv`, `.doc`, `.docx`, `.ods`, `.xls`, `.xlsx`

**Called by:** Transclusion logic for file embedding

---

## Internal Architecture

The type design follows a simple pattern: everything is either a type alias or a trivial sum type. No newtypes, no complex records with field accessors. This matches the GTX file format philosophy of minimal ceremony.

```
GTX File (text)
     ↓ parse
MetadataList [(Path, MetadataItem)]
     ↓ M.fromList
Metadata (Map Path MetadataItem)
     ↓ M.toList
MetadataList
     ↓ serialize
GTX File (text)
```

The tuple-based `MetadataItem` trades named access for positional access. While this makes field access more error-prone, it:
1. Matches the line-based GTX format directly
2. Avoids record update syntax overhead
3. Works well with pattern matching in Haskell

---

## Key Patterns

### Either for Archival State

The `ArchiveMetadataItem` type uses `Either` to encode a state machine:

```haskell
type ArchiveMetadataItem = Either Integer (Maybe FilePath)
```

States:
1. `Left julianDay` → Seen but not archived (age tracking)
2. `Right Nothing` → Failed/dead (don't retry)
3. `Right (Just path)` → Successfully archived

This encoding is compact and pattern-matches cleanly.

### Percentile Normalization in SizeDB

Sizes are stored with pre-computed percentiles to enable UI display ("this PDF is larger than 90% of PDFs on the site"). Percentiles are computed separately per category to give meaningful comparisons.

---

## Configuration

No runtime configuration. Type definitions are compile-time constants.

---

## Integration Points

### Imports

- `Data.Map.Strict` for Map type
- `Data.Text` for Text operations
- `Utils` for `isLocal`, `anyInfix`, `delete`

### Imported By

This module is imported by nearly every Haskell file in the build system:
- **GTX.hs**: Serialization/deserialization
- **LinkMetadata.hs**: Database management
- **Annotation.hs**: Scraper dispatch
- **hakyll.hs**: Site generation
- **LinkArchive.hs**: URL archiving
- **GenerateSimilar.hs**: Embedding-based recommendations
- Plus 20+ other modules

### Shared State

Types defined here are used for:
- `metadata/full.gtx` (complete annotations)
- `metadata/half.gtx` (partial annotations)
- `metadata/auto.gtx` (auto-generated annotations)
- `metadata/archive.hs` (archival status database)

---

## See Also

- [GTX.hs](/backend/gtx-hs) - GTX file format parser/writer
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database manager
- [Annotation.hs](/backend/annotation-hs) - URL-to-scraper dispatcher
- [Metadata/Format.hs](/backend/metadata-format-hs) - String cleaning utilities
- [Metadata/Author.hs](/backend/metadata-author-hs) - Author name processing
- [annotations.js](/frontend/annotations-js) - Frontend annotation data layer

---
