
# LinkMetadata.hs

**Path:** `build/LinkMetadata.hs` | **Language:** Haskell | **Lines:** ~944

The brain of gwern.net's annotation system - loads, creates, validates, and caches link metadata that powers popup annotations.

---

## Overview

LinkMetadata.hs orchestrates the annotation system that makes gwern.net's popups work. When you hover over a link and see a popup with title, author, date, abstract, and related links, this module is responsible for:

1. **Loading metadata** from the GTX files (`me.gtx`, `full.gtx`, `half.gtx`, `auto.gtx`)
2. **Creating new annotations** by dispatching to various scrapers (Arxiv, Wikipedia, Twitter, etc.)
3. **Writing annotation fragments** as HTML files that the frontend fetches for popups
4. **Walking Pandoc ASTs** to mark links as annotated and assign unique IDs

The module follows a tiered caching strategy: hand-curated annotations in `me.gtx`/`full.gtx` override semi-automated ones in `half.gtx`, which override fully automatic ones in `auto.gtx`. This left-biased union means you can always override machine-generated annotations with better human-written ones.

A key design decision is the "partial annotation" concept - links without full abstracts can still have useful metadata (tags, backlinks, similar links) that make a popup worthwhile. The module calculates a scoring function to decide whether a partial is worth displaying.

---

## Public API

### Core Types (re-exported from LinkMetadataTypes)

```haskell
type Metadata = M.Map Path MetadataItem
type MetadataItem = (String, String, String, String, [(String,String)], [String], String)
                  -- (Title, Author, Date, DateModified, KeyValues, Tags, Abstract)
type MetadataList = [(Path, MetadataItem)]
type Path = String
```

The `MetadataItem` 7-tuple is the heart of the annotation system. Key-values (`[(String,String)]`) store DOIs, IDs, and custom CSS extensions.

### readLinkMetadata → IO Metadata

Fast loading of all GTX files without validation. Used during normal site builds.

```haskell
readLinkMetadata :: IO Metadata
readLinkMetadata = do
  me   <- readGTXFast "metadata/me.gtx"
  full <- readGTXFast "metadata/full.gtx"
  half <- readGTXFast "metadata/half.gtx"
  auto <- readGTXFast "metadata/auto.gtx"
  let final = M.union (M.fromList me) $ M.union (M.fromList full) $
              M.union (M.fromList half) (M.fromList auto)
  return final
```

**Called by:** hakyll.hs (main build), most annotation consumers
**Calls:** GTX.readGTXFast

### readLinkMetadataSlow → IO Metadata

Slow loading with postprocessing (tag validation, date parsing, author canonicalization).

**Called by:** readLinkMetadataAndCheck, walkAndUpdateLinkMetadataGTX
**Calls:** GTX.readGTXSlow

### readLinkMetadataAndCheck → IO Metadata

Full validation pass that checks for:
- Duplicate URLs/abstracts
- Missing mandatory fields (title, author, abstract in full.gtx)
- Invalid DOIs (missing `/`, wrong format)
- Malformed dates
- Non-existent tags
- Broken local file references
- Unbalanced quotes/brackets in abstracts

**Called by:** Build scripts, updateGwernEntries
**Calls:** readGTXSlow, duplicateAbstracts, rewriteLinkMetadata

### hasAnnotation md block → Block

Walks a Pandoc Block, marking each Link/Image as annotated or not by adding CSS classes:
- `link-annotated` - full annotation available
- `link-annotated-partial` - partial metadata only
- `link-annotated-not` - explicitly no annotation

Also assigns unique link IDs via `generateID`.

```haskell
hasAnnotation :: Metadata -> Block -> Block
hasAnnotation md = walk (hasAnnotationOrIDInline md)
```

**Called by:** hakyll.hs pandocTransform, writeAnnotationFragment
**Calls:** hasAnnotationOrIDInline, addHasAnnotation, addID

### createAnnotations md pandoc → IO ()

Walks a Pandoc document extracting all links, then calls `annotateLink` on each to potentially create new annotations.

```haskell
createAnnotations :: Metadata -> Pandoc -> IO ()
createAnnotations md (Pandoc _ markdown) =
  Par.mapM_ (annotateLink md) $ extractLinksInlines (Pandoc nullMeta markdown)
```

**Called by:** writeAnnotationFragment
**Calls:** annotateLink, extractLinksInlines

### writeAnnotationFragments am md sizes writeOnlyMissing → IO ()

Writes HTML fragment files for each annotation to `metadata/annotation/`. These fragments are what the frontend fetches for popups.

Two-pass approach:
1. First pass: write partials (short/empty abstracts) so they exist for cross-references
2. Second pass: write full annotations

**Called by:** hakyll.hs
**Calls:** writeAnnotationFragment, getBackLinkCheck, getSimilarLinkCheck

### addPageLinkWalk pandoc → Pandoc

Adds `link-page` class to links pointing to gwern.net essays (local paths with no file extension).

```haskell
addPageLinkWalk :: Pandoc -> Pandoc
addPageLinkWalk = walk addPageLink
```

**Called by:** hakyll.hs, writeAnnotationFragment
**Calls:** isPagePath

### annotateLink md inline → IO (Either Failure (Path, MetadataItem))

The main annotation creation entry point. Given a Link inline:
1. Checks if annotation already exists in metadata
2. If not, calls `linkDispatcher` to try fetching from various sources
3. Appends new annotation to `auto.gtx` via `appendLinkMetadata`

Returns `Left Temporary` for transient failures, `Left Permanent` for permanent ones, or `Right (path, item)` on success.

**Called by:** createAnnotations
**Calls:** linkDispatcher, appendLinkMetadata

---

## Internal Architecture

### GTX File Hierarchy

```
metadata/
├── me.gtx      # Personal/about-me annotations (highest priority)
├── full.gtx    # Hand-written complete annotations
├── half.gtx    # Tagged but not fully written
└── auto.gtx    # Machine-generated (lowest priority, auto-cleaned)
```

Priority flows left to right - `me.gtx` overrides everything, `auto.gtx` is overridden by everything.

### Annotation Fragment Generation Flow

```
Metadata DB
    │
    ▼
writeAnnotationFragments
    │
    ├─► For each (path, item):
    │       │
    │       ▼
    │   getBackLinkCheck, getSimilarLinkCheck, getLinkBibLinkCheck
    │       │
    │       ▼
    │   partialScoring (tags + abstract + backlinks + similar)
    │       │
    │       ▼
    │   generateAnnotationBlock (builds Pandoc AST)
    │       │
    │       ▼
    │   Pandoc transforms (linkIcon, linkLive, inflation, etc.)
    │       │
    │       ▼
    │   writeHtml5String → HTML fragment
    │       │
    │       ▼
    │   metadata/annotation/{encoded-path}.html
```

### Link Processing Pipeline

```
Link in document
    │
    ▼
hasAnnotationOrIDInline
    │
    ├─► Check if already annotated (has class)
    │
    ├─► Look up in Metadata map
    │       │
    │       ├─► Nothing/Empty → addID only
    │       │
    │       └─► Just item → addHasAnnotation + addRecentlyChanged + addID
    │
    └─► Return modified Link with classes:
            - link-annotated / link-annotated-partial / (none)
            - link-modified-recently (if modified within current month)
            - Unique ID in anchor attribute
```

### Partial Annotation Scoring

A partial annotation is displayed if any of these are true:
- Has 3+ tags
- Has any abstract text
- Has 2+ backlinks
- Has 7+ similar links

```haskell
let partialScoring = 0 < sum [length (drop 2 ts),
                               length abst,
                               if blN > 1 then 1 else 0,
                               if slN > 6 then 1 else 0]
```

---

## Key Patterns

### Left-Biased Map Union for Priority

```haskell
let final = M.union (M.fromList me) $
            M.union (M.fromList full) $
            M.union (M.fromList half) (M.fromList auto)
```

`M.union` is left-biased, so keys in `me` override `full` override `half` override `auto`. This allows progressive refinement of annotations.

### Parallel Processing with monad-parallel

```haskell
import qualified Control.Monad.Parallel as Par

Par.mapM_ (annotateLink md) $ extractLinksInlines ...
```

Link annotation and file operations use parallel map to speed up builds.

### unsafePerformIO for Caching Decisions

```haskell
| not $ unsafePerformIO $ doesFileExist $ fst $ getAnnotationLink $ T.unpack f = x'
```

The annotation fragment existence check uses `unsafePerformIO` because it's a pure function that needs IO to check file existence. This is safe because file existence is effectively constant during a build.

### URL Canonicalization

```haskell
let target' = replace "https://gwern.net/" "/" target
let target'' = if head target' == '.' then drop 1 target' else target'
```

All gwern.net URLs are normalized to local paths (`/doc/foo.pdf` not `https://gwern.net/doc/foo.pdf`) for consistent lookup.

### Fallback Prefix Matching

```haskell
lookupFallback :: Metadata -> String -> (FilePath, MetadataItem)
```

For URLs like `/doc/2020-paper.pdf#google` where the annotation is stored with the fragment, this falls back to prefix matching to find annotations for fragment-less base URLs.

---

## Configuration

### From Config.Misc

- `minimumAnnotationLength` - threshold for "full" vs "partial" annotation (~characters)
- `currentMonthAgo` - date threshold for "recently modified" highlighting
- `gtxKeyValueKeyNames` - whitelist of valid key names in KV pairs
- `minFileSizeWarning` - when to show file size in MB

### From Config.LinkID

- `affiliationAnchors` - organization anchors for duplicate URL detection

### Hardcoded Thresholds

- Backlinks > 1 for partial scoring
- Similar links > 6 for partial scoring
- File size > 10MB disables prefetch
- URL > 273 chars triggers truncation warning

---

## Integration Points

### Reads From

- `metadata/me.gtx`, `full.gtx`, `half.gtx`, `auto.gtx` - annotation databases
- `metadata/annotation/*.html` - checks for existing fragments
- `doc/` directories - for tag validation

### Writes To

- `metadata/auto.gtx` - new machine-generated annotations (via appendLinkMetadata)
- `metadata/annotation/{encoded-path}.html` - popup HTML fragments
- `_site/metadata/annotation/` - copies for Hakyll sync

### Key Module Dependencies

- **GTX.hs** - GTX format parsing/writing
- **LinkMetadataTypes.hs** - type definitions
- **Annotation.hs** - `linkDispatcher` for fetching new annotations
- **Annotation.Gwernnet.hs** - scraper for local gwern.net pages
- **LinkBacklink.hs** - backlinks, similar links, link bibliography
- **LinkID.hs** - unique ID generation
- **LinkArchive.hs** - local archive URL resolution
- **LinkLive.hs** - live link handling
- **Tags.hs** - tag validation and processing

### Frontend Integration

The HTML fragments written to `metadata/annotation/` are fetched by `popups.js` when users hover over annotated links. The `link-annotated` / `link-annotated-partial` classes tell the JS whether to attempt a fetch.

---

## See Also

- [Annotation.hs](/backend/annotation-hs) - URL-to-scraper dispatcher for fetching new annotations
- [LinkMetadataTypes.hs](/backend/link-metadata-types-hs) - Core type definitions (MetadataItem, Path, Failure)
- [GTX.hs](/backend/gtx-hs) - GTX format parser/writer for annotation databases
- [annotations.js](/frontend/annotations-js) - Frontend annotation data layer and caching
- [Annotation/Arxiv.hs](/backend/annotation-arxiv-hs) - arXiv paper metadata scraper
- [Annotation/Biorxiv.hs](/backend/annotation-biorxiv-hs) - bioRxiv/medRxiv scraper
- [Annotation/Gwernnet.hs](/backend/annotation-gwernnet-hs) - Local gwern.net page scraper
- [Metadata/Format.hs](/backend/metadata-format-hs) - Abstract HTML cleaning utilities
