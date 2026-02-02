
# generateDirectory.hs

**Path:** `build/generateDirectory.hs` | **Language:** Haskell | **Lines:** ~398

> Generates tag directory index pages (`doc/*/index.md`) from filesystem contents and annotation metadata

---

## Overview

This module creates the browsable tag directory index pages that form gwern.net's hierarchical organization system. Each `doc/*/index.md` page aggregates three types of content: subdirectories (child tags), files physically present in the directory, and external links/annotations tagged with that directory's tag.

The key insight is treating directories as tags—a file in `/doc/ai/` is implicitly tagged with `ai`, but so can any annotation in the metadata database that explicitly includes `ai` in its tag list. This unifies physical filesystem organization with semantic tagging into one coherent browsing experience.

The module generates rich Markdown output including YAML frontmatter, hierarchical navigation links (parent/previous/next), transcluded abstracts, clustered link sections via embedding similarity, and link bibliographies. Output uses Pandoc AST construction followed by Markdown serialization.

---

## Public API

### `main :: IO ()`

Entry point. Reads command-line arguments (directory paths), loads metadata databases, and dispatches to `generateDirectory` for each path.

**Called by:** `sync.sh` (build orchestrator)
**Calls:** `readLinkMetadataSlow`, `readArchiveMetadata`, `readListName`, `readListSortedMagic`, `generateDirectory`

**Key arguments:**
- `--fast`: Skip regular directories, only generate `newest` and `blog`
- List of directory paths: e.g., `["doc/", "doc/ai/", "doc/ai/anime/"]`

---

### `generateDirectory :: Bool -> ArchiveMetadata -> Metadata -> ListName -> ListSortedMagic -> [FilePath] -> FilePath -> IO ()`

Core function that generates a single `index.md` file for a directory/tag.

**Parameters:**
- `newestp`: Boolean flag for special `/doc/newest/` handling
- `am`: Archive metadata for URL localization
- `md`: Full annotation metadata database
- `ldb`: Embedding list name mapping
- `sortDB`: Pre-computed similarity sorted lists
- `dirs`: All directories being generated (for prev/next navigation)
- `dir''`: The specific directory to generate

**Called by:** `main`
**Calls:** `listTagDirectories`, `listFiles`, `listTagged`, `generateYAMLHeader`, `generateDirectoryItems`, `generateSections`, `writeUpdatedFile`

---

## Internal Architecture

### Data Flow

```
Filesystem scan (listDirectory)
         ↓
    listFiles() ──────────────→ [(FilePath, MetadataItem, LinkBib)]
         ↓                              ↓
    listTagged() ─────────────→ [(FilePath, MetadataItem, LinkBib)]
         ↓                              ↓
  Sort & partition:                     ↓
   - selfLinks (Gwern-authored)         ↓
   - titledLinks (has title)            ↓
   - untitledLinks (no title)           ↓
   - linksWP (Wikipedia)                ↓
         ↓                              ↓
  generateSections() ←──────────────────┘
         ↓
  Pandoc AST construction
         ↓
  writeMarkdown → index.md
```

### Key Data Structures

**Triplet format:** `(FilePath, MetadataItem, FilePath)` where:
- First `FilePath`: URL or file path
- `MetadataItem`: `(title, author, date, doi, tags, abstract)` tuple
- Second `FilePath`: Link bibliography path (if any)

### Page Structure

Generated index pages contain these sections in order:
1. **YAML frontmatter** - title, description, dates, thumbnail, navigation
2. **Abstract** - Transcluded from `abstract.md` or top-level essay annotation
3. **See Also** - Child directories + cross-linked tag directories
4. **Gwern** - Self-authored items (sorted by modification date)
5. **Links** - External annotations (sorted by publication date, optionally clustered)
6. **Miscellaneous** - Untitled links as bullet list
7. **Bibliography** - Link bibliographies for annotated items

---

## Key Patterns

### Directory-as-Tag Unification

```haskell
let tagSelf = if dir'' == "doc/" then ""
              else init $ delete "doc/" dir''
-- "doc/cat/psychology/drug/catnip/" → 'cat/psychology/drug/catnip'
```

Directories under `doc/` become tags. A file at `/doc/ai/2024-paper.pdf` is implicitly in the `ai` tag, but annotations can also explicitly list `ai` in their tags field to appear in `/doc/ai/index`.

### Newest Meta-Directory

The `/doc/newest/` directory is special—it shows the N most recent items across all tags:

```haskell
filterDbNewest :: Int -> Int -> Int -> Metadata -> Metadata
filterDbNewest selfN annotationN linkN md = ...
-- Takes: 10 self-authored, 50 annotations, 50 links
```

Uses modification date for self-authored (captures updates), creation date for others.

### Cross-Linked Tag Directories

Tags can themselves be tagged, enabling cross-references without deep nesting:

```haskell
let taggedDirs = sort $ map (\(f,_,_) -> f) $
    filter (\(f,_,_) -> "/doc/"`isPrefixOf`f &&
                        "/index"`isSuffixOf`f &&
                        f `notElem` direntries') tagged
```

So `/doc/exercise/index` tagged with `longevity` appears in `/doc/longevity/index`'s directory section.

### Smart Thumbnail Selection

```haskell
let imageFirst = take 1 $ filter safeSVGNot $
    concatMap (\(p,(_,_,_,_,_,_,abstract),_) ->
        if isImageFilename p
        then [Image nullAttr [] (T.pack p,"")]
        else extractImages $ toPandoc abstract) $
    sortByDateModified $ links ++ triplets
```

Finds the first non-SVG image from files or annotation abstracts as the page thumbnail.

### Magic Sorting via Embeddings

For tags with enough entries, provides an alternative "Sort By Magic" view:

```haskell
titledLinksSorted <- if newestp then return []
    else sortSimilarsStartingWithNewestWithTag ldb sortDB md' tagSelf titledLinks
```

Uses pre-computed embedding similarities to cluster related annotations together.

---

## Configuration

| Constant | Location | Purpose |
|----------|----------|---------|
| `newestp` limits | `filterDbNewest` | 10 self, 50 annotations, 50 links for `/doc/newest/` |
| `chunkSize = 1` | `main` | Directories processed per parallel batch (kept low to avoid OOM) |
| `CGS.minTagAuto` | `Config.GenerateSimilar` | Minimum entries before showing "Sort By Magic" |

---

## Integration Points

### Imports From Other Modules

| Module | Imports |
|--------|---------|
| `LinkMetadata` | `readLinkMetadataSlow`, `generateAnnotationTransclusionBlock`, `hasAnnotation`, `annotateLink`, `lookupFallback`, sorting functions |
| `Tags` | `listTagDirectories`, `listTagDirectoriesAll`, `abbreviateTag` |
| `LinkBacklink` | `getLinkBibLinkCheck` |
| `GenerateSimilar` | `sortSimilarsStartingWithNewestWithTag`, `readListName`, `readListSortedMagic` |
| `Typography` | `identUniquefy`, `titleWrap` |
| `LinkArchive` | `readArchiveMetadata` |

### Output Files

Writes to `doc/*/index.md` via `writeUpdatedFile`, which only updates if content changed (for incremental builds).

### YAML Frontmatter Fields

Generated pages include:
- `title`: Tag name with abbreviation
- `description`: Summary with counts and links to sections
- `thumbnail` / `thumbnail-text`: First image from content
- `created` / `modified`: Min/max dates from entries
- `previous` / `next`: Sibling directory navigation
- `index: True`: Marks as directory index
- `backlink: False`: Disables backlink generation

---

## See Also

- [hakyll.hs](/backend/hakyll-hs) - Main site generator that uses generated directories
- [Tags.hs](/backend/tags-hs) - Tag listing and abbreviation utilities
- [sync.sh](/backend/sync-sh) - Build orchestrator that invokes this module
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database providing entry metadata
- [GenerateSimilar.hs](/backend/generate-similar-hs) - Embedding-based clustering for "Sort By Magic"
- [Typography.hs](/backend/typography-hs) - Title formatting utilities
