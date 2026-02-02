
# Annotation.Gwernnet

**Path:** `build/Annotation/Gwernnet.hs` | **Language:** Haskell | **Lines:** ~280

> Local gwern.net page scraper for extracting metadata from local essays and sections

---

## Overview

Annotation.Gwernnet is the scraper for extracting metadata from local gwern.net pages. When the parent `Annotation.hs` dispatcher encounters a URL starting with `/` that isn't a PDF or known file type, it delegates here to fetch the live page via `curl` and parse its HTML for metadata.

The module handles two distinct cases: full pages and section anchors. For full pages (e.g., `/gpt-3`), it extracts the page's title, author, date, description, abstract (from the `<div class="abstract">` block), thumbnail, and table of contents. For section anchors (e.g., `/gpt-3#scaling-laws`), it locates that specific section and extracts any section-specific abstract, appending a section title suffix like "GPT-3 § Scaling Laws".

Key design decisions:
- Extensive file extension blacklist prevents scraping binaries, images, and data files
- URL pattern blacklist skips metadata pages, blog dates, and common non-content sections (#see-also, #footnotes, etc.)
- Thumbnail handling includes automatic color-inversion detection via `invertImage`
- Special-case for `/doc/index` generates a hierarchical ToC of all tag directories

---

## Public API

### `gwern :: Metadata -> Path -> IO (Either Failure (Path, MetadataItem))`

Main entry point. Fetches and parses a gwern.net URL, returning structured metadata.

```haskell
gwern md "/gpt-3"           -- Scrape full page
gwern md "/gpt-3#section"   -- Scrape section-specific abstract
gwern md "/doc/ai/index"    -- Scrape tag index page
```

**Called by:** `Annotation.linkDispatcherURL` (line 84)
**Calls:** `pdf` (for PDFs), `gwernAbstract`, `gwerntoplevelDocAbstract`, `invertImage`, `linkAutoHtml5String`

### `findDivContent :: String -> Maybe String`

Extracts the contents of a `<div id="TOC" class="TOC">` element from raw HTML. Returns `Nothing` if no ToC exists.

**Called by:** `gwern` (line 120)

---

## Internal Architecture

### Metadata Extraction Flow

```
gwern
  │
  ├─ Early returns:
  │   ├─ "/" or ""                        → Permanent failure
  │   ├─ .pdf extension                   → delegate to pdf scraper
  │   ├─ Binary/data extensions           → Permanent failure (blacklist)
  │   ├─ Metadata paths, #see-also, etc.  → Permanent failure
  │   └─ Anonymous sections, footnotes    → Permanent failure
  │
  └─ Fetch & parse:
      ├─ curl https://gwern.net/{path}
      ├─ Parse HTML with TagSoup
      ├─ Extract <meta> tags:
      │   ├─ title          → dc.title / title
      │   ├─ author         → author
      │   ├─ date           → dc.date.issued
      │   ├─ dateModified   → dcterms.modified
      │   ├─ description    → description
      │   ├─ keywords       → keywords (→ tags)
      │   ├─ thumbnail      → og:image
      │   ├─ thumbnailText  → og:image:alt
      │   └─ thumbnailCSS   → gwern:thumbnail:css-classes
      │
      ├─ invertImage(thumbnail) → color detection
      ├─ gwernAbstract() → extract abstract + ToC
      └─ Return MetadataItem tuple
```

### Meta Tag Extraction

The module filters for `<meta>` tags and extracts values via helper functions:

| Function | Meta Tag | Returns |
|----------|----------|---------|
| `safeTitle` | `name="title"` | Page title |
| `safeDateIssued` | `name="dc.date.issued"` | Creation date |
| `safeDateModified` | `name="dcterms.modified"` | Last modified date |
| `safeDescription` | `name="description"` | SEO description |
| `safeAuthor` | `name="author"` | Author name |
| `safeKeywords` | `name="keywords"` | Comma-separated tags |

OpenGraph tags are used for thumbnails:
- `og:image` → thumbnail URL
- `og:image:alt` → thumbnail alt text
- `gwern:thumbnail:css-classes` → CSS classes (including invert flags)

### Abstract Extraction Logic

`gwernAbstract` handles both page-level and section-level abstracts:

```
Full page (no #anchor):
  1. Find <body>
  2. Find <div class="abstract">
  3. Extract contents until </div>
  4. Append ToC if present

Section anchor (with #anchor):
  1. Find element with matching id
  2. Extract section title from <a> inside header
  3. Find <div class="abstract"> within section
  4. Extract section-specific abstract
  5. Convert relative #anchors to absolute paths
```

### MetadataItem Structure

```haskell
type MetadataItem = (String, String, String, String, [(String,String)], [String], String)
--                   title   author  date    dateCreated  kvs        tags     abstract

-- Example output:
("GPT-3 § Scaling Laws", "Gwern Branwen", "2020-06-01", "2020-06-01", [],
 ["ai", "scaling"], "<p>Abstract text...</p><figure>...</figure>")
```

---

## Key Patterns

### Blacklist-Driven Early Returns

The module uses extensive pattern matching to skip non-content URLs:

```haskell
-- Extension blacklist (data files, binaries, images)
| ext `elem` ["avi", "bmp", "csv", "doc", "gif", "jpg", "json",
              "mp3", "mp4", "pdf", "png", "svg", "txt", "zip", ...]
    = return (Left Permanent)

-- Path pattern blacklist
| anyPrefix p ["metadata", "/metadata", ...]
  || anySuffix p ["#see-also", "#footnotes", "#links", ...]
  || anyInfix p ["index.html", "/index#"]
    = return (Left Permanent)

-- Regex-based skips
| p =~ sectionAnonymousRegex = return (Left Permanent)  -- #section-3 etc.
| p =~ footnoteRegex = return (Left Permanent)          -- #fn1, #fnref2 etc.
```

### Thumbnail Color Inversion

Thumbnails can have automatic or manual inversion for dark mode:

```haskell
(color, h, w) <- invertImage thumbnail'  -- Returns (needsInvert, height, width)

let color' = if "invert" `elem` thumbnailCSS || "invert-not" `elem` thumbnailCSS
             then ""                           -- Manual override present
             else if color then "invert-auto"  -- Auto-detected
             else "invert-not"                 -- No inversion needed
```

### TagSoup Parsing Helpers

A suite of predicate functions for TagSoup-based HTML navigation:

```haskell
dropToBody     -- Skip until <body>
dropToAbstract -- Skip until <div class="abstract">
takeToAbstract -- Take until </div>
filterAbstract -- Remove div/blockquote wrappers
dropToID       -- Skip until element with specific id
dropToSectionEnd -- Stop at </section> or nested <section>
```

### Special-Case /doc/index Generation

The top-level `/doc/index` page is synthesized rather than scraped:

```haskell
gwerntoplevelDocAbstract = do
  allDirs <- listTagDirectoriesAll ["doc/"]
  -- Generate links to all subdirectories
  let allDirLinks = unlines $ map (\d ->
        "<li><a class='link-page link-tag directory-indexes-downwards...' href=\""
        ++ d ++ "\">" ++ abbreviateTag d ++ "</a></li>") allDirs
  return $ Right ("/doc/index", (title, author, ..., abstractWithToc))
```

---

## Configuration

### Extension Blacklist (line 60-67)

Files with these extensions are never scraped:

```haskell
["avi", "bmp", "conf", "css", "csv", "doc", "docx", "ebt", "epub",
 "gif", "hi", "hs", "htm", "html", "ico", "idx", "img",
 "jpeg", "jpg", "js", "json", "jsonl", "maff", "mdb", "mht",
 "mp3", "mp4", "mkv", "o", "ods", "opml", "pack", "md", "patch",
 "php", "png", "r", "rm", "sh", "svg", "swf", "tar", "ttf", "txt",
 "wav", "webm", "xcf", "xls", "xlsx", "xml", "xz", "zip", "sqlite",
 "par2", "pkl", "h5", "t7", "weights"]
```

*Note: When adding new formats, also update LinkIcon!*

### Path Blacklists (line 68-77)

- **Prefixes:** `metadata`, `/metadata`, `/blog/2` (date routes)
- **Suffixes:** `#external-links`, `#see-also`, `#footnotes`, `#links`, `#misc`, `#appendix`, `#conclusion`, `#media`, etc.
- **Infixes:** `index.html`, `/index#`
- Newsletter sections like `/newsletter/2022/01#fiction` (no abstracts)

### Curl Configuration (line 84)

```bash
curl --silent --max-filesize 100000000 \
     --user-agent "gwern+gwernscraping@gwern.net" \
     https://gwern.net/{path}
```

No `--location` flag: redirects are intentionally not followed to prevent duplicate annotations.

### Invalid Date Handling (line 93-96)

Dates of "N/A" or "2009-01-01" are treated as missing (2009-01-01 is a legacy placeholder).

---

## Integration Points

### Upstream (callers)

- **Annotation.hs:84** - `linkDispatcherURL` routes local non-PDF URLs here
- Direct access for re-scraping: `LinkMetadata.hs` can import `gwern` directly

### Downstream (dependencies)

| Module | Functions Used |
|--------|----------------|
| `Annotation.PDF` | `pdf` for PDF delegation |
| `Image` | `invertImage` for thumbnail color detection |
| `Metadata.Format` | `checkURL`, `cleanAbstractsHTML`, regex patterns |
| `Metadata.Author` | `cleanAuthors` |
| `Metadata.Date` | `isDate` validation |
| `Tags` | `listTagDirectoriesAll`, `abbreviateTag` |
| `LinkAuto` | `linkAutoHtml5String` for auto-linking |
| `Utils` | String manipulation helpers |

### Shared State

- Reads live HTML from `gwern.net` (no caching within module)
- Uses shared regex patterns from `Metadata.Format`: `sectionAnonymousRegex`, `footnoteRegex`

### Output Format

Annotations include special HTML classes for downstream rendering:
- `page-description-annotation` - wrapper for description paragraph
- `columns TOC` - two-column table of contents
- `float-right page-thumbnail` - positioned thumbnail figure
- `invert-auto` / `invert-not` - dark mode inversion hints

---

## Error Handling

| Condition | Result |
|-----------|--------|
| 404 / "no page by this name" | `Left Permanent` |
| Empty tags AND empty abstract | `Left Permanent` |
| Download failure (curl error) | `Left Permanent` + red warning |
| Missing thumbnail-text | Yellow warning, continues |
| File > 100MB | Runtime error (shouldn't happen) |
| Period in essay path | Runtime error (indicates bug) |

---

## See Also

- [Annotation.hs](/backend/annotation-hs) - Parent dispatcher that routes to this module
- [LinkMetadata.hs](/backend/link-metadata-hs) - Manages the metadata database
- [LinkMetadataTypes.hs](/backend/link-metadata-types-hs) - Core type definitions (Failure, MetadataItem)
- [Annotation/PDF.hs](/backend/annotation-pdf-hs) - PDF metadata extraction (called for .pdf files)
- [Metadata/Format.hs](/backend/metadata-format-hs) - cleanAbstractsHTML and regex patterns
- [Metadata/Author.hs](/backend/metadata-author-hs) - Author name normalization
- [Metadata/Date.hs](/backend/metadata-date-hs) - Date validation
