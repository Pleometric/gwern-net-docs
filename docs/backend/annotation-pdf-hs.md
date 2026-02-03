
# Annotation/PDF.hs

**Path:** `build/Annotation/PDF.hs` | **Language:** Haskell | **Lines:** ~74

> Extracts metadata from local PDF files using exiftool and fetches abstracts from Crossref

---

## Overview

Annotation/PDF.hs handles metadata extraction for locally-hosted PDF files on gwern.net. When a PDF link is encountered that lacks annotation data, this module attempts to populate title, author, date, and DOI fields by parsing the PDF's embedded metadata via `exiftool`, then optionally enriches the annotation with an abstract fetched from the Crossref API.

The module occupies a specific niche in the annotation pipeline: it's invoked by `Annotation.linkDispatcherURL` when processing `.pdf` URLs. Unlike web scrapers that parse HTML, this module relies entirely on embedded PDF metadata and external DOI resolution. It returns `Left Permanent` only when **all** extracted fields are empty; missing files trigger an error.

Key design decisions include preferring the longer of `Author` vs `Creator` fields (since PDF metadata conventions vary), using Crossref as the sole source for abstracts, and appending page-number suffixes to titles when linking to specific PDF pages.

---

## Public API

### `pdf :: Metadata -> Path -> IO (Either Failure (Path, MetadataItem))`

Main entry point. Extracts metadata from a PDF file at the given path.

**Parameters:**
- `md` — The existing metadata database (passed through to `doi2Abstract` for paragraph processing)
- `p` — Local file path (e.g., `/doc/ai/2023-smith.pdf` or `/doc/ai/2023-smith.pdf#page=5`)

**Returns:** `Right (path, metadataItem)` on success with extracted title, author, date, DOI, and abstract; `Left Permanent` only when all extracted fields are empty (missing files `error`).

**Called by:** `Annotation.linkDispatcherURL` (when path has `.pdf` extension)
**Calls:** `exiftool` (via shell), `doi2Abstract`, `cleanAuthors`, `linkAutoHtml5String`, `pageNumberParse`

---

### `doi2Abstract :: Metadata -> String -> IO (Maybe String)`

Fetches an abstract from Crossref given a DOI.

```haskell
doi2Abstract :: Metadata -> String -> IO (Maybe String)
doi2Abstract md doi = if length doi < 7 then return Nothing
                   else do (_,_,bs) <- runShellCommand "./" Nothing "curl"
                           ["--location", "--silent",
                            "https://api.crossref.org/works/"++doi,
                            "--user-agent", "gwern+crossrefscraping@gwern.net"]
```

**Parameters:**
- `md` — Metadata database (passed to paragraph processing)
- `doi` — DOI string (e.g., `10.1234/example`)

**Returns:** `Just abstract` with cleaned, auto-linked HTML, or `Nothing` if DOI is too short, not found, or lacks an abstract.

**Called by:** `pdf`
**Calls:** `curl` (via shell), Crossref API, `processParagraphizer`, `linkAutoHtml5String`, `cleanAbstractsHTML`

---

## Internal Architecture

### Data Flow

```
PDF file path
    ↓
exiftool (5 calls)
    ↓
[Title, Author, Creator, Date, DOI]
    ↓
Field selection & cleaning
    ↓
doi2Abstract (if DOI present)
    ↓
Crossref API → JSON decode
    ↓
MetadataItem tuple
```

### Crossref JSON Types

```haskell
newtype Crossref = Crossref { message :: Message }
newtype Message = Message { abstract :: Maybe String }
```

The Crossref API returns a nested JSON structure. These newtypes extract just the `message.abstract` field via Aeson's generic deriving.

### MetadataItem Structure

The returned tuple follows the standard gwern.net format:
```haskell
(title, author, date, dateCreated, keyValues, tags, abstract)
-- e.g., ("Paper Title § pg5", "John Smith", "2023-01-15", "", [("doi","10.1234/...")], [], "<p>Abstract...</p>")
```

---

## Key Patterns

### Author/Creator Field Selection

PDF metadata is inconsistent. Some PDFs use `Author` for the content authors and `Creator` for the PDF-generating software/person; others reverse this. The module picks whichever is longer:

```haskell
let author = ... $ if length eauthor' > length ecreator
                   then eauthor'
                   else ecreator
```

This heuristic handles the common case where one field contains "Adobe Acrobat" and the other contains the actual author list.

### Page Number Handling

Links can target specific PDF pages via fragment (e.g., `#page=5`). The module appends this to the title:

```haskell
let pageNumber = pageNumberParse p
let title = titleBase ++ (if null pageNumber' || null titleBase
                          then ""
                          else " § pg" ++ pageNumber')
```

### Parallel Exiftool Calls

Each metadata field requires a separate `exiftool` invocation (5 total: Title, Author, Creator, Date, DOI). These run sequentially in the current implementation—a potential optimization point.

---

## Configuration

### Crossref User-Agent

The Crossref API call includes a contact email for rate-limiting politeness:

```haskell
"--user-agent", "gwern+crossrefscraping@gwern.net"
```

This identifies the scraper to Crossref per their API guidelines.

### Working Directory

The module calls `C.cd` at the start to ensure consistent working directory for `exiftool` path resolution.

---

## Integration Points

### Upstream: Annotation Dispatcher

Called from `Annotation.Gwernnet.gwern`:
```haskell
gwern md p
    | ext == ".pdf"  = pdf md p
```

The `.pdf` extension check happens in Gwernnet; PDF.hs assumes it receives valid PDF paths.

### Downstream: Metadata Utilities

Uses several shared utilities from `Metadata.*`:
- `cleanAuthors` — Normalizes author name formats
- `trimTitle` — Removes trailing punctuation, normalizes whitespace
- `filterMeta` — Strips problematic characters
- `processDOI` — Normalizes DOI format (removes `doi:` prefix, URL prefixes)
- `pageNumberParse` — Extracts page number from URL fragment

### External Dependencies

- **exiftool** — Must be installed and in PATH; called 5 times per PDF
- **curl** — Used for Crossref API requests
- **Crossref API** — `https://api.crossref.org/works/{doi}` — rate-limited, requires polite user-agent

### Events/State

None. This module is stateless and performs pure I/O operations.

---

## Error Handling

| Condition | Result |
|-----------|--------|
| Empty path argument | `error` (fatal, should never happen) |
| PDF file doesn't exist | `error` (fatal, invalid path) |
| No metadata extracted | `Left Permanent` |
| Crossref request fails | Prints red error, returns `Nothing` for abstract |
| DOI too short (\<7 chars) | Skips Crossref lookup |

The distinction between `error` (programming bug) and `Left Permanent` (expected failure for unreadable PDFs) is deliberate.

---

## See Also

- [Annotation.hs](/backend/annotation-hs) - URL-to-scraper dispatcher that routes to this module
- [Annotation/Gwernnet.hs](/backend/annotation-gwernnet-hs) - Calls this module for .pdf files
- [LinkMetadata.hs](/backend/link-metadata-hs) - Database that stores extracted metadata
- [LinkMetadataTypes.hs](/backend/link-metadata-types-hs) - Core type definitions (Failure, MetadataItem)
- [Metadata/Format.hs](/backend/metadata-format-hs) - String cleaning utilities (processDOI, trimTitle)
- [Metadata/Author.hs](/backend/metadata-author-hs) - Author name normalization via cleanAuthors
- [Paragraph.hs](/backend/paragraph-hs) - LLM-based paragraph splitting for Crossref abstracts
