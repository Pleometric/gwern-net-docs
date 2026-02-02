
# Annotation/Arxiv.hs

**Path:** `build/Annotation/Arxiv.hs` | **Language:** Haskell | **Lines:** ~106

> Scrapes metadata from arXiv papers via the arXiv API, converting LaTeX abstracts to HTML.

---

## Overview

This module handles metadata extraction for arXiv preprints. When gwern.net encounters a link to `arxiv.org/abs/*` or `arxiv.org/pdf/*`, this module queries the arXiv API, extracts structured metadata (title, authors, date, DOI, abstract), and converts the LaTeX-formatted abstract into clean HTML.

The core challenge is that arXiv abstracts are pseudo-HTML containing raw LaTeX—inline math, citation commands, special characters. The module uses Pandoc to parse LaTeX and emit HTML, then applies extensive regex-based cleanup to handle edge cases like dollar signs in non-math contexts, broken citation macros, and percentage signs.

The module also handles arXiv's DOI assignment scheme (introduced 2022), constructing DOIs for older papers using the `10.48550/arXiv.XXXX.XXXXX` format when no explicit DOI is provided.

---

## Public API

### `arxiv :: Metadata -> Path -> IO (Either Failure (Path, MetadataItem))`

Main entry point. Downloads arXiv API response for a URL, parses XML, extracts metadata fields, processes the abstract through LaTeX→HTML conversion.

**Called by:** `Annotation.linkDispatcher` (when URL matches `arxiv.org/abs/` or `arxiv.org/pdf/`)
**Calls:** `arxivDownload`, `processArxivAbstract`, `cleanAuthors`, `trimTitle`, `processDOI`, `processDOIArxiv`, `linkAutoHtml5String`, `processParagraphizer`

**Returns:**
- `Left Temporary` — API call failed or parsing produced empty/error results (may succeed on retry)
- `Right (url, metadata)` — Successfully extracted metadata tuple

### `processArxivAbstract :: String -> String`

Converts a LaTeX abstract string to HTML. Exposed for reuse by `Annotation.OpenReview`.

**Called by:** `arxiv` (this module), `Annotation.OpenReview`
**Calls:** Pandoc `readLaTeX`/`writeHtml5String`, `inlineMath2Text`, `cleanAbstractsHTML`

---

## Internal Architecture

### Data Flow

```
URL → arxivDownload → XML response
                          ↓
                    parseTags (TagSoup)
                          ↓
              element "entry" extraction
                          ↓
    ┌─────────────────────┼─────────────────────┐
    ↓                     ↓                     ↓
  title               authors              abstract
    ↓                     ↓                     ↓
processArxivAbstract  getAuthorNames    processArxivAbstract
                          ↓                     ↓
                     cleanAuthors        linkAutoHtml5String
                                                ↓
                                        processParagraphizer
                          ↓
                    MetadataItem tuple
```

### Key Functions

**`arxivDownload :: String -> IO (ExitCode, ByteString, String)`**

Extracts arXiv ID from URL (handles both `/abs/` and `/pdf/` formats), calls the arXiv query API via curl. Returns exit code, response body, and extracted ID.

**`element :: String -> [Tag String] -> ([Tag String], [Tag String])`**

TagSoup helper to extract content between XML open/close tags. Handles nested same-name elements via a depth counter. Inlined from the unmaintained `arxiv` Hackage package.

**`findTxt :: [Tag String] -> String`**

Extracts text content from the first `TagText` node in a tag list.

**`getAuthorNames :: [Tag String] -> [String]`**

Recursively extracts all `<author><name>...</name></author>` entries from the API response.

---

## Key Patterns

### LaTeX Abstract Processing Pipeline

arXiv abstracts require multiple transformation stages:

```haskell
processArxivAbstract a =
  -- 1. Dollar sign heuristic: single $ means currency, not math
  let dollarSignsN = length $ filter (=='$') a
      tex = sedMany C.arxivAbstractRegexps $
            replaceMany C.arxivAbstractFixedRewrites $
            (if dollarSignsN == 1 then replaceMany [("$", "\\$")] else id) a
  -- 2. Parse as LaTeX, write as HTML with MathJax
  pandoc <- readLaTeX def{...} $ T.pack tex
  writeHtml5String opts $ walk (unsafePerformIO . inlineMath2Text) pandoc
  -- 3. Final HTML cleanup
```

The regex rewrites convert broken citation commands (`\cite{...}` → monospace text), fix percentage escaping (`%` → `\%` → `%` → `$\%$` all become `%`), and handle various one-off typos in popular papers.

### DOI Construction

Since 2022, arXiv auto-assigns DOIs. The module checks for an explicit `<arxiv:doi>` tag; if absent, constructs one using the official schema:

```haskell
let doi = if null doiTmp
          then processDOIArxiv url'  -- → "10.48550/arXiv.XXXX.XXXXX"
          else processDOI doiTmp
```

### Inlined TagSoup Code

The module inlines XML parsing helpers from `Network.Api.Arxiv` rather than depending on the unmaintained `arxiv` package, which had silent data corruption bugs. This keeps the parsing logic close to the application for easy patching.

---

## Configuration

All configuration lives in `Config.Misc`:

| Constant | Purpose |
|----------|---------|
| `cleanArxivAbstracts` | Post-processing string replacements; removes/replaces "significant" puffery |
| `arxivAbstractRegexps` | Regex patterns for citation commands, lambda syntax |
| `arxivAbstractFixedRewrites` | Fixed string replacements for escape sequences, typos |

Example from `cleanArxivAbstracts`:
```haskell
[(" significant", ""), (" significantly", ""),
 ("more significant", "important"), ...]
```

This systematically removes vague superlatives that add no information in research abstracts.

---

## Integration Points

### Module Dependencies

- **`LinkMetadataTypes`** — `Failure`, `Metadata`, `MetadataItem`, `Path` types
- **`Metadata.Format`** — `checkURL`, `cleanAbstractsHTML`, `processDOI`, `processDOIArxiv`, `trimTitle`
- **`Metadata.Author`** — `cleanAuthors` for name normalization
- **`LinkAuto`** — `linkAutoHtml5String` auto-links author names, terms
- **`Paragraph`** — `processParagraphizer` adds paragraph breaks
- **`Utils`** — `inlineMath2Text` converts LaTeX math to Unicode where possible

### External Services

- **arXiv API** — `https://export.arxiv.org/api/query?id_list=XXXX`
  - Rate limits: undocumented, generally permissive
  - User-Agent: `gwern+arxivscraping@gwern.net`

### Shared State

- Requires `C.cd` to set working directory before processing (enables `latex2unicode.py` calls)

### Error Handling

Returns `Left Temporary` for:
- curl failures (network issues)
- Empty abstract or title="Error" (API lag)
- Author="arXiv api core" (placeholder response)

These are retryable; the link will be re-queried on next build.

---

## See Also

- [Annotation.hs](/backend/annotation-hs) - Dispatcher that routes URLs to this module
- [LinkMetadata.hs](/backend/link-metadata-hs) - Stores extracted annotations in the metadata database
- [LinkMetadataTypes.hs](/backend/link-metadata-types-hs) - Core type definitions (Failure, MetadataItem)
- [Annotation/OpenReview.hs](/backend/annotation-openreview-hs) - Reuses processArxivAbstract for LaTeX abstracts
- [Annotation/Biorxiv.hs](/backend/annotation-biorxiv-hs) - Similar scraper for bioRxiv papers
- [Metadata/Format.hs](/backend/metadata-format-hs) - cleanAbstractsHTML and processDOI utilities
- [Paragraph.hs](/backend/paragraph-hs) - LLM-based paragraph splitting for abstracts
