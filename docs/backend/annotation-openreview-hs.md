
# Annotation.OpenReview

**Path:** `build/Annotation/OpenReview.hs` | **Language:** Haskell | **Lines:** ~42

> Scrapes academic paper metadata from OpenReview.net conference papers

---

## Overview

OpenReview.net is a platform for open peer review of academic papers, hosting many machine learning conference submissions (ICLR, NeurIPS workshops, etc.). This module extracts structured metadata from OpenReview paper URLs for use in gwern.net's annotation system.

The module delegates the actual HTTP fetching and JSON parsing to an external shell script (`openReviewAbstract.sh`), then processes and normalizes the returned data. It handles both `/forum?id=` (discussion page) and `/pdf?id=` (direct PDF) URL formats, converting the latter to the former for scraping since the forum page contains the structured metadata.

A notable feature is the combination of multiple abstract-like fields: OpenReview papers often have a "TL;DR" summary in addition to the full abstract, plus keywords. All three are combined into a unified abstract field with the TL;DR first, then full abstract, then keywords as an HTML comment.

---

## Public API

### `openreview :: Metadata -> Path -> IO (Either Failure (Path, MetadataItem))`

Main entry point for scraping an OpenReview paper URL.

**Parameters:**
- `md` - The global metadata database (passed through for `processParagraphizer`)
- `p` - The OpenReview URL to scrape

**Returns:**
- `Right (path, metadataItem)` on success with extracted metadata
- `Left Permanent` on failure (download error or parse failure)

**Called by:** `Annotation.hs` (URL dispatcher, pattern matches on `openreview.net/forum?id=` or `openreview.net/pdf?id=`)

**Calls:**
- `runShellCommand` - Executes `openReviewAbstract.sh` with the URL
- `processArxivAbstract` - Cleans up LaTeX math notation in abstracts
- `cleanAbstractsHTML` - Sanitizes HTML in abstract text
- `processParagraphizer` - Adds paragraph breaks to long abstracts
- `linkAutoHtml5String` - Auto-links recognized terms in keywords
- `cleanAuthors` / `trimTitle` - Normalizes author and title strings

---

## Internal Architecture

### Data Flow

```
URL (forum or pdf)
        │
        ▼
openReviewAbstract.sh (curl + jq)
        │
        ▼
  Line-separated output:
  [title, author, date, tldr, abstract, keywords...]
        │
        ▼
  Parse & clean each field
        │
        ▼
  Combine: tldr + abstract + "<!-- Keywords: ... -->"
        │
        ▼
  MetadataItem tuple
```

### Shell Script Behavior

The companion script `build/openReviewAbstract.sh` does the heavy lifting:

1. Fetches the URL via curl with redirects followed
2. Pipes HTML through `tidy` to normalize it
3. Greps for the `pageProps` JSON blob embedded in the page
4. Uses `jq` to extract: title, authors (joined), timestamp (converted to date), TL;DR, abstract, keywords (joined)
5. Cleans up null values and LaTeX artifacts

The script handles two OpenReview JSON formats (with and without `.value` nesting) for compatibility across different paper types.

### MetadataItem Structure

Returns a 7-tuple:
```haskell
(title, authors, date, "", [], [], abstract)
--  ^      ^      ^    ^   ^   ^     ^
--  |      |      |    |   |   |     Combined TL;DR + abstract + keywords
--  |      |      |    |   |   Tags (empty)
--  |      |      |    |   Key-value pairs (empty)
--  |      |      |    DOI/created date (empty)
--  |      |      Publication date (YYYY-MM-DD)
--  |      Comma-separated author list
--  Cleaned title
```

---

## Key Patterns

### URL Normalization

PDF URLs are converted to forum URLs before scraping:

```haskell
let p' = replace "/pdf?id=" "/forum?id=" p
```

This ensures consistent metadata extraction since the PDF endpoint doesn't contain the structured metadata JSON.

### Keyword Embedding as HTML Comment

Keywords are preserved but hidden from display using HTML comments:

```haskell
let keywords'
  | null keywords || keywords == [""] = ""
  | length keywords > 1 =
      unlines (init keywords) ++ "\n<!-- [Keywords: " ++ last keywords ++ "] -->"
  | otherwise = "<!-- [Keywords: " ++ concat keywords ++ "] -->"
```

This keeps keywords searchable in the raw HTML while not cluttering the visible abstract.

### Arxiv Abstract Processing Reuse

OpenReview papers often contain LaTeX math notation similar to arXiv papers, so the module reuses `processArxivAbstract` for cleanup:

```haskell
let tldr' = cleanAbstractsHTML $ processArxivAbstract tldr
let desc' = cleanAbstractsHTML $ processArxivAbstract desc
```

---

## Configuration

No direct configuration options. Behavior is controlled by:

- The `openReviewAbstract.sh` script's jq selectors
- Shared abstract cleaning functions from `Metadata.Format`
- Author normalization rules in `Metadata.Author`

---

## Integration Points

### URL Dispatch (Annotation.hs)

The annotation dispatcher routes to this module based on URL prefix matching:

```haskell
| "https://openreview.net/forum?id=" `isPrefixOf` l
  || "https://openreview.net/pdf?id=" `isPrefixOf` l =
    openreview md l
```

### Link Archiving (Config/LinkArchive.hs)

OpenReview forum URLs are converted to PDF URLs for archiving:

```haskell
replace "https://openreview.net/forum" "https://openreview.net/pdf"
```

This complements this module's inverse transformation, ensuring users see metadata from the forum page while archived PDFs provide permanence.

### Link Icons (Config/LinkIcon.hs)

OpenReview links display a distinctive "OR" icon in red (`#8c1b13`), matching the site's branding.

---

## See Also

- [Annotation.hs](/backend/annotation-hs) - URL dispatcher that routes to this module
- [LinkMetadata.hs](/backend/link-metadata-hs) - Metadata database manager
- [LinkMetadataTypes.hs](/backend/link-metadata-types-hs) - Core type definitions (Failure, MetadataItem)
- [Annotation/Arxiv.hs](/backend/annotation-arxiv-hs) - Similar scraper (shares processArxivAbstract)
- [Annotation/Biorxiv.hs](/backend/annotation-biorxiv-hs) - Similar scraper for bioRxiv
- [openReviewAbstract.sh](/shell/openreviewabstract) - Shell script that fetches OpenReview data
- [Paragraph.hs](/backend/paragraph-hs) - LLM-based paragraph splitting for abstracts

---
