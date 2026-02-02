
# Annotation.hs

**Path:** `build/Annotation.hs` | **Language:** Haskell | **Lines:** ~290

URL-to-scraper dispatcher for metadata extraction

---

## Overview

Annotation.hs is the routing layer that decides how to fetch metadata for a given URL. When LinkMetadata.hs encounters a URL not already in the metadata database, it calls `linkDispatcher` here, which pattern-matches the URL against known sources (arXiv, bioRxiv, OpenReview, local PDFs, etc.) and delegates to the appropriate scraper.

The module follows a guard-clause pattern: `linkDispatcherURL` is a series of pattern matches testing URL prefixes/infixes, returning early when one matches. If no specialized scraper applies, it falls back to either local gwern.net scraping or a generic HTML title fetch.

Key design decisions:
- Scrapers return `Either Failure (Path, MetadataItem)` where `Failure` is `Temporary` (retry later) or `Permanent` (cache as unfetchable)
- URL canonicalization happens before routing via `linkCanonicalize`
- Post-processing applies to all results: date guessing, title reformatting (italics, em-dashes), author/date inference from file paths

---

## Public API

### `linkDispatcher :: Metadata -> Inline -> IO (Either Failure (Path, MetadataItem))`

Main entry point. Takes existing metadata DB and a Pandoc `Link` inline, returns scraped metadata.

```haskell
-- Example:
-- > md <- LinkMetadata.readLinkMetadata
-- > linkDispatcher md (Link nullAttr [] ("https://arxiv.org/abs/2512.03750",""))
```

Called by: `LinkMetadata.getMetadataItem` (line 499)
Calls: `linkDispatcherURL`, `tooltipToMetadata`, `guessAuthorDateFromPath`, `reformatTitle`

### `tooltipToMetadata :: Path -> String -> (String, String, String)`

Parses an existing link tooltip to extract (title, author, date). Re-exported from `Metadata.Title`.

Called by: `linkDispatcher` (when scraping fails permanently, extracts what it can from tooltip)

### `htmlDownloadAndParseTitleClean :: String -> IO String`

Fetches a URL and extracts its `<title>` tag. Re-exported from `Metadata.Title`.

Called by: `linkDispatcherURL` (fallback for unknown URLs)

### `processItalicizer :: String -> IO String`

Runs `static/build/italicizer.py` to auto-italicize work titles in strings.

Called by: `reformatTitle`

### `testGuessAuthorDate :: [(FilePath, (String,String), (String,String))]`

Test suite for `guessAuthorDateFromString`. Returns failing test cases (empty list = all pass).

---

## Internal Architecture

### URL Routing Flow

```
linkDispatcher
    │
    ├─ canonicalize URL
    │
    └─ linkDispatcherURL ─┬─ "/metadata/...", "/doc/www/", etc  → Permanent failure (blacklist)
                          ├─ "en.wikipedia.org"                 → extract title from URL
                          ├─ "arxiv.org/abs/"                   → arxiv scraper
                          ├─ "openreview.net"                   → openreview scraper
                          ├─ "biorxiv.org" / "medrxiv.org"      → biorxiv scraper
                          ├─ "x.com"                            → twitter (username only)
                          ├─ ".pdf" + local path                → pdf scraper
                          ├─ local path (no ext)                → gwern scraper
                          └─ external URL                       → htmlDownloadAndParseTitleClean
```

### Failure Type

```haskell
data Failure = Temporary | Permanent
```

- **Temporary**: Scraping failed but might succeed later (API down, rate-limited). Not cached—will retry.
- **Permanent**: URL is blacklisted or fundamentally unscrappable. Cached to avoid repeated failures.

### MetadataItem Type

```haskell
type MetadataItem = (String, String, String, String, [(String,String)], [String], String)
--                   Title   Author  Date    DateCreated  K-Vs       Tags     Abstract
```

---

## Key Patterns

### Guard-Clause Routing

`linkDispatcherURL` uses Haskell guards with pattern matching:

```haskell
linkDispatcherURL md l
  | anyPrefix l ["/metadata/...", ...] = return (Left Permanent)
  | "arxiv.org/abs/" `isInfixOf` l = arxiv md l
  | ".pdf" `isInfixOf` l = if head l' == '/' then pdf md l' else return (Left Permanent)
  | otherwise = ...
```

Order matters: more specific patterns come before general ones.

### Post-Processing Pipeline

All successful metadata goes through:
1. `guessDateFromString` - infer date from title/URL if missing
2. `reformatTitle` - italicize work titles, convert hyphens to em-dashes
3. `guessAuthorDateFromPath` - extract author/date from local file paths
4. `authorsUnknownPrint` - warn about unrecognized authors

### File Path Date/Author Inference

For local files like `/doc/ai/2020-08-05-hippke-analysis.pdf`, the module extracts:
- Date: finds valid YYYY-MM-DD patterns, takes the latest if multiple
- Author: token after date, validated against known author list

```haskell
-- "/doc/traffic/2015-07-03-2016-01-03-gwern-analytics.pdf"
--   → date: "2016-01-03", author: "gwern"
```

---

## Scraper Modules

### Annotation.Arxiv (~105 lines)

Fetches from arXiv's Atom API (`export.arxiv.org/api/query`).

- Handles both `/abs/` and `/pdf/` URLs
- Processes LaTeX in titles/abstracts via Pandoc
- Constructs DOI if not provided (`10.48550/arXiv.XXXX.XXXXX`)
- Returns `Temporary` failure if API lags (abstract may appear later)

### Annotation.Biorxiv (~60 lines)

Scrapes bioRxiv/medRxiv HTML pages.

- Extracts Dublin Core metadata (`DC.Title`, `DC.Contributor`, etc.)
- Skips direct PDF links (returns empty metadata)
- Known bug: broken quote encoding (`9s` → `s` workaround)

### Annotation.OpenReview (~40 lines)

Calls external shell script `openReviewAbstract.sh`.

- Combines TL;DR, description, and keywords into abstract
- Reuses `processArxivAbstract` for LaTeX handling

### Annotation.PDF (~75 lines)

Reads local PDF metadata via `exiftool`.

- Falls back to Crossref API for abstract if DOI present
- Handles Creator vs Author field ambiguity (uses longer one)
- Appends page number to title if URL has `#page=N`

### Annotation.Gwernnet (~280 lines)

Scrapes gwern.net pages.

- Large extension blacklist (images, archives, data files)
- Extracts: title, description, abstract, thumbnail, ToC, tags
- Special handling for `/doc/index` (generates directory listing)
- Section anchors get their own section-specific abstracts

---

## Configuration

### URL Blacklists

Hardcoded in `linkDispatcherURL`:
```haskell
anyPrefix l ["/metadata/annotation/backlink/", "/metadata/annotation/similar/",
             "/doc/www/", "/ref/", "/blog/", "irc://", "mailto:"]
```

### Extension Blacklist (Gwernnet)

Files with these extensions are skipped:
```haskell
ext `elem` ["avi", "bmp", "csv", "doc", "epub", "gif", "jpg", "json",
            "mp3", "mp4", "pdf", "png", "svg", "txt", "zip", ...]
```

### Italicizer Whitelist

Titles that should not be auto-italicized:
```haskell
whitelist = ["Guys and Dolls", "A critique of pure reason"]
```

### Config Module References

- `Config.Misc.todayDayString` - current date for validation
- `Config.Misc.cd` - change to project root before running external tools
- `Config.Misc.arxivAbstractRegexps` / `arxivAbstractFixedRewrites` - arXiv cleanup patterns

---

## Integration Points

### Upstream (callers)

- **LinkMetadata.hs:499** - `getMetadataItem` calls `linkDispatcher` for unknown URLs
- **LinkMetadata.hs** imports `gwern` directly for forced re-scrapes

### Downstream (callees)

- **Metadata.Format** - URL canonicalization, abstract cleaning, DOI processing
- **Metadata.Author** - author validation and canonicalization
- **Metadata.Date** - date guessing and validation
- **Metadata.Title** - title extraction and cleaning
- **LinkAuto** - auto-linking in abstracts
- **Paragraph** - paragraph splitting in abstracts
- **Typography** - typographic cleanup

### External Tools

- `exiftool` - PDF metadata extraction
- `curl` - HTTP fetching (arXiv, bioRxiv, gwern.net)
- `static/build/italicizer.py` - title italicization
- `openReviewAbstract.sh` - OpenReview scraping

---

## Adding a New Scraper

1. Create `Annotation/NewSource.hs` with signature:
   ```haskell
   newSource :: Metadata -> Path -> IO (Either Failure (Path, MetadataItem))
   ```

2. Import in `Annotation.hs`:
   ```haskell
   import Annotation.NewSource (newSource)
   ```

3. Add guard clause in `linkDispatcherURL` (order matters):
   ```haskell
   | "newsource.com/" `isInfixOf` l = newSource md l
   ```

4. Return `Left Temporary` for transient failures, `Left Permanent` for permanent ones

5. Use standard cleaners: `cleanAbstractsHTML`, `cleanAuthors`, `trimTitle`

---

## See Also

- [LinkMetadata.hs](/backend/link-metadata-hs) - Calls linkDispatcher, manages the metadata database
- [LinkMetadataTypes.hs](/backend/link-metadata-types-hs) - Core type definitions (Failure, MetadataItem)
- [Annotation/Arxiv.hs](/backend/annotation-arxiv-hs) - arXiv paper scraper
- [Annotation/Biorxiv.hs](/backend/annotation-biorxiv-hs) - bioRxiv/medRxiv scraper
- [Annotation/OpenReview.hs](/backend/annotation-openreview-hs) - OpenReview conference paper scraper
- [Annotation/PDF.hs](/backend/annotation-pdf-hs) - Local PDF metadata extraction
- [Annotation/Gwernnet.hs](/backend/annotation-gwernnet-hs) - Local gwern.net page scraper
- [popups.js](/frontend/popups-js) - Frontend popup system that displays annotations
- [extracts.js](/frontend/extracts-js) - Pop-frame extraction and rendering
