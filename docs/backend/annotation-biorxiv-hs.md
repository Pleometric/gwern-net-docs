---
title: "Annotation.Biorxiv"
description: "Scrapes metadata from bioRxiv and medRxiv preprint pages using Dublin Core meta tags"
---

# Annotation.Biorxiv

Scrapes metadata from bioRxiv and medRxiv preprint pages using Dublin Core meta tags

<div className="doc-meta">
  <div><strong>Path</strong><code>build/Annotation/Biorxiv.hs</code></div>
  <div><strong>Language</strong>Haskell</div>
  <div><strong>Lines</strong>59</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/Annotation/Biorxiv.hs">build/Annotation/Biorxiv.hs</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the Haskell build pipeline, generators, metadata code, or backend utility behavior around Annotation.Biorxiv.
</div>

## Overview

This module extracts bibliographic metadata from bioRxiv and medRxiv preprint pages. Both preprint servers share the same codebase and embed Dublin Core metadata in HTML `<meta>` tags, which this module parses using TagSoup after fetching the page via curl.

The scraper extracts title, authors, date, DOI, and abstract from the HTML. It handles several bioRxiv-specific quirks: broken quote encoding in abstracts (`'9s` -> `'s`), and two different tag formats for abstracts (`citation_abstract` vs `DC.Description`). The extracted abstract is cleaned and optionally split into paragraphs by the LLM-based paragraphizer.

This module is one of several domain-specific scrapers under `Annotation/` that feed into the central `Annotation.linkDispatcher` routing system.

---

## Public API

### `biorxiv :: Metadata -> Path -> IO (Either Failure (Path, MetadataItem))`

Main entry point. Fetches and parses a bioRxiv/medRxiv URL, returning structured metadata.

```haskell
biorxiv md "https://www.biorxiv.org/content/10.1101/2020.04.03.024554.full"
-- → Right ("https://www.biorxiv.org/content/...", ("Title", "Authors", "2020-04-03", "", [("doi","10.1101/...")], [], "<p>Abstract...</p>"))
```

**Called by:** `Annotation.linkDispatcherURL` (when URL matches `https://www.biorxiv.org/content/` or `https://www.medrxiv.org/content/`)

**Calls:**
- `runShellCommand` — curl fetch
- `parseTags` — HTML parsing
- `cleanAuthors` — author name normalization
- `processDOI` — DOI cleanup
- `cleanAbstractsHTML` — abstract HTML cleanup
- `processParagraphizer` — LLM-based paragraph splitting

**Returns:**
- `Right (url, MetadataItem)` — success with 7-tuple: (title, author, date, created, [(key,value)], tags, abstract)
- `Left Permanent` — parse failure (title empty)
- `Left Temporary` — abstract empty after processing

---

## Internal Architecture

### Data Flow

```
URL → curl fetch → TagSoup parse → filter <meta> tags → extract by name attribute → clean/transform → MetadataItem
```

### Key Internal Functions

#### `parseMetadataTagsoup :: String -> [Tag String] -> [String]`

Extracts values from meta tags where the `name` attribute matches the key. Returns the `content` attribute value from the first attribute position.

```haskell
-- For: <meta name="DC.Title" content="Paper Title">
parseMetadataTagsoup "DC.Title" metas  -- → ["Paper Title"]
```

#### `parseMetadataTagsoupSecond :: String -> [Tag String] -> [String]`

Similar to above but reads from the second attribute position. Used for `citation_abstract` which has `lang` before `content`.

```haskell
-- For: <meta name="citation_abstract" lang="en" content="Abstract text">
parseMetadataTagsoupSecond "citation_abstract" metas  -- → ["Abstract text"]
```

#### `safeKeyList` / `safeKeyList2`

Low-level extractors that pattern match on `TagOpen` with attribute lists. Error on malformed tags (partial function).

### Dublin Core Tags Extracted

| Meta Name | Field | Notes |
|-----------|-------|-------|
| `DC.Title` | title | Required; empty triggers parse failure |
| `DC.Contributor` | author | Multiple tags joined with `, ` |
| `DC.Date` | date | Publication date |
| `DC.Description` | abstract | Fallback if `citation_abstract` empty |
| `citation_abstract` | abstract | Primary source (second attr position) |
| `citation_doi` | DOI | Stored in key-value list |

---

<details className="generated-section">
<summary>Key Patterns</summary>

### PDF URL Short-Circuit

PDF URLs are immediately returned with empty metadata rather than fetched:

```haskell
if ".pdf" `isInfixOf` p then return (Right (p, ("", "", "", "", [], [], "")))
```

This avoids downloading PDFs when the abstract page should be used instead.

### Broken Quote Encoding Workaround

BioRxiv has a known bug where apostrophes in abstracts are encoded as `'9` (e.g., "patient's" → "patient'9s"). The code works around this:

```haskell
replace "9s" "s"  -- BUG: BioRxiv abstracts have broken quote encoding
```

This is noted as reported but unfixed for 2+ years.

### Fallback Abstract Source

The scraper tries `citation_abstract` first (using the second attribute parser), then falls back to `DC.Description`:

```haskell
let abstractRaw' = if not (null abstractRaw) then abstractRaw
                   else concat $ parseMetadataTagsoup "DC.Description" metas
```

---
</details>

<details className="generated-section">
<summary>Configuration</summary>

No direct configuration. Behavior is controlled by:

- **User-Agent:** Hardcoded to `gwern+biorxivscraping@gwern.net` for curl requests
- **Abstract processing:** Delegated to `cleanAbstractsHTML` (config in `Config.Metadata.Format`)
- **Abstract processing:** Delegated to `cleanAbstractsHTML` and `processParagraphizer`

---
</details>

## Integration Points

### Events / State

None. Pure scraper with no global state or event system.

### Module Dependencies

**Imports:**
- `LinkMetadataTypes` — `Failure`, `MetadataItem`, `Path`, `Metadata` types
- `Metadata.Format` — `checkURL`, `cleanAbstractsHTML`, `processDOI`
- `Metadata.Author` — `cleanAuthors`
- `Metadata.Format` — `cleanAbstractsHTML`
- `Paragraph` — `processParagraphizer`
- `Utils` — `printRed`, `replace`

**External:**
- `curl` — fetched via `runShellCommand`
- `Text.HTML.TagSoup` — HTML parsing

### URL Routing

Triggered by `Annotation.linkDispatcherURL` for URLs matching:
```haskell
anyPrefix l ["https://www.biorxiv.org/content/", "https://www.medrxiv.org/content/"]
```

### Output Format

Returns standard `MetadataItem` 7-tuple:
```haskell
(title, author, date, "", [("doi", doi)], [], abstract)
--  ^      ^      ^    ^        ^          ^      ^
--  |      |      |    |        |          |      Cleaned HTML with <p> tags
--  |      |      |    |        |          Tags (empty, TODO for ML)
--  |      |      |    |        Key-value pairs
--  |      |      |    Date created (empty, set later)
--  |      |      Publication date
--  |      Comma-separated authors
--  Title string
```

---

<details className="generated-section">
<summary>See Also</summary>

- [Annotation.hs](/backend/annotation-hs) - Parent dispatcher that routes URLs to this scraper
- [LinkMetadata.hs](/backend/link-metadata-hs) - Stores extracted annotations in the metadata database
- [LinkMetadataTypes.hs](/backend/link-metadata-types-hs) - Core type definitions (Failure, MetadataItem)
- [Annotation/Arxiv.hs](/backend/annotation-arxiv-hs) - Similar scraper for arXiv (uses XML API)
- [Annotation/OpenReview.hs](/backend/annotation-openreview-hs) - Similar scraper for OpenReview
- [Metadata/Format.hs](/backend/metadata-format-hs) - cleanAbstractsHTML and processDOI utilities
- [Paragraph.hs](/backend/paragraph-hs) - LLM-based paragraph splitting for abstracts

---
</details>
