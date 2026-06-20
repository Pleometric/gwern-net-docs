---
title: "link-tooltip.hs"
description: "Archived batch tool that extracts citation metadata from HTML link tooltips and updates the annotation database"
---

# link-tooltip.hs

Archived batch tool that extracts citation metadata from HTML link tooltips and updates the annotation database

<div className="doc-meta">
  <div><strong>Path</strong><code>build/old/link-tooltip.hs</code></div>
  <div><strong>Language</strong>Haskell</div>
  <div><strong>Lines</strong>46</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/old/link-tooltip.hs">build/old/link-tooltip.hs</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the Haskell build pipeline, generators, metadata code, or backend utility behavior around link-tooltip.
</div>

## Overview

`link-tooltip.hs` is a command-line utility that mines metadata from the title attributes (tooltips) of HTML anchor tags. When authors write Markdown links with tooltips in a citation-like format (e.g., `'Title', Author 2020`), this tool parses those strings and backfills empty annotation database entries with the extracted title, author, and date.

The tool serves as a data recovery mechanism: rather than requiring manual annotation entry, it leverages the human-readable citation hints already embedded in link tooltips. It operates in batch mode, processing a single Markdown/HTML file and updating all four annotation databases (`me.gtx`, `full.gtx`, `half.gtx`, `auto.gtx`).

The parsing logic is conservative—it only upgrades metadata entries that have empty titles, and prefers longer author/date strings when merging with existing data. Tooltips that look like costs (`$5`), interwiki prefixes (`!W`), Bitcoin amounts (`₿20`), or contain "Original URL:" markers are ignored.

---

## Public API

### `main :: IO ()`

Entry point. Takes a single filepath argument, extracts all link tooltips from that file, parses them into metadata triplets, then walks all annotation databases applying upgrades.

**Called by:** Command line / build scripts
**Calls:** `extractMetadataCandidates`, `walkAndUpdateLinkMetadata`, `upgradeMetadata`

---

### `extractAnchorTooltips :: Pandoc -> [(String, String)]`

Queries a Pandoc AST for all `Link` nodes that have non-empty tooltips. Returns a list of (URL, tooltip) pairs.

**Called by:** `extractMetadataCandidates`
**Calls:** Pandoc's `queryWith`

---

### `upgradeMetadata :: M.Map String (String,String,String) -> (String, MetadataItem) -> IO (String, MetadataItem)`

Transformation function for `walkAndUpdateLinkMetadata`. Given a lookup map of URL → (title, author, date) and an existing metadata entry, returns an upgraded entry if:
1. The existing title is empty
2. The URL exists in the lookup map

When merging, it keeps the longer of the existing vs. new author/date strings.

**Called by:** `main` (via `walkAndUpdateLinkMetadata`)
**Calls:** None (pure lookup and comparison)

---

### `extractMetadataCandidates :: String -> IO [(String, (String,String,String))]`

Reads a file, parses it as Markdown or HTML, extracts all tooltips, converts each to metadata via `tooltipToMetadata`, filters out entries with empty titles, and returns the sorted list.

**Called by:** `main`
**Calls:** `parseMarkdownOrHTML`, `extractAnchorTooltips`, `tooltipToMetadata`

---

## Internal Architecture

### Data Flow

```
Input File (Markdown/HTML)
         │
         ▼
    parseMarkdownOrHTML
         │
         ▼
  extractAnchorTooltips
   [(url, tooltip), ...]
         │
         ▼
    tooltipToMetadata (for each)
   [(url, (title, author, date)), ...]
         │
         ▼
    Filter empty titles
         │
         ▼
    Build lookup Map
         │
         ▼
walkAndUpdateLinkMetadata
   (updates all 4 .gtx files)
```

### MetadataItem Tuple Structure

The 7-tuple structure used throughout:
```haskell
(title, author, date, dateCreated, keyValuePairs, tags, abstract)
```

The upgrade function only touches the first three fields (title, author, date).

---

<details className="generated-section">
<summary>Key Patterns</summary>

### Conservative Merge Strategy

The `upgradeMetadata` function uses a "prefer longer" heuristic for author and date fields:

```haskell
m y z = if length y > length z then y else z
```

This prevents overwriting detailed existing data (e.g., `"John Smith"`) with abbreviated tooltip data (e.g., `"Smith"`).

### Tooltip Parsing Regex Pipeline

The actual parsing happens in `Metadata.Title.tooltipToMetadata`, which uses a series of `sed` regex substitutions to extract:
- **Title**: Content between curly quotes (`'...'` or `"..."`)
- **Author**: Text after the closing quote, before the year
- **Date**: 4-digit year with optional suffix (e.g., `2020a`, `2020-06-12`)

Example inputs and outputs:
```
"'My Title', Smith 2020"         → ("My Title", "Smith", "2020")
"'My Title', Smith & Jones 2020" → ("My Title", "Smith, Jones", "2020")
"'My Title', Smith et al 2020a"  → ("My Title", "Smith, et al", "2020")
```

### Filtering Invalid Tooltips

Tooltips starting with these characters are skipped entirely:
- `/` – Relative paths
- `!` – Interwiki prefixes
- `$` – Dollar amounts
- `₿` – Bitcoin amounts

Also skipped: tooltips containing `"Original URL:"` (archive markers).

---
</details>

<details className="generated-section">
<summary>Configuration</summary>

No configuration files. Behavior is hardcoded in the parsing logic.

The minimum length thresholds are:
- Title: 6 characters minimum
- Date: 4 characters minimum

These prevent spurious single-letter or short string matches from being treated as valid metadata.

---
</details>

## Integration Points

### Dependencies

| Module | Purpose |
|--------|---------|
| [LinkMetadata](link-metadata-hs) | `walkAndUpdateLinkMetadata` for batch database updates |
| [Annotation](annotation-hs) | Re-exports `tooltipToMetadata` from Metadata.Title |
| Metadata.Title | Core `tooltipToMetadata` parsing logic |
| Query | `parseMarkdownOrHTML` for file parsing |

### Database Files Modified

Updates all four annotation databases:
- `metadata/me.gtx` – Personal annotations
- `metadata/full.gtx` – Full annotations
- `metadata/half.gtx` – Partial annotations
- `metadata/auto.gtx` – Auto-generated annotations

### Usage in Build Pipeline

Typically invoked on Markdown files that contain citation-style tooltips:

```bash
runghc build/link-tooltip.hs path/to/file.md
```

---

<details className="generated-section">
<summary>See Also</summary>

- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database manager
- [linkTitler](/backend/link-titler-hs) - Related tool that adds titles to links (reverse direction)
- [Annotation.hs](/backend/annotation-hs) - URL-to-scraper dispatcher (re-exports tooltipToMetadata)
- [LinkID.hs](/backend/link-id-hs) - Citation ID generation for metadata entries
- [link-prioritize.hs](/backend/link-prioritize-hs) - Identifies links lacking annotations
- [Query.hs](/backend/query-hs) - Parses Markdown/HTML to extract tooltips
</details>
