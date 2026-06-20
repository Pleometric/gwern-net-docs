---
title: "generateLinkBibliography.hs"
description: "Generates per-page \"forward citation\" bibliographies listing all outbound links with their annotations"
---

# generateLinkBibliography.hs

Generates per-page "forward citation" bibliographies listing all outbound links with their annotations

<div className="doc-meta">
  <div><strong>Path</strong><code>build/app/generateLinkBibliography.hs</code></div>
  <div><strong>Language</strong>Haskell</div>
  <div><strong>Lines</strong>176</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/app/generateLinkBibliography.hs">build/app/generateLinkBibliography.hs</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the Haskell build pipeline, generators, metadata code, or backend utility behavior around generateLinkBibliography.
</div>

## Overview

This module creates link bibliographies—ordered lists of all outbound links from a page or annotation, displayed with their full annotations where available. Link bibliographies are the "forward citation" counterpart to backlinks: while backlinks show what points *to* a page, link bibliographies show what a page points *out to*.

The output is an HTML fragment (not a full page) designed for transclusion into popups and page metadata blocks. Each bibliography is written to `metadata/annotation/link-bibliography/$ESCAPED_PATH.html`. The fragment contains a numbered list with the header "Bibliography" and appends a count only when `N >= 2`.

A key design decision is separating Wikipedia links into a collapsed sub-list at the end. Wikipedia links are "so numerous, and so bulky" (per source comments) that they would overwhelm the primary bibliography. Links without full annotations still get entries—if a title exists, it's rendered as titlecased HTML; if the title is empty, the path is shown in code styling, with optional context transclusion via `.include-block-context`.

---

## Public API

### `main :: IO ()`

Entry point. Loads metadata databases and generates bibliography fragments for all known pages/annotations in parallel.

**Called by:** Build system (sync.sh or Hakyll)
**Calls:** `readLinkMetadata`, `readArchiveMetadata`, `writeLinkBibliographyFragment`

---

### `writeLinkBibliographyFragment :: ArchiveMetadata -> Metadata -> FilePath -> IO ()`

Writes a link bibliography HTML fragment for a single page path if:
1. The path has a non-empty abstract in metadata
2. The original source is newer than the existing bibliography (or bibliography doesn't exist)
3. There are enough non-Wikipedia links (≥ `C.mininumLinkBibliographyFragment`)

**Called by:** `main` (via parallel map)
**Calls:** `getLinkBibLink`, `parseExtractCompileWrite`, `extractLinksFromPage`

---

## Internal Architecture

### Data Flow

```
Metadata DB
    │
    ▼
┌─────────────────────────────────────┐
│  For each path in metadata keys:    │
│  1. Check if update needed          │
│  2. Extract links from source       │
│  3. Look up annotations for links   │
│  4. Generate HTML bibliography      │
│  5. Write to link-bib path          │
└─────────────────────────────────────┘
    │
    ▼
metadata/annotation/link-bibliography/*.html
```

### Key Data Structures

**Link tuple:** `(String, String)` — URL and its context ID (hash anchor where it appears in the parent)

**Triplet:** `(String, String, MetadataItem)` — URL, context ID, and full annotation data

**Bibliography structure:**
```
Para [Link "Bibliography (N):"]
OrderedList [
  [primary items...],
  [Div.collapse [
    Para "Wikipedia Bibliography:",
    OrderedList [wikipedia items...]
  ]]
]
```

### Link Extraction Path

For essays (`/path` with no extension):
- Parse the `.md` file with Pandoc
- Extract all `(URL, ID)` pairs via `extractLinkIDsWith`
- Normalize `https://gwern.net/` to `/`

For annotations (external URLs, files):
- Parse the abstract field as Pandoc
- Extract links from the abstract text

---

<details className="generated-section">
<summary>Key Patterns</summary>

### Wikipedia Link Segregation

Wikipedia links are filtered out of the primary list and placed in a collapsed sub-section:

```haskell
let itemsWP      = filter (\(u,_,_) -> "https://en.wikipedia.org/wiki/" `isPrefixOf` u) items
    itemsPrimary = items \\ itemsWP
```

This prevents bibliographies from being dominated by encyclopedic reference links.

### Context Link Generation

Each bibliography item can include a "context link" pointing back to where the link appeared in the source:

```haskell
prefix = [Link ("",["id-not", "link-bibliography-context"],[])
              [Str "\8203"]  -- zero-width space
              (pathParent ++ "#" ++ ident, "Original context in page.")]
```

The zero-width space creates a clickable but visually minimal element.

### Unannotated Link Transclusion

For local links without annotations (like `/question#feynman`), the module generates an `.include-block-context` transclusion:

```haskell
transcludeTarget = [BlockQuote [Para [
    Link ("", ["backlink-not", "include-block-context", ...], [])
         [Span ... [Str "[Transclude the forward-link's context]"]]
         (f, "")]]]
```

This ensures even unannotated section links show useful context.

### Staleness Check

Updates are conditional on modification time:

```haskell
originalLastModified <- getModificationTime target
lbLastModified       <- getModificationTime path'
return (originalLastModified >= lbLastModified)
```

This prevents regenerating unchanged bibliographies.

---
</details>

<details className="generated-section">
<summary>Configuration</summary>

| Setting | Location | Effect |
|---------|----------|--------|
| `mininumLinkBibliographyFragment` | `Config.Misc` | Minimum non-Wikipedia links required to generate a bibliography |

---
</details>

## Integration Points

### Inputs

- **Metadata database** — Via `readLinkMetadata` from LinkMetadata.hs
- **Archive metadata** — Via `readArchiveMetadata` from LinkArchive.hs
- **Markdown sources** — Read directly from disk for essay pages

### Outputs

- **HTML fragments** — Written to `metadata/annotation/link-bibliography/*.html`
- Uses `writeUpdatedFile` for atomic updates with change detection

### Dependencies

| Module | Usage |
|--------|-------|
| `LinkBacklink` | `getLinkBibLink` for output path, `getAnnotationLinkCheck` for source path |
| `LinkMetadata` | `generateAnnotationTransclusionBlock` for full annotation rendering |
| `LinkID` | `metadataItem2ID` for generating stable link IDs |
| `Typography` | `typographyTransformTemporary`, `titlecase'` for text formatting |
| `Query` | `extractLinkIDsWith` for parsing links from Pandoc AST |
| `Interwiki` | `convertInterwikiLinks` for expanding shorthand links |
| `Inflation` | `isInflationURL` to filter out inflation adjustment links |

### Filters Applied

Links are excluded if they:
- Are self-references (link to the page being processed)
- Are inflation URLs
- Result in fewer than minimum required links (after excluding Wikipedia)

---

## Output Format

The generated HTML has this structure:

```html
<p><a class="icon-special" href="/design#link-bibliographies">
  <strong>Bibliography (42):</strong>
</a></p>
<ol>
  <li><!-- Primary link with annotation transclusion --></li>
  <li><!-- Another link --></li>
  ...
  <li>
    <div class="collapse">
      <p><strong>Wikipedia Bibliography:</strong></p>
      <ol>
        <li><!-- Wikipedia links --></li>
      </ol>
    </div>
  </li>
</ol>
```

Items with full annotations use `generateAnnotationTransclusionBlock` for rich display. Items without annotations show as:
- Code-styled path for external URLs
- Titlecased title as link text (if title exists)
- Optional `.include-block-context` blockquote for local anchors

---

<details className="generated-section">
<summary>See Also</summary>

- [hakyll.hs](/backend/hakyll-hs) - Main site generator using bibliography fragments
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database and transclusion blocks
- [sync.sh](/backend/sync-sh) - Build orchestrator that invokes this module
- [LinkBacklink.hs](/backend/link-backlink-hs) - Backlinks (the "reverse" of link bibliographies)
- [Annotation.hs](/backend/annotation-hs) - URL annotation dispatcher
- [Query.hs](/backend/query-hs) - Link extraction utilities
</details>
