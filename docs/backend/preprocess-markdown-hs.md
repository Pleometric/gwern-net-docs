---
title: "preprocessMarkdown.hs"
description: "Standalone preprocessor that transforms Markdown abstracts into cleaned HTML with interwiki expansion and recommendations"
---

# preprocessMarkdown.hs

Standalone preprocessor that transforms Markdown abstracts into cleaned HTML with interwiki expansion and recommendations

<div className="doc-meta">
  <div><strong>Path</strong><code>build/app/preprocessMarkdown.hs</code></div>
  <div><strong>Language</strong>Haskell</div>
  <div><strong>Lines</strong>47</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/app/preprocessMarkdown.hs">build/app/preprocessMarkdown.hs</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the Haskell build pipeline, generators, metadata code, or backend utility behavior around preprocessMarkdown.
</div>

## Overview

`preprocessMarkdown.hs` is a small, focused command-line utility that reads Markdown from stdin, applies a series of Pandoc AST transformations, and outputs enriched HTML. It serves as a preprocessing step for annotations and abstracts before they enter the main Hakyll build pipeline.

The module performs three key operations: (1) interwiki link expansion, (2) Wikipedia link validation, and (3) generation of "See Also" recommendations using embedding-based similarity matching. The active source imports `LinkMetadata`, `Interwiki`, `GenerateSimilar`, `Utils`, `Config.Misc`, and `Query`.

This tool is designed for single-document processing (annotations, abstracts) rather than full pages. It's invoked during annotation creation workflows to ensure consistency between manually-written abstracts and auto-generated ones.

---

## Public API

### `main :: IO ()`

Entry point that orchestrates the preprocessing pipeline.

**Pipeline:**
1. Read Markdown from stdin
2. Parse to Pandoc AST with full Pandoc extensions
3. Apply `convertInterwikiLinks` transformation
4. Validate Wikipedia links via `checkWP`
5. Render to HTML5
6. Clean abstracts via `cleanAbstractsHTML`
7. Remove `class="link-live"` from the standalone output
8. Generate embedding-based recommendations
9. Output HTML with optional "See Also" section

**Called by:** Shell scripts, annotation workflows
**Calls:** `convertInterwikiLinks`, `checkWP`, `cleanAbstractsHTML`, `singleShotRecommendations`

---

## Internal Architecture

### Processing Pipeline

```
stdin (Markdown)
    │
    ▼
┌─────────────────────┐
│ readMarkdown        │  Parse with pandocExtensions
└─────────────────────┘
    │
    ▼
┌─────────────────────┐
│ convertInterwikiLinks│  !W, !G, etc. → full URLs
└─────────────────────┘
    │
    ▼
┌─────────────────────┐
│ checkWP             │  Validate Wikipedia links
└─────────────────────┘
    │
    ▼
┌─────────────────────┐
│ writeHtml5String    │  Render to HTML
└─────────────────────┘
    │
    ▼
┌─────────────────────┐
│ cleanAbstractsHTML  │  Sanitize output
└─────────────────────┘
    │
    ▼
┌─────────────────────┐
│ singleShotRecommendations │  Embedding similarity
└─────────────────────┘
    │
    ▼
stdout (HTML + See Also)
```

### Wikipedia Validation

```haskell
checkWP :: Pandoc -> IO ()
checkWP p = do
  let links = filter ("wikipedia.org"`T.isInfixOf`) $ extractURLs p
  mapM_ (isWPArticle True) links  -- Check existence
  mapM_ isWPDisambig links        -- Warn on disambiguation pages
```

This validation catches two common errors:
- Links to non-existent Wikipedia articles (typos, deleted pages)
- Links to disambiguation pages (should link to specific article)

---

<details className="generated-section">
<summary>Key Patterns</summary>

### Standalone Document Processing

Unlike the main Hakyll build which processes whole sites, this tool handles single documents in isolation:

```haskell
main = do
  originalMarkdown <- TIO.getContents  -- Single document from stdin
  -- ... process ...
  putStrLn html                         -- Single document to stdout
```

This design enables:
- Integration with Unix pipelines
- Use in annotation creation workflows
- Testing transformations on individual documents

### Working Directory Management

The tool explicitly sets the working directory before loading databases:

```haskell
C.cd  -- Ensure correct directory for metadata databases
matchList <- GS.singleShotRecommendations html
```

This is necessary because `singleShotRecommendations` reads the embeddings database and metadata from project-relative paths. In this single-shot mode, backlinks context is empty rather than loaded from the backlinks database.

### See Also Formatting

Recommendations are wrapped in a collapsible div for consistent styling:

```haskell
"<div class=\"aux-links-append see-also-append collapse\">\n\n" ++
"<p><strong>See Also</strong>:</p>\n\n" ++
matchList ++
"\n</div>"
```

The `collapse` class allows the recommendations to be hidden by default on pages where they might be distracting.

---
</details>

<details className="generated-section">
<summary>Configuration</summary>

| Setting | Source | Purpose |
|---------|--------|---------|
| `pandocExtensions` | Pandoc | Full Markdown extension set |
| `safeHtmlWriterOptions` | Utils | HTML output settings |
| Interwiki prefixes | Config.Interwiki | Shorthand → URL mappings |
| `C.cd` | Config.Misc | Working directory path |

---
</details>

## Integration Points

### Dependencies

| Module | Usage |
|--------|-------|
| `LinkMetadata` | `cleanAbstractsHTML` for output sanitization |
| `Interwiki` | `convertInterwikiLinks`, `isWPArticle`, `isWPDisambig` |
| `GenerateSimilar` | `singleShotRecommendations` for See Also generation |
| `Query` | `extractURLs` for Wikipedia link validation |

### Input/Output

- **Input:** Markdown text via stdin
- **Output:** HTML with optional See Also section via stdout
- **Side effects:** HTTP requests to Wikipedia API for link validation

### Database Access

Reads (via `singleShotRecommendations`):
- Embeddings database (`metadata/embeddings.bin`)
- Metadata database (`.gtx` file, for rendering matched entries)
- No backlinks database; single-shot recommendations use empty backlinks context

---

<details className="generated-section">
<summary>See Also</summary>

- [hakyll.hs](/backend/hakyll-hs) - Main build system that uses similar transforms
- [sync.sh](/backend/sync-sh) - Build orchestrator that may invoke this tool
- [Typography.hs](/backend/typography-hs) - Typography transforms shared with hakyll
- [Interwiki.hs](/backend/interwiki-hs) - Interwiki link expansion
- [GenerateSimilar.hs](/backend/generate-similar-hs) - Embedding-based recommendations
- [LinkMetadata.hs](/backend/link-metadata-hs) - Metadata and abstract handling
</details>
