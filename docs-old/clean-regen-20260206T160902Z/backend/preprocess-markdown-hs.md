
# preprocess-markdown.hs

**Path:** `build/preprocess-markdown.hs` | **Language:** Haskell | **Lines:** ~50

> Standalone preprocessor that transforms Markdown abstracts into annotated HTML with auto-links and recommendations

---

## Overview

`preprocess-markdown.hs` is a small, focused command-line utility that reads Markdown from stdin, applies a series of Pandoc AST transformations, and outputs enriched HTML. It serves as a preprocessing step for annotations and abstracts before they enter the main Hakyll build pipeline.

The module performs three key transformations: (1) interwiki link expansion (converting shorthand like `[foo](!W)` to full Wikipedia URLs), (2) automatic term hyperlinking via regex patterns defined in LinkAuto, and (3) generation of "See Also" recommendations using embedding-based similarity matching. It also validates Wikipedia links to catch broken references early in the pipeline.

This tool is designed for single-document processing (annotations, abstracts) rather than full pages. It's invoked during annotation creation workflows to ensure consistency between manually-written abstracts and auto-generated ones.

---

## Public API

### `main :: IO ()`

Entry point that orchestrates the preprocessing pipeline.

**Pipeline:**
1. Read Markdown from stdin
2. Parse to Pandoc AST with full Pandoc extensions
3. Apply `convertInterwikiLinks` transformation
4. Apply `linkAuto` transformation
5. Validate Wikipedia links via `checkWP`
6. Render to HTML5
7. Clean abstracts via `cleanAbstractsHTML`
8. Generate embedding-based recommendations
9. Output HTML with optional "See Also" section

**Called by:** Shell scripts, annotation workflows
**Calls:** `convertInterwikiLinks`, `linkAuto`, `checkWP`, `cleanAbstractsHTML`, `singleShotRecommendations`

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
│ linkAuto            │  Regex-based term linking
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

## Key Patterns

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

This is necessary because `singleShotRecommendations` needs to read the embeddings database, backlinks database, and metadata from specific file paths.

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

## Configuration

| Setting | Source | Purpose |
|---------|--------|---------|
| `pandocExtensions` | Pandoc | Full Markdown extension set |
| `safeHtmlWriterOptions` | Utils | HTML output settings |
| Interwiki prefixes | Config.Interwiki | Shorthand → URL mappings |
| LinkAuto patterns | Config.LinkAuto | Term → URL regex patterns |
| `C.cd` | Config.Misc | Working directory path |

---

## Integration Points

### Dependencies

| Module | Usage |
|--------|-------|
| `LinkMetadata` | `cleanAbstractsHTML` for output sanitization |
| `LinkAuto` | `linkAuto` for automatic term linking |
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
- Backlinks database
- Metadata database (`.gtx` file)

---

## See Also

- [hakyll.hs](/backend/hakyll-hs) - Main build system that uses similar transforms
- [sync.sh](/backend/sync-sh) - Build orchestrator that may invoke this tool
- [Typography.hs](/backend/typography-hs) - Typography transforms shared with hakyll
- [LinkAuto.hs](/backend/link-auto-hs) - Automatic term hyperlinking
- [Interwiki.hs](/backend/interwiki-hs) - Interwiki link expansion
- [GenerateSimilar.hs](/backend/generate-similar-hs) - Embedding-based recommendations
- [LinkMetadata.hs](/backend/link-metadata-hs) - Metadata and abstract handling
