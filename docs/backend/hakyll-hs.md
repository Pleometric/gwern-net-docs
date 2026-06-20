---
title: "hakyll.hs"
description: "Hakyll-based static site generator entry point orchestrating Pandoc transforms and template rendering"
---

# hakyll.hs

Hakyll-based static site generator entry point orchestrating Pandoc transforms and template rendering

<div className="doc-meta">
  <div><strong>Path</strong><code>build/app/hakyll.hs</code></div>
  <div><strong>Language</strong>Haskell</div>
  <div><strong>Lines</strong>602</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/app/hakyll.hs">build/app/hakyll.hs</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the Haskell build pipeline, generators, metadata code, or backend utility behavior around hakyll.
</div>

## Overview

hakyll.hs is the main entry point for building gwern.net. It uses Hakyll (a Haskell static site generator) to compile Markdown files into HTML through a multi-stage pipeline: Hakyll parses the Markdown, Pandoc converts it to an AST, a chain of custom transforms enriches the AST, then Pandoc renders HTML which is wrapped in templates.

The file handles three main concerns: (1) pre-build setup including loading metadata databases and writing annotation fragments, (2) defining routing rules that control URL structure and file handling, and (3) the `pandocTransform` pipeline that applies 15+ AST transformations for typography, link annotations, archives, and more.

A key design decision is the `SLOW` environment variable toggle—expensive operations like running the full test suite only happen when `SLOW=true`. The build also supports targeted compilation via CLI arguments (e.g., `./hakyll build note/foo.md` to rebuild a single file).

---

## Public API

### `main :: IO ()`

Entry point. Loads databases, writes blog entries and annotations, then runs Hakyll's build system.

```haskell
main = do
  arg <- lookupEnv "SLOW"
  let slow = "true" == fromMaybe "" arg
  am   <- readArchiveMetadataAndCheck  -- Archive link database
  meta <- readLinkMetadataSlow         -- Annotation database
  sizes <- annotationSizeDB meta am    -- Size metadata
  writeOutBlogEntries meta
  withArgs [head args] $ hakyll $ do ...
```

**Called by:** sync.sh (via the `hakyll` executable)
**Calls:** readArchiveMetadataAndCheck, readLinkMetadataSlow, writeAnnotationFragments, pandocTransform

---

### `pandocTransform :: Metadata -> ArchiveMetadata -> String -> Pandoc -> IO Pandoc`

The core AST transformation pipeline. Applies all content transforms to a parsed Markdown document.

`SizeDB` is computed earlier for annotation fragment generation, but it is not passed into `pandocTransform`.

**Called by:** Hakyll's `pandocCompilerWithTransformM`
**Calls:** (in order)
1. `convertInterwikiLinks` - Expand interwiki syntax.
2. `footnoteAnchorChecker` - Fail the build on suspicious short spaceless footnotes (non-index pages only).
3. `createAnnotations` - Trigger annotation generation (non-index pages only).
4. `addPageLinkWalk` - Mark local links before annotation/archive rewrites.
5. `nominalToRealInflationAdjuster` - Adjust dollar amounts for inflation (non-index pages only).
6. `hasAnnotation` - Add annotation classes from metadata.
7. `localizeLink` - Rewrite to archived versions.
8. `typographyTransformTemporary` - Typography fixes.
9. `headerSelflinkAndSanitize` - Make headers self-linking and sanitize IDs.
10. `addPageLinkWalk` - Re-mark local links after header/archive rewrites.
11. `wrapInParagraphs` - Convert `Plain` blocks to paragraphs.
12. `imageLinkHeightWidthSet` - Add image dimensions.
13. `addCanPrefetch` - Mark prefetchable links.

---

### `postCtx :: Metadata -> String -> Context String`

Hakyll context providing template variables. This is where YAML frontmatter is processed and exposed to templates.

```haskell
postCtx md rts =
  fieldsTagPlain md <>
  fieldsTagHTML md <>
  titlePlainField "title-plain" <>
  descField True "title" "title-escaped" <>
  ...
  defaultContext
```

**Template Variables Provided:**
| Variable | Description |
|----------|-------------|
| `$title$` | Title with HTML formatting |
| `$title-plain$` | Plain text title for `<title>` |
| `$title-escaped$` | HTML-escaped for `<meta>` |
| `$description$` | Description with HTML |
| `$description-escaped$` | Escaped description rendered through Utext for Unicode-rich plain text |
| `$created$` | Creation date (YYYY-MM-DD) |
| `$modified$` | Last modified date |
| `$status$` | Writing status |
| `$confidence$` | Certainty level |
| `$importance$` | Topic importance (0-10 or N/A) |
| `$tagsHTML$` | Rendered tag links |
| `$tags-plain$` | Comma-separated tags |
| `$thumbnail$` | OG image path |
| `$safe-url$` | CSS-safe page identifier |
| `$escaped-url$` | URL-encoded path |
| `$backlinks-yes$` | Boolean: has backlinks? |
| `$similars-yes$` | Boolean: has similar links? |
| `$linkbib-yes$` | Boolean: has link bibliography? |
| `$page-created-recently$` | CSS class if within `C.isNewWithinNDays` days; currently 62 days |
| `$refMapTimestamp$` | Cache-busting timestamp for /ref/ |
| `$date-range-HTML$` | Formatted date range with duration |

---

### `woptions :: WriterOptions`

Pandoc writer configuration.

```haskell
woptions = defaultHakyllWriterOptions {
  writerSectionDivs = True,        -- Wrap sections in <section>
  writerTableOfContents = True,    -- Generate TOC
  writerTOCDepth = 4,              -- Include h1-h4
  writerHTMLMathMethod = MathJax defaultMathJaxURL,
  writerEmailObfuscation = NoObfuscation
}
```

The TOC is injected via a custom template that wraps content in `#TOC` and `#markdownBody` divs.

---

## Internal Architecture

### Build Flow

```
main
 ├── Load databases (archive, metadata, sizes)
 ├── Write blog entries
 └── hakyll $ do
      ├── preprocess: writeAnnotationFragments
      ├── preprocess: testAll (if SLOW=true)
      ├── preprocess: writeOutID2URLdb
      │
      ├── match "**.md" (excluding doc/www/**)
      │    ├── Route: strip .md, remove commas/apostrophes, spaces→hyphens
      │    ├── Compile: pandocCompilerWithTransformM + pandocTransform
      │    ├── Apply template: default.html
      │    └── Post-process: imgUrls (add dimensions)
      │
      ├── match static files (metadata/**)
      │    └── copyFileCompiler
      │
      ├── match static files (doc/**, **.hs, **.css, etc.)
      │    └── symlinkFileCompiler (custom forked Hakyll)
      │
      └── match "template/*.html"
           └── templateCompiler
```

**Note:** The `symlinkFileCompiler` is a custom optimization requiring a forked Hakyll installation. Instead of copying files, it creates symlinks to save disk space and I/O. See [hakyll#786](https://github.com/jaspervdj/hakyll/issues/786).

### Template System

Templates live in `template/` and use Hakyll's `$variable$` syntax with conditionals:

```html
$if(author)$
<meta name="author" content="$author$">
$else$
<meta name="author" content="Gwern">
$endif$
```

The main template `default.html` (186 lines) handles:
- HTML `<head>` with SEO metadata
- Body class assignment (`page-$safe-url$ $css-extension$`)
- Article wrapper with title and metadata block
- Backlinks/similar links sections (conditional)
- Footer and analytics

---

<details className="generated-section">
<summary>Key Patterns</summary>

### Index Page Detection

Essays with `index: True` in frontmatter take a lighter transform path. They still run interwiki conversion, but skip footnote checking, annotation creation, and inflation adjustment:

```haskell
let indexp = indexp' == "True"
let pw = if indexp then convertInterwikiLinks p
         else walk footnoteAnchorChecker $ convertInterwikiLinks p
```

### Safe ID Generation

Headers are checked for invalid CSS characters (periods, colons, hashes) which would break JS/CSS selectors:

```haskell
headerSelflinkAndSanitize x@(Header a (href,b,c) d) =
  let href' = T.filter (`notElem` ['.', '#', ':']) href in
    when (href' /= href) $ error "Invalid ID..."
```

### Progress Field Decoration

Status/confidence fields get visual progress indicators via `completionProgressHTML`:

```haskell
progressField "status" "status-plus-progress" <>
progressField "confidence" "confidence-plus-progress"
```

### Duplicate Header Detection

Warns at build time if two top-level headers have identical text:

```haskell
duplicateTopHeaders :: Pandoc -> [String]
duplicateTopHeaders = duplicates . query topHeaderTexts
```

### Plain→Para Normalization

HTML/Markdown roundtripping can produce `Plain` blocks instead of `Para` blocks in list items, causing `<li>text</li>` instead of `<li><p>text</p></li>`. Since gwern.net doesn't intentionally use `Plain` anywhere, a blanket rewrite converts all `Plain` to `Para`:

```haskell
wrapInParagraphs :: Pandoc -> Pandoc
wrapInParagraphs = walk go
  where go (Plain strs) = Para strs
        go x = x
```

### Ref Map Timestamp

The template context includes `$refMapTimestamp$` set to the most recently modified file in `metadata/annotation/id/`. This is used for cache-busting the `/ref/` annotation lookup system.

---
</details>

<details className="generated-section">
<summary>Configuration</summary>

### Environment Variables

| Variable | Effect |
|----------|--------|
| `SLOW=true` | Run full test suite, expensive transforms |

### CLI Arguments

```bash
./hakyll build                    # Build all .md files
./hakyll build note/foo.md        # Build single file
./hakyll build --annotation-rebuild       # Rewrite all annotations
./hakyll build --annotation-missing-one-shot  # Write missing, then exit
./hakyll watch                    # Dev server with live reload
./hakyll clean                    # Remove _site/ and _cache/
```

### YAML Frontmatter Fields

Required:
- `title` - Page title (can include HTML like `<em>`)
- `created` - ISO date (YYYY-MM-DD)
- `status` - Writing stage: abandoned/notes/draft/in progress/finished
- `description` - Page summary

Optional:
- `modified` - Last edit date
- `confidence` - Certainty validated by `C.yamlValidConfidences` (`certain`, `highly likely`, `likely`, `possible`, `unlikely`, `highly unlikely`, `remote`, `impossible`, `log`, `emotional`, `fiction`)
- `importance` - Topic significance: 0-10 or N/A
- `author` - If not Gwern
- `thumbnail` - OG image path
- `thumbnail-text` - Alt text for thumbnail
- `thumbnail-css` - CSS classes for thumbnail
- `css-extension` - Additional body classes (e.g., `dropcaps-de-zs`)
- `tags` - List of tag slugs
- `index` - `True` for tag directories (skips some transforms)
- `error404` - `True` for the 404 page (adds noindex, loads guesser JS)
- `placeholder` - Skips footer SSI

---
</details>

## Integration Points

### Databases Read

| Database | Loader | Purpose |
|----------|--------|---------|
| Archive metadata | `readArchiveMetadataAndCheck` | URL→local path mappings |
| Link metadata | `readLinkMetadataSlow` | Annotation data (title, author, abstract) |
| Size DB | `annotationSizeDB` | File sizes for links |

### Files Written

| File | Writer | Purpose |
|------|--------|---------|
| Annotation fragments | `writeAnnotationFragments` | `/metadata/annotation/*.html` |
| Blog entries | `writeOutBlogEntries` | Newsletter content |
| ID→URL mapping | `writeOutID2URLdb` | `/ref/` cache |

### Modules Called

The local custom imports include:
- **Image**: dimensions, optimization
- **Inflation**: dollar adjustment
- **Interwiki**: `[WP:...](WP:...)` expansion
- **LinkArchive**: archive.org localization
- **LinkBacklink**: backlink detection
- **LinkMetadata**: annotation system
- **LinkMetadataTypes**: metadata and size database types
- **Tags**: tag rendering
- **Typography**: text polish
- **Utils**: helpers
- **Test**: test suite
- **Config.Misc**: date utilities
- **Metadata.Author**: author cleanup and linking
- **Metadata.Date**: date range formatting
- **LinkID**: ID mapping
- **Blog**: newsletter generation
- **Utext**: Unicode-rich plain text conversion

---

<details className="generated-section">
<summary>See Also</summary>

- [sync.sh](/backend/sync-sh) - Build orchestrator that invokes hakyll.hs
- [preprocess-markdown.hs](/backend/preprocess-markdown-hs) - Standalone Markdown preprocessor
- [Typography.hs](/backend/typography-hs) - AST typography transforms
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database manager
- [Utils.hs](/backend/utils-hs) - Shared utility functions
- [Test.hs](/backend/test-hs) - Test suite run during SLOW builds
</details>
