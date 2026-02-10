---
sidebar_position: 5
---

# pandoc/template-html5-articleedit.html5

**Path:** `template/pandoc/template-html5-articleedit.html5` | **Language:** HTML5/Pandoc | **Lines:** ~77

Standard Pandoc HTML5 template for generating standalone HTML articles with minimal gwern.net branding.

## Overview

This is a modified version of Pandoc's default HTML5 template (`template-html5.html5`) used for generating standalone HTML articles outside the main Hakyll build process. It provides a complete, self-contained HTML document with Pandoc's full feature set, including table of contents, math rendering (MathJax), syntax highlighting, and metadata support.

Unlike `default.html` (the Hakyll template), this template is used by Pandoc directly for conversion tasks that need standard HTML output without gwern.net's custom page structure. It's suitable for generating articles that can be viewed independently, shared externally, or published to other platforms. The template includes MathJax for mathematical notation, making it appropriate for technical/academic content.

The template follows Pandoc's conventions for variable names and structure, making it compatible with standard Pandoc workflows and documentation. It supports both modern and legacy browsers (includes HTML5 shiv for IE8 compatibility).

## Key Variables/Blocks

### Document Metadata

- `$pagetitle$`: Raw page title (used in `<title>` element)
- `$title-prefix$`: Optional prefix for title (rendered as "prefix – title")
- `$title$`: Formatted title (rendered in `<h1>` within header)
- `$subtitle$`: Optional subtitle
- `$author$`: Author name(s) (supports multiple with `$for(author)$` loop)
- `$date$`: Publication date
- `$author-meta$`: Author in meta tag format
- `$date-meta$`: Date in `dcterms.date` format
- `$keywords$`: Comma-separated keywords for meta tag

### HTML Attributes

- `$dir$`: Text direction (e.g., "rtl" for right-to-left languages)
- `$lang$`: Language code (defaults to "en")

### Styling and Scripts

- `$highlighting-css$`: Pandoc-generated CSS for syntax highlighting
- `$for(css)$`: Loops through external CSS files to link
- `$for(header-includes)$`: Custom header content injection point
- `$math$`: Math rendering configuration (MathJax)
  - Hardcoded: `<script src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js">`

### Content Blocks

- `$for(include-before)$`: Content to inject before article body
- `$body$`: Main article content (Pandoc-generated HTML)
- `$for(include-after)$`: Content to inject after article body
- `$toc$`: Enables table of contents generation
- `$table-of-contents$`: Rendered TOC HTML
- `$idprefix$`: Prefix for element IDs (for TOC anchors)

### Legacy Browser Support

- Conditional IE8 comment with HTML5 shiv:
  ```html
  <!--[if lt IE 9]>
    <script src="//cdnjs.cloudflare.com/ajax/libs/html5shiv/3.7.3/html5shiv-printshiv.min.js"></script>
  <![endif]-->
  ```

## Usage

This template is invoked directly by Pandoc for generating standalone HTML from Markdown:

```bash
# Basic usage
pandoc input.md \
    --template=template/pandoc/template-html5-articleedit.html5 \
    --standalone \
    --mathjax \
    --toc \
    --output=output.html

# With metadata
pandoc input.md \
    --template=template/pandoc/template-html5-articleedit.html5 \
    --standalone \
    --metadata title="My Article" \
    --metadata author="Gwern Branwen" \
    --metadata date="2025-01-07" \
    --output=output.html
```

Or within Hakyll for specific compilation routes:

```haskell
-- For standalone exports or special pages
compile $ pandocCompilerWith readerOptions writerOptions
    >>= loadAndApplyTemplate "template/pandoc/template-html5-articleedit.html5" ctx
```

### Typical Use Cases

1. **Standalone article exports**: Generating self-contained HTML versions of essays for archiving or external publication
2. **Email newsletters**: Creating HTML for email distribution
3. **Print-friendly versions**: Simplified HTML without site navigation
4. **Academic paper output**: Converting LaTeX/Markdown to HTML with proper math rendering
5. **Testing and development**: Quick HTML previews during writing

### Inline Styles

The template includes minimal inline CSS for basic formatting:

```css
code { white-space: pre-wrap; }
span.smallcaps { font-variant: small-caps; }
span.underline { text-decoration: underline; }
div.column { display: inline-block; vertical-align: top; width: 50%; }
```

It also handles quotation marks based on `$if(quotes)$`:
```css
q { quotes: """ """ "'" "'"; }
```

---

## See Also

- [default.html](/templates/default) - Main Hakyll site template with full gwern.net structure
- [sourcecode.html5](/templates/sourcecode) - Minimal template for syntax-highlighted code
- [include-inlined-standalone](/templates/include-inlined-standalone) - Resource bundling for standalone pages
- [hakyll.hs](/backend/hakyll-hs) - Hakyll build system that may invoke this template
- [Typography.hs](/backend/typography-hs) - Text processing applied before templating
- [gwern.net.conf](/nginx/gwern-net-conf) - Server config for serving standalone pages
