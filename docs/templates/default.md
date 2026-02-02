---
sidebar_position: 1
---

# default.html

**Path:** `template/default.html` | **Language:** HTML5/Pandoc | **Lines:** ~175

Main Hakyll template for all standard gwern.net pages.

## Overview

The `default.html` template is the primary HTML wrapper for all gwern.net content pages. It provides the complete HTML document structure, including the `<head>` section with extensive metadata (Dublin Core, OpenGraph, Twitter Cards, Google Scholar citations), Server-Side Include (SSI) directives for performance-critical resources, and the `<body>` layout with sidebar, article content, and footer.

This template is invoked for all regular pages (essays, essays, notes) but not for special cases like the 404 error page or placeholder pages. It implements a sophisticated metadata system that supports rich social media previews, academic citation, and SEO optimization. The template uses conditional Hakyll variables to customize behavior based on page type (e.g., displaying different metadata fields for the index page versus article pages).

The template heavily relies on SSI (Server-Side Includes) to inline critical CSS/JS and modular HTML components. This approach balances performance (inlining critical resources) with maintainability (keeping repeated components in separate files). The design assumes an nginx server configured to process SSI directives.

## Key Variables/Blocks

### Metadata Variables

- `$title-escaped$`, `$title-plain$`: Page title in escaped and plain formats
- `$author$`: Optional author name (defaults to "Gwern" if not specified)
- `$description-escaped$`: Meta description for SEO and social media
- `$thumbnail$`, `$thumbnail-height$`, `$thumbnail-width$`, `$thumbnail-text$`, `$thumbnail-css$`: OpenGraph image metadata
- `$url$`, `$safe-url$`, `$escaped-url$`: Various URL encodings for links and CSS classes
- `$created$`, `$modified$`: Publication and modification dates in ISO format
- `$tags-plain$`, `$tagsHTML$`: Keyword metadata and rendered tag list
- `$status-plus-progress$`, `$confidence-plus-progress$`, `$importance$`: Gwern's custom metadata fields
- `$css-extension$`: Additional CSS classes based on file extension
- `$date-range-HTML$`: Date range display for multi-year projects

### Conditional Flags

- `$if(index)$`: True for homepage only (enables Google site verification meta tag)
- `$if(error404)$`: True for 404 pages (adds noindex robots meta, loads 404-guesser.js)
- `$if(placeholder)$`: True for placeholder pages (hides footer, sets refMapTimestamp)
- `$if(backlinks-yes)$`, `$if(similars-yes)$`, `$if(linkbib-yes)$`: Enable respective sections

### SSI Blocks

- `<!--#include virtual="/static/include/inlined-head.html" -->`: Critical CSS/JS inlined for performance
- `<!--#include virtual="/static/include/inlined-asset-links.html" -->`: Preload/prefetch resource hints
- `<!--#include virtual="/static/include/sidebar.html" -->`: Navigation sidebar
- `<!--#include virtual="/static/include/footer.html" -->`: Site footer

### Content Blocks

- `$body$`: Main page content (comes from Pandoc-generated HTML, wrapped in `<div class="markdownBody">`)
- `$description$`: Rendered description block (shown in page metadata section)

## Usage

This template is applied to most pages in the Hakyll build pipeline:

```haskell
-- In hakyll.hs
match "*.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "template/default.html" defaultContext
        >>= relativizeUrls
```

The template is not used for:
- Syntax-highlighted source code previews (uses `pandoc/sourcecode.html5`)
- Raw Pandoc HTML output (uses `pandoc/template-html5-articleedit.html5`)
- Easter egg/special templates (`idealconditionsdonotexistandwillneverhappen.html`, `unfortunatelytheclockisticking.html`)

The template expects specific CSS class conventions:
- `page-$safe-url$`: Unique per-page class for custom styling
- `$css-extension$`: Extension-based styling (e.g., `.page-gwern-net-md`)
- `$page-created-recently$`: Styling for recently created pages

---

## See Also

- [hakyll.hs](/backend/hakyll-hs) - Hakyll build system that invokes this template
- [include-footer](/templates/include-footer) - Footer component included via SSI
- [include-sidebar](/templates/include-sidebar) - Sidebar navigation included via SSI
- [include-inlined-head](/templates/include-inlined-head) - Critical CSS/JS inlined in document head
- [include-inlined-asset-links](/templates/include-inlined-asset-links) - Deferred asset loading
- [sourcecode.html5](/templates/sourcecode) - Alternative template for syntax-highlighted code
- [template-html5-articleedit.html5](/templates/template-html5-articleedit) - Standalone Pandoc HTML5 template
- [gwern.net.conf](/nginx/gwern-net-conf) - Nginx config that processes SSI directives
