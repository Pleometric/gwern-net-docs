---
sidebar_position: 4
---

# pandoc/sourcecode.html5

**Path:** `template/pandoc/sourcecode.html5` | **Language:** HTML5/Pandoc | **Lines:** ~27

Minimal Pandoc template for syntax-highlighted source code previews.

## Overview

The `sourcecode.html5` template is a stripped-down Pandoc HTML5 template used specifically for generating syntax-highlighted previews of source code files. Unlike the full `default.html` Hakyll template, this template provides minimal scaffolding - just enough HTML structure to display Pandoc's syntax-highlighted code with proper styling.

This template is used when gwern.net needs to display raw source code files (e.g., linking to `.js`, `.py`, `.hs` files) in a readable, syntax-highlighted format rather than as plain text. The template relies on Server-Side Includes (SSI) to inject shared CSS/JS resources via `/static/include/inlined-standalone.html`, ensuring consistent styling with the rest of the site.

The template is intentionally minimal to keep the focus on the code itself. It includes basic metadata (author, date) support from Pandoc variables but doesn't include the full metadata apparatus of `default.html`.

## Key Variables/Blocks

### Metadata Variables

- `$pagetitle$`: Page title (typically the filename), displayed as "/$pagetitle$ (syntax-highlighted preview)"
- `$author-meta$`: Optional author metadata (loops with `$for(author-meta)$`)
- `$date-meta$`: Optional date metadata

### Conditional Blocks

- `$if(dir)$ dir="$dir$"$endif$`: Text direction attribute for internationalization
- `$if(date-meta)$`: Conditionally includes Dublin Core date metadata

### CSS/Asset Loading

- `$for(css)$`: Loops through CSS files to include
  - Each CSS file rendered as: `<link rel="stylesheet" href="$css$">`
- SSI block: `<!--#include virtual="/static/include/inlined-standalone.html" -->`
  - Provides inlined critical CSS/JS for standalone pages
  - Ensures syntax highlighting styles are available

### Content Block

- `$body$`: Pandoc-generated syntax-highlighted HTML
  - Contains the actual code with `<pre><code>` structure
  - Pandoc applies language-specific CSS classes for highlighting

## Usage

This template is invoked by Pandoc (not Hakyll) for generating syntax-highlighted source code previews:

```bash
# Hypothetical usage in build system
pandoc source.js \
    --template=template/pandoc/sourcecode.html5 \
    --syntax-highlighting \
    --standalone \
    --output=source.js.html
```

Or within Hakyll:

```haskell
-- For source code preview routes
match "**.js" $ do
    route $ setExtension "html"
    compile $ getResourceBody
        >>= loadAndApplyTemplate "template/pandoc/sourcecode.html5" ctx
```

The template is specifically designed for:
1. **Source file previews**: Making `.js`, `.hs`, `.py`, `.sh` files viewable in-browser with syntax highlighting
2. **Code snippet pages**: Standalone pages that are just code (no prose)
3. **Embedded code viewers**: Content transcluded into popups or iframes

### Body Class Convention

The template applies a special body class `file-preview-source-code` to distinguish these pages from regular content pages. This allows CSS to apply specific styling:

```css
body.file-preview-source-code {
    /* Monospace-optimized layout */
    font-family: monospace;
    max-width: none; /* Allow wide code */
    padding: 1em;
}
```

---

## See Also

- [default.html](/templates/default) - Main Hakyll page template for regular content
- [template-html5-articleedit.html5](/templates/template-html5-articleedit) - Full-featured Pandoc HTML5 template
- [include-inlined-standalone](/templates/include-inlined-standalone) - SSI resource injection for standalone pages
- [hakyll.hs](/backend/hakyll-hs) - Build system that compiles source code previews
- [initial.js](/frontend/initial-js) - JavaScript initialization for syntax highlighting
- [colors.css](/css/colors) - CSS custom properties for syntax highlighting colors
