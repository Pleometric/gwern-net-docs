---
sidebar_position: 5
---

# inlined-standalone.html

**Path:** `include/inlined-standalone.html` | **Language:** HTML/CSS | **Lines:** ~345

Standalone page variant that bundles all CSS resources inline for single-file portability.

## Overview

The `inlined-standalone.html` include file is an alternative to `inlined-asset-links.html` designed for "standalone" pages that need to be fully self-contained or function independently of the main site infrastructure. Instead of loading stylesheets via `<link>` elements, this variant inlines all CSS directly into the HTML document.

This approach is used for pages that may be:
- Archived or distributed as single HTML files
- Viewed offline without server access
- Embedded in contexts where external resource loading is unreliable
- Distributed via email or saved locally by users
- Used in reader modes or print views

The trade-off is increased HTML file size in exchange for complete portability and guaranteed styling without network dependencies.

## Content Structure

### Inline Critical Styles

```html
<style id="inlined-styles-colors">
  /* Full CSS custom property definitions - identical to inlined-head.html */
</style>
```

This section is identical to the first `<style>` block in `inlined-head.html`, containing:
- Complete `:root` custom property definitions
- All color tokens for light mode
- Pattern/image variable references

### External Critical Resources

```html
<link rel="stylesheet" href="/static/css/head.css?v=1766877006">
```

Unlike `inlined-head.html`, this variant loads `head.css` as an external resource rather than inlining it. This suggests:
- `head.css` is less critical for standalone pages
- Or standalone pages may have a different performance profile
- The file might be cached in standalone contexts where external requests are acceptable

```html
<link rel="stylesheet" href="/static/css/style.css?v=1766877006">
```

**Key Difference from `inlined-asset-links.html`**:
- No `media="print"` trick for async loading
- Loaded synchronously, will block rendering
- Ensures complete styling before page displays
- Acceptable trade-off for standalone/offline contexts where progressive loading is less critical

### Standalone Context Implications

The lack of performance optimizations (media query tricks, defer attributes) suggests this variant prioritizes:
1. **Completeness** over speed
2. **Reliability** over optimization
3. **Offline functionality** over progressive enhancement

## Integration

### Build Process

This include is used conditionally in page templates:

```html
<head>
  <!--#include virtual="/include/inlined-head.html" -->

  <!-- Conditional logic (likely in Hakyll template): -->
  <!-- If standalone page: -->
  <!--#include virtual="/include/inlined-standalone.html" -->

  <!-- Else: -->
  <!--#include virtual="/include/inlined-asset-links.html" -->
</head>
```

The decision of which variant to use is made during the Hakyll build process based on page metadata or configuration flags.

### Use Cases

**Standalone Pages**:
- Archive pages (e.g., `/doc/*/index` pages that collect many articles)
- Single-article distributions
- Print/PDF generation targets
- Email newsletter versions
- Offline documentation

**Why Bundle Everything**:
- No dependency on CDN or server availability
- Survives link rot and server migrations
- Works in restricted network environments
- Ensures visual consistency in uncontrolled contexts

### Performance Characteristics

**Advantages**:
- Guaranteed complete styling on first render
- No FOUC (Flash of Unstyled Content)
- No network requests for stylesheets after initial load
- Predictable rendering behavior

**Disadvantages**:
- Larger HTML file size (CSS is not separately cacheable)
- Slower initial download
- Duplicated CSS across multiple standalone pages (no caching benefit)
- Synchronous stylesheet loading blocks rendering

### Comparison with Other Includes

| Include File | CSS Loading | Use Case | Performance Profile |
|--------------|-------------|----------|---------------------|
| `inlined-head.html` | Minimal inline + external | Normal pages | Optimized for speed |
| `inlined-asset-links.html` | Deferred external | Normal pages | Progressive enhancement |
| `inlined-standalone.html` | Full inline + sync external | Standalone pages | Optimized for portability |

---

## See Also

- [default.html](/templates/default) - Main page template for regular pages
- [include-inlined-head](/templates/include-inlined-head) - Critical inline resources
- [include-inlined-asset-links](/templates/include-inlined-asset-links) - Deferred loading variant for regular pages
- [sourcecode.html5](/templates/sourcecode) - Source code template that uses standalone includes
- [hakyll.hs](/backend/hakyll-hs) - Static site generator that determines page variants
- [colors.css](/css/colors) - CSS custom properties bundled inline
