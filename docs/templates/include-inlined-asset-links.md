---
sidebar_position: 4
---

# inlined-asset-links.html

**Path:** `include/inlined-asset-links.html` | **Language:** HTML | **Lines:** ~2

Deferred stylesheet and JavaScript asset loading for non-critical resources.

## Overview

The `inlined-asset-links.html` include file defines the loading of non-critical CSS and JavaScript assets using performance optimization techniques. It contains only three lines of code but implements sophisticated loading strategies to minimize render-blocking resources and improve perceived page load performance.

This file is typically included near the end of the `<body>` or in a deferred section of the `<head>`, ensuring that critical rendering and interactivity are not blocked by large stylesheet and script downloads. The techniques used here represent modern best practices for progressive web performance.

## Content Structure

### Deferred Stylesheet Loading

```html
<link rel="stylesheet" href="/static/css/style.css?v=1766877006" media="print" onload="this.media=`all`">
```

**Loading Strategy**:
- Uses the `media="print"` attribute to prevent render-blocking behavior
- Browsers load print stylesheets with low priority and don't block rendering
- The `onload` handler switches `media` to `all` once the stylesheet loads
- This provides instant page rendering with progressive style enhancement

**Implications**:
- The page initially renders with only critical styles from `inlined-head.html`
- Non-critical styles (animations, complex layouts, decorative elements) load asynchronously
- Users see content immediately, with full styling appearing moments later
- Modern browsers optimize this pattern well, caching `style.css` after first load

### Deferred Script Loading

```html
<script src="/static/js/script.js?v=1767147992" defer></script>
```

**Loading Strategy**:
- Uses the `defer` attribute for optimal script loading behavior
- `defer` causes the script to:
  - Download in parallel with HTML parsing (non-blocking)
  - Execute after DOM is fully parsed
  - Execute in document order (relative to other deferred scripts)
  - Execute before `DOMContentLoaded` event fires

**Why `defer` over `async`**:
- `async` scripts execute as soon as downloaded, potentially out of order
- `defer` maintains execution order, critical for dependency chains
- Most gwern.net scripts likely depend on `GW` namespace setup in `head.js`
- Ensures modules load in correct sequence

### Cache-Busting

Both assets use query string versioning:
- `?v=1766877006` (CSS) - timestamp indicates build/deploy time
- `?v=1767147992` (JS) - different timestamp suggests separate update cycle
- Ensures browsers fetch fresh content when files change
- Allows aggressive caching (1 year+ cache headers) without staleness issues

## Integration

### Build Process

This file is included via SSI at the end of the page body:

```html
<body>
  <!-- page content -->
  <!--#include virtual="/include/inlined-asset-links.html" -->
</body>
```

Or potentially in the `<head>` after critical resources:

```html
<head>
  <!--#include virtual="/include/inlined-head.html" -->
  <!--#include virtual="/include/inlined-asset-links.html" -->
</head>
```

The exact placement depends on the base template structure but follows the principle of loading critical resources first.

### Performance Implications

**Critical Rendering Path**:
1. Browser parses HTML and encounters `inlined-head.html`
2. Critical CSS is applied immediately (inline styles)
3. `head.js` loads and executes synchronously
4. Page content begins rendering
5. `inlined-asset-links.html` triggers async downloads
6. `style.css` loads with low priority (media="print" trick)
7. `script.js` downloads in parallel with parsing
8. DOM parsing completes
9. `script.js` executes
10. `style.css` onload fires, switching to media="all"

**Metrics Impact**:
- **First Contentful Paint (FCP)**: Minimized by deferring non-critical styles
- **Time to Interactive (TTI)**: Improved by defer attribute on scripts
- **Largest Contentful Paint (LCP)**: Not blocked by stylesheet downloads
- **Cumulative Layout Shift (CLS)**: May increase slightly if deferred styles cause reflows

### CSS Architecture

The deferred `style.css` likely contains:
- Component-specific styles (popups, collapsible sections, tables)
- Layout styles (grid systems, responsive breakpoints)
- Animation and transition definitions
- Less-critical typographic enhancements
- Print-specific styles (ironically loaded via print media query trick)

Everything in `style.css` builds upon the CSS custom properties defined in `inlined-head.html`, ensuring consistent theming.

### JavaScript Architecture

The deferred `script.js` likely contains:
- Module implementations (popups, transclusion, annotations, etc.)
- Event handlers and user interaction logic
- Progressive enhancement features
- Analytics and tracking (if any)
- Dynamic content loading systems

The script depends on:
- `GW` namespace (defined in `head.js`)
- DOM APIs (guaranteed by `defer` execution timing)
- CSS custom properties (for dynamic styling)

---

## See Also

- [default.html](/templates/default) - Main page template that includes this via SSI
- [include-inlined-head](/templates/include-inlined-head) - Critical inline resources loaded before this
- [include-inlined-standalone](/templates/include-inlined-standalone) - Standalone page variant with bundled CSS
- [initial.js](/frontend/initial-js) - Main application JavaScript bundle
- [hakyll.hs](/backend/hakyll-hs) - Build system that generates versioned asset URLs
- [gwern.net.conf](/nginx/gwern-net-conf) - Nginx config with cache headers for assets
