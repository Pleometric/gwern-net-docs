---
sidebar_position: 3
---

# inlined-head.html

**Path:** `include/inlined-head.html` | **Language:** HTML/CSS | **Lines:** ~844

Critical CSS and JavaScript inlined in the document `<head>` for optimal initial page load.

## Overview

The `inlined-head.html` include file contains critical above-the-fold CSS and JavaScript that must be loaded before the page renders. It is included directly in the `<head>` section of every page to minimize render-blocking requests and provide immediate styling and functionality.

This file is structured around performance optimization: it inlines comprehensive CSS custom property definitions for the entire site's color scheme (including light and dark mode variants), critical layout styles, and essential JavaScript for early-stage page initialization. The inline approach eliminates additional network requests for these critical resources, reducing time-to-first-paint.

The file is particularly notable for its extensive CSS custom properties (CSS variables) system, which defines over 300 color tokens used throughout the site. This centralized theming system enables dark mode, popup styling, syntax highlighting, and all UI component colors to be managed from a single location.

## Content Structure

### Inline Styles (`#inlined-styles-colors`)

**Light Mode Color Scheme**:
- Comprehensive `:root` CSS custom properties defining the default (light) theme
- Categories include:
  - General (background, text)
  - Selection colors
  - Links (default, hover, visited, inverted variants)
  - Blockquotes (4 nesting levels)
  - Abstracts
  - Table of contents
  - Collapse blocks
  - Headings
  - Lists
  - Figures and embeds
  - Footnotes and sidenotes
  - Tables (including sortable header states, zebra striping)
  - Code blocks and syntax highlighting (full Pandoc/KDE color scheme)
  - Math blocks
  - Dropcaps (5 font variants)
  - Admonitions (note, tip, warning, error)
  - Footer styling
  - Popups and popins (including focus states)
  - Page toolbar
  - Reader mode
  - Accessibility features

**Pattern/Image References**:
- Secondary `:root` block defining pattern images used in UI elements
- References to data URIs or static image paths for popup title bars, scrollbars, checkerboard patterns

### Dark Mode Styles (`#inlined-styles-colors-dark`)

**Media Query Wrapper**:
```css
media="all and (prefers-color-scheme: dark)"
```

**Dark Mode Overrides**:
- Parallel structure to light mode colors
- Carefully adjusted values to maintain contrast ratios in dark environments
- Notable adjustments:
  - Background: `#000` → `#161616` (prevents pixel-off OLED issues)
  - Text: `#fff` → `#f1f1f1` (reduces stark contrast)
  - All color tokens recalculated for dark backgrounds

**Image Filters**:
- `.dark-mode-invert` utility class applies CSS filter inversion
- Complex filter rules for images, admonitions, SVG icons, table headers
- Hover state transitions for inverted images
- Special handling for `.invert`/`.invert-auto` classes on figures

**Component-Specific Dark Mode Rules**:
- Admonition icon filters
- Table sorter arrow icons (dark variants)
- Image inversion and grayscale effects
- Loading spinners
- SVG icon filters

### External Resource Preloading

**Critical CSS**:
```html
<link rel="stylesheet" href="/static/css/head.css?v=1766875259">
```

**Critical JavaScript**:
```html
<script src="/static/js/head.js?v=1766875259"></script>
```

**Icon Sprite Preload**:
```html
<link rel="preload" href="/static/img/icon/icons.svg?v=1765918942" as="image">
```
- Uses `rel="preload"` to fetch SVG sprite early in page load
- Icons are used throughout the page for UI elements

### Cache-Busting

All external resources use query string versioning (`?v=TIMESTAMP`) to ensure proper cache invalidation when files are updated.

## Integration

### Build Process

This file is included in the base page template's `<head>` section via SSI:

```html
<head>
  <!--#include virtual="/include/inlined-head.html" -->
  <!-- other head elements -->
</head>
```

The inlining happens at serve-time via nginx SSI, though the content itself is static and could be inlined at build-time for even better performance.

### CSS Architecture

**Custom Property Naming Convention**:
- Prefix: `--GW-` (Gwern Web)
- Structure: `--GW-{component}-{element}-{property}-{state}-{variant}`
- Examples:
  - `--GW-body-background-color`
  - `--GW-popups-popup-title-bar-button-focused-color-hover`
  - `--GW-table-sorted-column-heading-text-shadow-color`

**Theme Inheritance**:
- Dark mode overrides use the same variable names
- Components reference variables, not hard-coded colors
- Enables runtime theme switching beyond just media queries

### JavaScript Integration

**head.js Functions** (loaded by this file):
- Likely contains critical path initialization
- May handle:
  - Feature detection
  - Browser compatibility polyfills
  - Early event listeners
  - Namespace setup for `GW` object (see `initial.js`)

### Performance Considerations

**Why Inline?**:
- Eliminates render-blocking CSS request for critical styles
- Provides instant theming (no FOUC - Flash of Unstyled Content)
- Ensures dark mode is applied before first paint

**Trade-offs**:
- Increases HTML size (not cacheable at HTTP level)
- But HTML is served compressed, and styles are identical across pages
- nginx caching can mitigate repeated processing

---

## See Also

- [default.html](/templates/default) - Main page template that includes this via SSI
- [include-inlined-asset-links](/templates/include-inlined-asset-links) - Deferred non-critical assets
- [include-inlined-standalone](/templates/include-inlined-standalone) - Standalone page variant
- [dark-mode.js](/frontend/dark-mode-js) - Client-side dark mode control
- [colors.css](/css/colors) - CSS custom property definitions
- [initial.js](/frontend/initial-js) - Critical JavaScript initialization
- [gwern.net.conf](/nginx/gwern-net-conf) - Nginx config that processes SSI includes
