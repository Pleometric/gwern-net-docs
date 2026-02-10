sidebar_position: 4
---

# inlined-asset-links.html

**Path:** `include/inlined-asset-links.html` | **Language:** HTML | **Lines:** ~2

Deferred stylesheet and JavaScript asset loading for non-critical resources.

## Overview

The `inlined-asset-links.html` include file defines deferred loading for the main non-critical stylesheet and JavaScript bundle. It is included after critical head resources so it does not block initial rendering.

## Content Structure

```html
<link rel="stylesheet" href="/static/css/style.css?v=..." media="print" onload="this.media='all'">
<script src="/static/js/script.js?v=..." defer></script>
```

### Loading Strategy

**Stylesheet:**
- Uses `media="print"` to avoid render-blocking.
- Switches to `media="all"` on load for progressive enhancement.

**Script:**
- Uses `defer` so download happens in parallel with HTML parsing.
- Executes after DOM parsing and before `DOMContentLoaded`.
- Preserves execution order relative to other deferred scripts.

### Cache-Busting

The `?v=` query string is a version timestamp to force cache refreshes when the bundle changes.

## Integration

Typical SSI placement:

```html
<body>
  <!-- page content -->
  <!--#include virtual="/include/inlined-asset-links.html" -->
</body>
```

## See Also

- [include-inlined-head](/templates/include-inlined-head) - Critical inline resources loaded before this
- [include-inlined-standalone](/templates/include-inlined-standalone) - Standalone page variant with bundled CSS
- [initial.js](/frontend/initial-js) - Entry point for JS initialization
- [gwern.net.conf](/nginx/gwern-net-conf) - Cache headers for static assets
