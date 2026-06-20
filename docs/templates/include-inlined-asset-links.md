---
title: "inlined-asset-links.html"
description: "The inlined-asset-links.html include file defines deferred loading for the main non-critical stylesheet and JavaScript bundle."
---

# inlined-asset-links.html

The inlined-asset-links.html include file defines deferred loading for the main non-critical stylesheet and JavaScript bundle.

<div className="doc-meta">
  <div><strong>Path</strong><code>include/inlined-asset-links.html</code></div>
  <div><strong>Language</strong>HTML</div>
  <div><strong>Lines</strong>2</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/include/inlined-asset-links.html">include/inlined-asset-links.html</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the HTML/Pandoc templates and include fragments that shape rendered gwern.net pages around inlined-asset-links.
</div>

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

<details className="generated-section">
<summary>See Also</summary>

- [include-inlined-head](/templates/include-inlined-head) - Critical inline resources loaded before this
- [include-inlined-standalone](/templates/include-inlined-standalone) - Standalone page variant with bundled CSS
- [initial.js](/frontend/initial-js) - Entry point for JS initialization
- [gwern.net.conf](/nginx/gwern-net-conf) - Cache headers for static assets
</details>
