---
title: "navbar.html"
description: "navbar.html defines the persistent navigation bar included on standard pages."
sidebar_position: 2
---

# navbar.html

navbar.html defines the persistent navigation bar included on standard pages.

<div className="doc-meta">
  <div><strong>Path</strong><code>include/navbar.html</code></div>
  <div><strong>Language</strong>HTML</div>
  <div><strong>Lines</strong>13</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/include/navbar.html">include/navbar.html</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the HTML/Pandoc templates and include fragments that shape rendered gwern.net pages around navbar.
</div>

## Overview

`navbar.html` defines the persistent navigation bar included on standard pages.

It contains:

- Logo link to `/index`
- Internal links to `about`, `me`, `changelog`, `blog`, and `doc/newest`
- External links to Patreon and Substack

The root container is `nav#navbar`, with links grouped under `.navbar-links`.

## Structure

### Container

- Root element: `<nav id="navbar">`

### Logo

- Anchor class: `.logo`
- SVG sprite reference: `/static/img/logo/logo-smooth.svg#logo`
- Keyboard access key: `\`

### Link Group

Inside `.navbar-links`:

- `.site` -> `/about`
- `.me` -> `/me`
- `.new` -> `/changelog`
- `.blog` -> `/blog/index`
- `.links` -> `/doc/newest/index`
- `.patreon` -> `https://www.patreon.com/gwern`
- `.mail` -> `https://gwern.substack.com/`

## Integration

`template/default.html` includes it via SSI:

```html
<!--#include virtual="/static/include/navbar.html" -->
```

Related code paths:

- `css/initial.css` styles `#navbar` and `.navbar-links`
- `js/extracts-load.js` includes `#navbar` in content container selectors
- `js/special-occasions.js` targets `#navbar .logo-image` for seasonal logo swaps

---

<details className="generated-section">
<summary>See Also</summary>

- [default.html](/templates/default) - Main page template that includes navbar
- [include-footer](/templates/include-footer) - Companion bottom-of-page include
- [special-occasions.js](/frontend/special-occasions-js) - Seasonal navbar/logo behaviors
- [initial.css](/frontend/initial-css) - Core navbar styling rules
</details>
