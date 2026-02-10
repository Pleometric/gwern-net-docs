---
sidebar_position: 2
---

# navbar.html

**Path:** `include/navbar.html` | **Language:** HTML | **Lines:** ~13

Site-wide top navigation include with logo and primary section links.

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

## See Also

- [default.html](/templates/default) - Main page template that includes navbar
- [include-footer](/templates/include-footer) - Companion bottom-of-page include
- [special-occasions.js](/frontend/special-occasions-js) - Seasonal navbar/logo behaviors
- [initial.css](/frontend/initial-css) - Core navbar styling rules

