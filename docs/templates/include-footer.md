---
sidebar_position: 1
---

# footer.html

**Path:** `include/footer.html` | **Language:** HTML | **Lines:** ~17

Site-wide footer component with navigation controls and dynamic daily content.

## Overview

The `footer.html` include file defines the bottom-of-page footer that appears on every page of gwern.net. It combines sequential navigation controls with a collection of dynamic "X of the Day" widgets that showcase different aspects of the site's content. The footer serves both functional (navigation) and discovery (daily features) purposes.

The footer is structured into two main sections: a navigation bar with left/center/right controls using decorative arabesque SVG icons, and a content area containing an anonymous feedback link plus four dynamically-loaded daily features (Quote, Site, Annotation, and Adblock PSA). Each daily feature is loaded asynchronously via the site's transclusion system.

All dynamic content elements use the `.include` class, which triggers the transclusion system to fetch and inject content from separate HTML fragments. This allows the daily content to be updated without regenerating all pages.

## Content Structure

### Navigation Bar (`#navigation`)

- **Left/Right Navigation SVGs**: Sequential navigation arrows using arabesque ornamental icons from `/static/img/ornament/sequential-nav-icons-arabesque.svg`
- **Center Control**: "Return to top" anchor (`#navigation-center`) linking to `#top` with `.link-page-not` class to prevent it being treated as an internal page link
- Uses SVG sprite technique with `<use href>` for efficient icon loading

### Footer Content (`#footer`)

Located within `.markdownBody` div for consistent styling:

**Anonymous Feedback Link** (`#footer-anonymous-feedback`):
- Google Forms link for submitting anonymous feedback
- Uses hair-space characters (`&hairsp;`) for typographic spacing around link text

**X of the Day Section** (`#x-of-the-day`):
- Container with `.dark-mode-invert` class for theme compatibility
- Contains three daily feature links:
  - **Quote of the Day** (`#qotd`): Links to `/metadata/today-quote.html`
  - **Site of the Day** (`#sotd`): Links to `/metadata/today-site.html`
  - **Annotation of the Day** (`#atod`): Links to `/metadata/today-annotation.html`
- All use `.include .include-spinner-not` classes for async loading without spinner

**Adblock PSA** (`#psa-adblock`):
- Conditional adblock-related message loaded from `/metadata/psa-adblock.html`
- Uses `.include .adsense .include-spinner-not` classes

## Integration

### Build Process

The footer is included via Server-Side Includes (SSI) in the Hakyll-generated page templates. The SSI directive appears in the base page template:

```html
<!--#include virtual="/include/footer.html" -->
```

This is processed by nginx's SSI module when pages are served, allowing the footer to be updated independently of the main site build process.

### JavaScript Integration

The footer interacts with several frontend systems:

**Transclusion System** (`transclude.js`):
- Elements with `.include` class are processed by the transclusion module
- Content is fetched from specified `href` URLs and injected into the link element
- The `.include-spinner-not` class prevents loading spinners from appearing during content fetch

**Sequential Navigation** (`sidenotes.js` or navigation module):
- The `#navigation-left` and `#navigation-right` SVG elements are populated dynamically with previous/next page links based on the current page context
- Typically handled by JavaScript that detects sequential relationships from site metadata

**Dark Mode** (`dark-mode.js`):
- The `.dark-mode-invert` class on `#x-of-the-day` triggers CSS filter inversion when dark mode is active
- Ensures daily content remains readable across light/dark themes

---

## See Also

- [default.html](/templates/default) - Main page template that includes the footer via SSI
- [include-sidebar](/templates/include-sidebar) - Companion site-wide navigation component
- [include-inlined-head](/templates/include-inlined-head) - Critical CSS/JS inlined in head
- [transclude.js](/frontend/transclude-js) - Handles `.include` element content loading
- [dark-mode.js](/frontend/dark-mode-js) - Theme system that processes `.dark-mode-invert`
- [content.js](/frontend/content-js) - Content type system for handling loaded fragments
- [gwern.net.conf](/nginx/gwern-net-conf) - Nginx config that processes SSI includes
