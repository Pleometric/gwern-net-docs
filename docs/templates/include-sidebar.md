---
sidebar_position: 2
---

# sidebar.html

**Path:** `include/sidebar.html` | **Language:** HTML | **Lines:** ~12

Site-wide sidebar navigation component with logo and primary navigation links.

## Overview

The `sidebar.html` include file defines the persistent left-side navigation panel that appears on every page of gwern.net. It provides the site logo and a compact set of primary navigation links to key sections of the site. The sidebar is designed to be minimal, functional, and always accessible.

The sidebar contains two main components: the site logo (which doubles as a homepage link) and a vertical list of six navigation links covering the main areas of the site. The logo uses an inline SVG reference rather than a direct image embed, allowing for theme-aware styling and efficient caching via SVG sprites.

Some navigation links include semantic `rel` attributes: the logo has `rel="home me contents"`, Site/Me use `rel="author"`, and Patreon uses `rel="me"`; the remaining links have no `rel` attributes.

## Content Structure

### Container (`#sidebar`)

The root `<nav>` element uses the `#sidebar` ID and contains all sidebar elements.

### Logo Link

- **Element**: `<a class="logo" rel="home me contents" href="/index" accesskey="\">`
- **Purpose**: Homepage link with multiple semantic relationships (home page, author profile, site contents)
- **Accessibility**: Uses `accesskey="\"` for keyboard shortcut access
- **SVG Logo**: Inline SVG with viewBox definition referencing sprite at `/static/img/logo/logo-smooth.svg#logo`
  - ViewBox: `0 0 64 75` (aspect ratio preserved)
  - Uses `<use href>` pattern for efficient sprite-based loading

### Navigation Links Container (`.sidebar-links`)

Six primary navigation links, each with specific purposes:

1. **Site** (`/about`)
   - Classes: `.site .link-page .link-annotated`
   - Title: "Site ideals, source, content, traffic, examples, license"
   - Rel: `author` (indicates author information)
   - About page with site philosophy and technical details

2. **Me** (`/me`)
   - Classes: `.me .link-page .link-annotated`
   - Title: "Who am I online, what have I done, what am I like? Contact information; sites I use; things I've worked on"
   - Rel: `author` (personal profile)
   - Personal information and contact details

3. **New** (`/changelog`)
   - Classes: `.new .link-page .link-annotated`
   - Title: "Changelog of what's new or updated"
   - Site changelog and recent updates

4. **Blog** (`/blog/index`)
   - Classes: `.blog .link-page`
   - Title: "Shortform Gwern writings"
   - Blog section (note: not `.link-annotated`, suggesting different treatment)

5. **Links** (`/doc/newest/index`)
   - Classes: `.links .link-page`
   - Title: "Most recent link annotations"
   - Chronological list of annotated external links

6. **Patreon** (external link)
   - Classes: `.patreon`
   - URL: `https://www.patreon.com/gwern`
   - Title: "Link to Patreon donation profile to support my writing"
   - Rel: `me` (indicates ownership of Patreon profile)
   - Only external link in sidebar

## Integration

### Build Process

The sidebar is included via Server-Side Includes (SSI) in the base page template, similar to the footer:

```html
<!--#include virtual="/include/sidebar.html" -->
```

This SSI directive is processed by nginx when serving pages, allowing the sidebar to be updated without rebuilding the entire site.

### CSS Integration

The sidebar uses several CSS class patterns:

**Link Type Classes**:
- `.link-page`: Indicates internal page link (vs. external or fragment link)
- `.link-annotated`: Links that have associated popup annotations/previews
- `.logo`: Special styling for the logo link

**Component Classes**:
- `.sidebar-links`: Container for navigation links, likely receives flexbox or grid layout
- Individual semantic classes (`.site`, `.me`, `.new`, etc.): Allow per-link custom styling

### JavaScript Integration

**Link Annotation System** (`extracts.js`, `popups.js`):
- Links with `.link-annotated` class trigger popup previews on hover/tap
- The annotation system fetches and displays page abstracts, author information, and metadata

**Accessibility**:
- The `accesskey="\"` on the logo provides keyboard navigation
- The backslash key serves as a global "return to homepage" shortcut

**Dark Mode** (`dark-mode.js`):
- SVG logo can be styled differently in light/dark modes via CSS custom properties
- Logo sprite may include theme-specific variations

---

## See Also

- [default.html](/templates/default) - Main page template that includes the sidebar via SSI
- [include-footer](/templates/include-footer) - Companion bottom-of-page navigation
- [include-inlined-head](/templates/include-inlined-head) - Critical CSS/JS inlined in head
- [extracts.js](/frontend/extracts-js) - Powers link annotations and popups
- [popups.js](/frontend/popups-js) - Popup window system for link previews
- [LinkMetadata.hs](/backend/link-metadata-hs) - Backend annotation database
- [gwern.net.conf](/nginx/gwern-net-conf) - Nginx config that processes SSI includes
