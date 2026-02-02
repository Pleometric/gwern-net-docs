---
sidebar_position: 3
---

# initial.css

**Path:** `css/initial.css` | **Language:** CSS | **Lines:** ~2,615

Core layout, typography, and critical above-the-fold styles loaded inline in the document head.

---

## Overview

This is gwern.net's most important stylesheet, containing the foundational CSS that defines the site's visual identity. It's inlined in the `<head>` of every page to ensure instant rendering of critical above-the-fold content without a flash of unstyled content (FOUC).

The file is substantial (~2600 lines) and covers the essential visual structure: the layout grid, typography system, semantic heading hierarchy, responsive breakpoints, link underlining, the table of contents, page metadata block, and the distinctive /index page layout. It also includes utility classes, accessibility features, and mobile adaptations.

Key design decisions encoded here include the responsive line-height system (1.45-1.60 depending on viewport width), justified text on desktop (ragged-right on mobile), oldstyle numerals, sophisticated link underlining using text-shadow to skip descenders, and a comprehensive heading system with small-caps and borders.

## Key Selectors/Variables

### CSS Variables
- `--GW-serif-font-stack`: Source Serif 4 + fallbacks (main body font)
- `--GW-sans-serif-font-stack`: Source Sans 3 + fallbacks (UI elements)
- `--GW-monospaced-font-stack`: IBM Plex Mono + fallbacks
- `--GW-body-text-font-size`: 20px (desktop), 18px (mobile ≤649px)
- `--GW-body-max-width`: 935px
- `--GW-body-side-padding`: 20px (desktop), 16px (mobile)
- `--line-height`: Responsive (1.45→1.50→1.55→1.60 across 4 breakpoints)
- `--text-indent`: 1.75em (default), 2.5em (main content on desktop)
- `--text-alignment`: justify (desktop), left (mobile)
- `--text-hyphenation`: auto

### Layout
- `html`, `body`: Full-height, zero margin/padding
- `main`: Flexbox column, centered, max-width constraint, padding
- `#sidebar`: Top navigation bar with logo and links
- `article`: Main content container
- `header`: Page title block
- `#page-metadata`: Metadata flex layout with responsive breaks

### Typography
- `.markdownBody`: Main content area with justified text, oldstyle numerals, hyphenation
- `.heading`: Section headings (H1-H6) with specific sizing/styling
  - H1: Right-aligned, small-caps, 1.75-2em, underlined
  - H2: Uppercase, 1.4em, dotted underline
  - H3-H6: Progressive size reduction
- `blockquote`: 3-level nested emphasis with scaling font size
- `.list`: Custom bullet markers (stars) and numbered list styling

### Links
- `a`: Sophisticated underlining using `background-image` + `text-shadow` to skip descenders
- `.markdownBody a`: Tufte CSS-style underline implementation
- `.has-indicator-hook`: Annotated link markers (dog-ear triangles)
- `.inline-icon`: Inline SVG icons (moon, gear, manicule, etc.)

### Table of Contents
- `#TOC`: Collapsible, two-column on mobile, floating on desktop
- Wikipedia-style hierarchical numbering (1, 1.1, 1.1.1, etc.)
- Hover effects with indicator bar

### Special Features
- `.marginnote`: Margin notes (inline or popped-out sidenotes)
- `.sidenote`: Positioned margin notes on wide viewports (≥1497px)
- `#skip-to-content-link`: Accessibility skip link
- `.display-random-*`: Random content display system

### Page-Specific
- `body.page-index`: /index page with 2-3 column grid layout
- `body.page-404`: 404 page adjustments
- `body[class*='-index']`: Tag/directory index pages

## Loading

This stylesheet is **inlined** in the `<head>` of every page, making it part of the critical rendering path. This ensures that users see properly styled content immediately, without waiting for external CSS to download.

The inline strategy is justified by:
1. **Performance**: Eliminates render-blocking CSS request for critical styles
2. **Size**: While large (~2600 lines), it compresses well with gzip/brotli
3. **Caching**: The entire HTML (including inlined CSS) is cached, so repeat visitors get instant loads

---

## See Also

- [default](/css/default) - Non-critical styles loaded asynchronously
- [colors](/css/colors) - Color variable definitions
- [dark-mode-adjustments](/css/dark-mode-adjustments) - Dark mode style overrides
- [reader-mode-initial](/css/reader-mode-initial) - Reader mode styling
- [sidenotes-js](/frontend/sidenotes-js) - JavaScript that positions margin notes
- [dark-mode-js](/frontend/dark-mode-js) - Switches between light/dark color schemes
