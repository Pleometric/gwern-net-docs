---
sidebar_position: 2
---

# default.css

**Path:** `css/default.css` | **Language:** CSS | **Lines:** ~7,569

The main stylesheet containing all non-critical component styles for gwern.net.

---

## Overview

This is gwern.net's largest stylesheet, a massive generated file with approximately 7,500 lines covering every component, widget, and interactive element not included in the critical inline CSS. It's the comprehensive "everything else" stylesheet that handles the full visual implementation of features like collapsible sections, admonitions, popups, sidenotes, footnotes, tables, code blocks, citations, media embeds, and countless other specialized components.

As a generated file, it's compiled from multiple source stylesheets and includes extensive CSS for:
- Interactive UI elements (collapse blocks, tabs, tooltips)
- Pop-frame system (popups and popins with complex title bars, scrollbars, controls)
- Typography refinements (smallcaps, dropcaps, citations, epigraphs)
- Content types (code blocks, math, tables, figures, blockquotes, lists)
- Navigation elements (page toolbar, back-to-top links, search UI)
- Special page types (directory indexes, 404 pages, newsletters)
- Accessibility features and mobile adaptations

Unlike `initial.css` which is inlined for instant rendering, `default.css` is loaded asynchronously after first paint, allowing the page to become interactive faster while deferring non-critical styling.

## Key Selector Groups

### Variables
- Popup/popin dimensions and constraints (min/max widths and heights for different content types)
- Page toolbar animation durations and filters
- Floating header indicators
- Search interface dimensions
- Color and pattern references

### Generic Elements
- `button`: Reset and base button styling (appearance: none, flexbox layout)

### Sections & Headings
- `.heading`: Pointer events and interactive behavior
- Section self-links (pilcrow ¶ links)
- Copy link buttons
- Heading hover states

### Collapse Blocks
- `.collapse`: Expandable/collapsible sections
- Disclosure buttons and indicators
- Animation states (expanded, collapsed, expanding, collapsing)
- Special variants: in-blockquotes, inline collapses, aux-links collapses
- Nested collapse handling

### Lists
- Continued list numbering
- Definition lists
- Footnote lists
- Multi-column list layouts

### Content Blocks
- Admonitions (note, tip, warning, error) with icons
- Epigraphs and poetry
- Data tables with sorting, scrolling, zebra striping
- Code blocks with syntax highlighting, line numbers, copy buttons
- Math rendering (MathJax/KaTeX)
- Figures and captions
- Blockquote nesting and styling

### Interactive Components
- **Pop-frames** (popups and popins):
  - Title bars with controls (pin, close, zoom, options)
  - Scrollbars (custom styled)
  - Resize handles
  - Loading states and spinners
  - Content-specific layouts (images, videos, annotations, Wikipedia, tweets)

- **Page Toolbar**:
  - Widgets (theme switcher, width adjuster, font size controls)
  - Collapse/expand behavior
  - Button states and animations
  - Mobile adaptations

- **Sidenotes**:
  - Positioning and layout
  - Sidenote columns
  - Overflow scrolling
  - Highlight on target

- **Footnotes**:
  - Backlinks
  - Footnote references
  - Highlighted states
  - Collapse controls for long footnote sections

### Typography Features
- Citations (academic citation formatting)
- Smallcaps (automatic and manual)
- Dropcaps (5 different decorative initial capital styles)
- Inflation adjustment indicators
- Text alignment utilities

### Media Embeds
- Images (including dark mode filtering, image focus/zoom)
- Videos (YouTube, Vimeo, local)
- Audio players
- PDFs and documents
- Tweet embeds
- Wikipedia entry embeds

### Navigation & UI Chrome
- Floating headers on mobile
- "Back to top" links
- Navigation bars
- Footer
- Logo brightness toggle
- Search interface

### Specialized Pages
- Directory indexes (`body[class*='-index']`)
- Tag pages
- Newsletter formatting
- 404 error page
- Lorem ipsum demo page

### Responsive Breakpoints
Multiple `@media` queries for:
- Mobile (max-width: 649px)
- Tablet (650-900px, 650-1179px)
- Desktop narrow (901-1496px)
- Desktop wide (≥1497px for sidenotes)
- Ultra-wide (≥1761px for expanded layouts)

## Loading

This file is loaded **asynchronously** after the initial page render via JavaScript-injected `<link>` tag or deferred loading. This is a performance optimization: by not blocking on this large CSS file, the page can render critical content faster.

The loading strategy:
1. Initial HTML + inline CSS renders immediately (first paint)
2. User sees content and can start reading
3. `default.css` loads in background
4. Advanced features progressively enhance as CSS applies

Because this is a generated file, it may include redundant or legacy styles that haven't been cleaned up. The build process concatenates multiple source CSS files into this single bundle.

---

## See Also

- [initial](/css/initial) - Critical inline styles loaded first
- [colors](/css/colors) - Color variable definitions
- [dark-mode-adjustments](/css/dark-mode-adjustments) - Dark mode overrides
- [reader-mode-initial](/css/reader-mode-initial) - Reader mode styles
- [popups-js](/frontend/popups-js) - JavaScript for popup window management
- [sidenotes-js](/frontend/sidenotes-js) - JavaScript for sidenote positioning
- [collapse-js](/frontend/collapse-js) - JavaScript for collapse block behavior
