---
sidebar_position: 4
---

# links.css

**Path:** `css/links.css` | **Language:** CSS | **Lines:** ~1,146

Comprehensive link icon and annotation indicator system for external and specialized links.

---

## Overview

This stylesheet implements gwern.net's distinctive link iconography system, which adds small visual indicators after links to denote their type, source domain, or file format. The philosophy is that link text often omits important context (e.g., whether a link is a PDF, a Wikipedia article, or a YouTube video), and compact icons can communicate this without interrupting the flow of text.

The file contains over 1100 lines defining two main systems: **annotation indicators** (the subtle "dog-ear" marks showing a link has popup content) and **link icons** (domain-specific or filetype icons). Icons can be either SVG-based graphical icons or textual icons using special Unicode characters or custom font rendering.

Link icons are styled to be subtle (50-90% opacity) and minimally intrusive, typically appearing as small marks to the right of the link. The system supports both monochrome icons (which invert in dark mode) and colored icons (which have hover variants).

## Key Selectors/Variables

### Annotation Indicators
- `.markdownBody a.has-indicator-hook`: Links with popup annotations
  - `.indicator-hook::before`: The "dog-ear" triangle using linear gradients
- `.markdownBody a.has-annotation`: Dotted underline for fully annotated links
- `.markdownBody a.has-annotation-partial`: Dotted underline for partial annotations
- `a[data-link-icon-color]:hover`: Colored hover state for link icons

### Link Icon System
- **Common base styles**:
  - `.markdownBody a.has-icon`: Base spacing adjustments
  - `a[data-link-icon-type*='svg']`: Graphical SVG icons (default size 0.55em, opacity 0.55)
  - `a[data-link-icon-type*='text']`: Textual/Unicode icons (default size 0.75em, opacity 0.83)

- **Icon variants**:
  - `[data-link-icon-type*='quad']`: 4-letter square icons (larger, 0.75-0.85em)
  - `[data-link-icon-type*='tri']`: 3-letter initials (0.65em)
  - `[data-link-icon-type*='sans']`, `[data-link-icon-type*='mono']`: Font family variants
  - `[data-link-icon-type*='bold']`, `[data-link-icon-type*='italic']`: Weight/style variants
  - `[data-link-icon-type*='overline']`: Overlined text icons

### Domain-Specific Icons

The file defines icons for 100+ domains and services, including:

**Textual/Logotype Icons**:
- `a[data-link-icon='𝛘']`: arXiv (chi symbol)
- `a[data-link-icon='n']`: Nature
- `a[data-link-icon='s']`: Slate
- `a[data-link-icon='W']`: Wikipedia (not defined here but referenced)
- `a[data-link-icon='♡']`: Fandom/Wikia
- `a[data-link-icon='≈']`: Similar links

**Graphical Icons** (SVG):
- `a[data-link-icon='wikipedia']`: Wikipedia "W" puzzle globe
- `a[data-link-icon='github']`: GitHub octocat
- `a[data-link-icon='youtube']`: YouTube play button
- `a[data-link-icon='pdf']`: PDF document icon (notably small: 0.45em desktop, 0.6em mobile)
- `a[data-link-icon='twitter']`: Twitter/X bird
- `a[data-link-icon='reddit']`: Reddit alien
- `a[data-link-icon='amazon']`: Amazon arrow-smile
- `a[data-link-icon='openai']`: OpenAI logo
- `a[data-link-icon='anthropic']`: Anthropic logo
- `a[data-link-icon='deepmind']`: DeepMind logo
- `a[data-link-icon='internet-archive']`: Archive.org logo
- Many more (see /lorem#link-icons for visual reference)

**Special Categories**:
- News organizations (NYT, Guardian, Washington Post, etc.)
- Academic publishers (Nature, Science, PLOS, etc.)
- AI companies (OpenAI, Anthropic, DeepMind, etc.)
- Tech platforms (GitHub, Stack Exchange, Substack, etc.)
- File formats (PDF, video icons, etc.)

### Custom Properties
Each icon uses CSS custom properties for fine-tuned positioning:
- `--link-icon-size`: Icon dimensions
- `--link-icon-offset-x`: Horizontal offset from link text
- `--link-icon-offset-y`: Vertical offset (baseline alignment)
- `--link-icon-opacity`: Default opacity
- `--link-icon-url`: SVG data URI or background image
- `--link-icon-font`: Font family for textual icons
- `--link-icon-font-weight`: Weight for textual icons

## Loading

This file is loaded **asynchronously** after initial page render as part of the non-critical CSS bundle. Link icons are a progressive enhancement—links are fully functional without them, so delaying their load doesn't hurt usability.

The icons are generated server-side by the Haskell build system (LinkIcon.hs module), which analyzes URLs and assigns appropriate `data-link-icon` attributes. This CSS then styles those attributes.

## See Also

- [links.css (Frontend)](/frontend/links-css) - Frontend documentation for the link icon system
- [LinkIcon.hs](/backend/link-icon-hs) - Server-side Haskell module that assigns link icons
- [Config.LinkIcon](/backend/config-link-icon-hs) - URL-to-icon mapping rules and color constants
- [build_icon_sprite_file.php](/php/build-icon-sprite-file) - Generates the SVG sprite sheet
- [default.css](/css/default) - Main stylesheet that imports link styles
- [extracts.js](/frontend/extracts-js) - Handles popup annotations that the indicator system marks
