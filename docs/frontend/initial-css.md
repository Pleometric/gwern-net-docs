
# initial.css

**Path:** `css/initial.css` | **Language:** CSS | **Lines:** ~2,616

> Core critical-path CSS for above-the-fold rendering and fundamental typography

---

## Overview

`initial.css` is the foundational stylesheet for gwern.net, loaded synchronously in the critical rendering path to ensure proper display of above-the-fold content before JavaScript executes. This ~2,600-line file establishes the site's visual identity through sophisticated typography, responsive layout systems, and carefully tuned typographic details.

The file handles everything needed for initial page render: font stacks, color variables (via CSS custom properties), the navigation sidebar, page headers and metadata blocks, table of contents, main content typography (headings, paragraphs, blockquotes, lists), figures and captions, margin notes, and extensive page-specific customizations for `/index`, `/404`, and other special pages.

Key design philosophy: progressive enhancement with mobile-first responsive breakpoints, extensive use of CSS custom properties for theming, and meticulous attention to typographic details like oldstyle numerals, hyphenation, text justification, and link underlining with descender-aware "skip-ink" simulation.

---

## Key Sections

### Variables & Configuration (lines 1–23)
- **CSS Custom Properties** in `:root`: Font stacks, body text size, max-width, side padding
- **Mobile breakpoint adjustments** (`@media max-width: 649px`): Reduced font size and padding

### General & Layout (lines 24–98)
- **Base styles** for `html`, `body`, `main` container
- **Flexbox main layout**: centered column with max-width, responsive padding
- **Adaptiveness utilities**: `.mobile-not` and `.desktop-not` display classes

### Sidebar Navigation (lines 99–245)
- **Logo positioning**: absolute positioning on wide screens (≥1180px), inline on narrow
- **Link styling**: dotted borders, small-caps on mobile, uppercase on desktop
- **Responsive logo sizing**: 4em absolute-positioned on wide, 1em inline on medium, 2.5em on mobile
- **Flexbox link layout** with manual ordering on mobile

### Article Container & Page Header (lines 246–294)
- **Header** with centered h1, small-caps, responsive sizing (2.5em → 2em on mobile)
- **Typography**: Source Serif 4 font-variant-caps, negative letter-spacing

### Page Metadata Block (lines 295–510)
- **Centered layout** with interpunct separators (middle dot `·`)
- **Flexbox metadata fields** with explicit ordering (date/author/status → confidence/importance → backlinks/similar/bibliography)
- **Link tags** in monospaced italic
- **Page description** in italic with nested em normalization
- **Complex responsive breakpoints** with different layouts for mobile vs. desktop

### Table of Contents (lines 511–959)
- **Collapsible TOC** with toggle button, SVG icon rotation animation
- **Two-column layout** on mobile (≤900px) with vertical divider line
- **Wikipedia-style hierarchical numbering** (1, 1.1, 1.2.1, etc.) using CSS counters up to 6 levels
- **Hover effects**: background color change, indicator bar on right edge
- **Special handling** for directory index pages (`.TOC-links-only`)
- **Tabular numerals** for consistent number alignment

### Main Content Typography (lines 960–1286)
- **Responsive line-height**: 1.45 (≤649px) → 1.50 (≤999px) → 1.55 (≤1199px) → 1.60 (≥1200px)
- **Oldstyle numerals** via `font-variant-numeric`
- **Text justification** on desktop only, left-aligned on mobile to avoid rivers
- **Auto-hyphenation** (`hyphens: auto`)
- **Paragraph indentation**: 1.75em default, 2.5em in main content on desktop
- **First-paragraph handling**: `.first-graf` has no indent
- **Poems**: monospaced serif font, left-aligned, special stanza handling

### Headings (lines 1040–1166)
- **H1**: right-aligned, small-caps, 600 weight, bottom border, 1.75em/2em responsive
- **H2**: uppercase, 1.4em, dotted bottom border
- **H3**: 1.35em bold
- **H4–H6**: decreasing sizes (1.2em, 1.1em, 1.1em italic)
- **Link styling in headings**: no underlines, no visited color differentiation
- **Code in headings**: inherit size, bold, no background

### Blockquotes (lines 1287–1356)
- **Three-level color cycling** for nested blockquotes (level 1-3, 4-6)
- **95% font-size scaling** with compensated line-height
- **Responsive padding**: 0.7em/1em on mobile, 0.9em/1.25em on desktop
- **Border and background** from CSS variables for theme support

### Lists (lines 1357–1494)
- **Custom bullet styling** using SVG data URIs: black star (level 1), white star (level 2), rotated white star (level 3)
- **Numbered list markers** with CSS counters, multiple types (decimal, alpha, roman, greek)
- **Special "recently modified" marker**: white star on black circle
- **Left padding**: 2.5em desktop, 1.5em mobile
- **Justification** only for `.big-list` on desktop

### Floats & Figures (lines 1495–1681)
- **Floating figures**: max 50% width minus margin, left/right variants
- **Figure centering** with `fit-content` wrapper
- **Image constraints**: `max-height: calc(100vh - 8rem)`, auto width
- **1px outline** on images/videos (suppressible with `.outline-not`)
- **Figcaptions**: 0.9em, left-aligned, bold first paragraph
- **`.width-full` figures**: full-width override

### Margin Notes (lines 1682–1786)
- **Dual display modes**: inline (italic colored text) or sidenote (popped-out on wide viewports ≥1497px)
- **Sidenote positioning**: absolute, positioned via `--marginnote-vertical-position` custom property
- **Width calculation**: `calc(50vw - (var(--GW-body-max-width) / 2 + 96px))`
- **Special handling** in admonitions (adjusted for icon area width)
- **"Icon only" variants** for pure visual indicators

### Links (lines 1819–2065)
- **Tufte CSS-style underlining**: gradient background-image positioned at baseline, with text-shadow for descender clearance
- **Color states**: link, visited (grayed), hover (highlight)
- **Visited link darkening**: subtle graying to show browsing history
- **Skip-ink simulation**: 7-point text-shadow corona creates gaps around descenders
- **Inline icons**: SVG backgrounds via CSS variables (`.inline-icon`)
- **Recently-modified icons**: star-in-circle indicator
- **Oldstyle numerals disabled** in links to prevent subscript-like overlap

### Sub/Superscripts (lines 2066–2100)
- **0.7em sizing**, relative positioning
- **`.subsup` class**: dual sub+superscript via flexbox column (for inflation adjusters)

### Misc Typography (lines 2101–2187)
- **Date ranges**: nowrap, oldstyle nums, special subsup positioning
- **Small-caps**: `.smallcaps` class, auto-smallcaps on `.intro-graf::first-line`
- **Editorial insertions**: monospaced serif font

### Sidenotes (lines 2209–2235)
- **Sidenote columns** hidden on ≤1760px viewports
- **Margin notes** hidden until JS determines layout form

### Accessibility (lines 2236–2267)
- **Skip-to-content link**: off-screen until Tab-focused, red block in upper-left
- **WCAG-compliant** keyboard navigation aid

### Page-Specific: /index (lines 2280–2538)
- **Header/TOC hidden** for compact presentation
- **Grid layout**: 2 columns (≥961px), 3 columns (≥1761px)
- **Special link list styling**: white star bullets (lighter than default), custom heading borders
- **"Newest: Blog" section**: condensed line-height, hyphenation disabled, responsive item hiding (10/20/30 items shown based on viewport)
- **Horizontal rulers hidden** on desktop (visual separation via columns)
- **Nav link color inversion** (hover color becomes default, default becomes hover)

### Page-Specific: /404 (lines 2539–2555)
- **Increased top margin** on desktop
- **Float margin adjustment** for figures

### Page-Specific: /doc/index (lines 2556–2579)
- **Tag short descriptions**: 0.875em, left-padded, sans-serif
- **See-also list styling**: suppress bullets, inline code styling

### Source Code Previews (lines 2599–2616)
- **Full-viewport code files**: no max-width, no padding on body
- **Unbounded pre blocks**: no max-height, no border

---

## CSS Custom Properties

### Fonts
- `--GW-serif-font-stack`: Source Serif 4, Apple Garamond, Baskerville, ... (with emoji/Quivira fallbacks)
- `--GW-sans-serif-font-stack`: Source Sans 3, Lucida Sans Unicode, Helvetica, ...
- `--GW-monospaced-font-stack`: IBM Plex Mono, Liberation Mono, Consolas, ...
- `--GW-monospaced-serif-font-stack`: Nimbus Mono, Free Mono, Courier New, ...

### Layout
- `--GW-body-text-font-size`: 20px (desktop), 18px (mobile ≤649px)
- `--GW-body-max-width`: 935px
- `--GW-body-side-padding`: 20px (desktop), 16px (mobile)
- `--GW-sidenotes-max-width`: maximum width for sidenotes

### Typography
- `--line-height`: responsive 1.45–1.60 based on viewport
- `--text-indent`: 1.75em (default), 2.5em (main content desktop)
- `--text-alignment`: justify (desktop), left (mobile)
- `--text-hyphenation`: auto (default), none (specific contexts)
- `--base-block-spacing`: 0.25em (used with `--bsm` multiplier)

### Colors (defined elsewhere, used here)
- `--GW-body-background-color`, `--GW-body-text-color`
- `--GW-body-link-color`, `--GW-body-link-hover-color`, `--GW-body-link-visited-color`
- `--GW-body-link-inverted-*` variants for color-inverted containers
- `--GW-nav-header-link-color`, `--GW-nav-header-link-hover-color`
- `--GW-H1-border-color`, `--GW-H2-border-color`
- `--GW-TOC-background-color`, `--GW-TOC-border-color`, `--GW-TOC-link-hover-*`
- `--GW-TOC-collapse-button-*` (multiple)
- `--GW-blockquote-background-color-level-{one,two,three}`
- `--GW-blockquote-border-color-level-{one,two,three,four}`
- `--GW-figure-outline-color`, `--GW-figure-caption-outline-color`
- `--GW-text-selection-background-color`, `--GW-text-selection-color`
- `--GW-skip-to-content-*` (text, background, border)

### Images (SVG data URIs)
- `--GW-image-single-black-star-svg`
- `--GW-image-single-white-star-svg`
- `--GW-image-single-white-star-rotated-svg`
- `--GW-image-single-white-star-on-black-circle-svg`

### Link Underlining
- `--link-underline-background-color`: typically `var(--background-color)`
- `--link-underline-gradient-line-color`: typically `currentColor`

### Floating Elements
- `--float-side-margin`: 2em (content), 1.5em (blockquotes), adjustable per-page

### Margin Notes
- `--marginnote-vertical-position`: dynamic, set by JS based on scroll position

### Blockquotes
- `--blockquote-vertical-padding`, `--blockquote-horizontal-padding`, `--blockquote-font-size-scaling-factor`

### Lists
- `--list-left-padding`: 2.5em (desktop), 1.5em (mobile)
- `--list-bullet`: SVG data URI for bullet style
- `--list-bullet-opacity`: 0.65–0.85 depending on level
- `--list-bullet-dark-mode-invert-filter`: invert(1) for dark mode compatibility

### Icons
- `--icon-opacity`, `--icon-width`, `--icon-height`, `--icon-offset-x`, `--icon-offset-y`, `--icon-url`
- Per-icon custom properties for specific icons (moon, gear, book, manicule, etc.)

---

## Responsive Breakpoints

The stylesheet uses a sophisticated cascade of breakpoints for different layout needs:

### Primary breakpoints (device width)
- **≤649px (mobile)**: Single-column, reduced fonts (18px), left-aligned text, no justification, simplified TOC, smaller padding, hidden desktop-only elements
- **650–900px (tablet)**: Two-column TOC, serif paragraph styling, some floating figures
- **901–1179px (desktop)**: Floating TOC sidebar, justified text, floating figures, larger fonts
- **1180–1496px (wide desktop)**: Absolute-positioned logo, heading left-margin adjustment
- **1497–1760px (ultra-wide)**: Sidenote-mode margin notes enabled
- **≥1761px (ultra-ultra-wide)**: Sidenote columns visible, 3-column `/index` layout

### Special breakpoints
- **≤779px**: Hide some nav links in sidebar (`.mobile-not` in sidebar context)
- **≤960px**: `/index` newest-blog section shows only 10 items
- **≥961px**: `/index` 2-column grid layout, newest-blog shows 20 items (2-column) or 30 items (3-column ≥1761px)
- **≤999px, ≤1199px, ≥1200px**: Progressive line-height increases (1.45 → 1.50 → 1.55 → 1.60)

### Media query types
- **`@media all and (max-width: Xpx)`**: Mobile-first, narrow viewports
- **`@media all and (min-width: Xpx)`**: Desktop, wide viewports
- **`@media all and (hover: hover)`**: Desktop with mouse (enables hover effects)
- **`@media all and (hover: none)`**: Touch devices (larger tap targets, no hover states)

---

## Key Selectors

### Structural
- `html`, `body`, `main`: Root layout containers
- `#sidebar`: Top navigation bar with logo and links
- `header`: Page title block
- `#page-metadata`: Metadata fields (author, date, tags, confidence, etc.)
- `#TOC`: Table of contents
- `article`, `#markdownBody`, `.markdownBody`: Main content containers
- `.shadow-body`: Popup shadow DOM content (inherits base styles)

### Content Elements
- `.heading`: Headings h1–h6 (via `section.level1–6 > .heading`)
- `p`, `.first-graf`, `.intro-graf`: Paragraph variants
- `blockquote`, `.blockquote-level-{1-6}`: Nested blockquote styling
- `ul`, `ol`, `.list`, `.list-level-{1-3}`: Lists with custom markers
- `.list-type-{decimal,lower-alpha,upper-alpha,lower-roman,upper-roman,lower-greek}`: List numbering types
- `figure`, `figcaption`, `.figure-outer-wrapper`, `.caption-wrapper`: Figure layout
- `.float`, `.float-left`, `.float-right`: Floating content
- `.width-full`: Full-width figures

### Typography
- `.poem`, `.poem-html`, `.stanza`, `.enjambed-line`: Poetry formatting
- `.text-center`, `.text-right`: Alignment overrides
- `.smallcaps`, `p.smallcaps`, `span.smallcaps`: Small-caps variants
- `.intro-graf::first-line`: Auto-smallcaps first line
- `em`: Italics (with 0.1em right margin to prevent text-shadow glitches)
- `sub`, `sup`, `.subsup`: Subscripts/superscripts
- `span.date-range`: Nowrap date ranges with special subsup handling
- `.editorial`: Bracketed editorial insertions

### Links
- `a`, `a:link`, `a:visited`, `a:hover`: Link states with Tufte CSS underlining
- `.decorate-not`: Suppress link underline/text-shadow
- `.inline-icon`, `.icon-{moon,gear,book,sun,magnifying-glass,question,...}`: SVG icon links
- `.has-recently-modified-icon .recently-modified-icon-hook`: Star icon for updated content
- `.link-modified-recently-list-item`: Special list bullet for updated items

### Margin Notes & Sidenotes
- `.marginnote`, `.marginnote.inline`, `.marginnote.sidenote`: Dual-mode margin notes
- `.marginnote.only-icon`: Icon-only margin notes
- `.margin-notes-block`: Collected margin notes at section start
- `#sidenote-column-left`, `#sidenote-column-right`: Sidenote containers
- `p.has-margin-note`: Paragraphs with positioned sidenotes

### TOC
- `.TOC`, `#TOC`: Table of contents containers
- `#TOC.collapsed`: Collapsed state
- `.toc-collapse-toggle-button`: Expand/collapse button
- `.TOC-links-only`: Directory index TOCs with special heading

### Special Classes
- `.mobile-not`, `.desktop-not`: Responsive visibility toggles
- `.display-not`: Universal hide utility
- `[class*='display-random-']`, `.visible`: Random display system
- `.clear-floats`: Clear both floats
- `.colors-invert`: Color-inverted containers (different link colors)
- `.block`, `--bsm`: Block spacing system
- `.big-list`: Lists with justification on desktop

### Page-Specific
- `body.page-index`: Home page overrides
- `body.page-404`: 404 page styling
- `body.page-doc-index`: Documentation index
- `body.page-fiction-october.reader-mode-active`: Fiction reader mode
- `body[class*='-index']`: Directory index pages
- `body.file-preview-source-code`: Code file previews

### Accessibility
- `#skip-to-content-link`: Skip navigation link
- `::selection`: Text selection styling

### Other
- `#console.hidden`: Hidden console element
- `div#new-popular-notable`: Special `/index` grid container
- `section#newest-blog`, `section#notable`: `/index` sections

---

## Integration Points

### JavaScript Dependencies
**Consumed by JS modules:**
- **[collapse-js](collapse-js)**: Relies on TOC `.collapsed` class, `.toc-collapse-toggle-button` markup
- **[sidenotes-js](sidenotes-js)**: Reads/writes `.marginnote.{inline,sidenote}` classes, sets `--marginnote-vertical-position` custom property
- **[content-js](content-js)**: `.shadow-body` class for popup styling inheritance
- **[popups-js](popups-js)**: Uses link underlining styles, reads `.markdownBody` context
- **Dark mode JS**: Toggles theme by swapping color custom properties; initial.css references all `--GW-*-color` variables

**Sets state for JS:**
- `.TOC .visible` class on random display elements
- `body.page-*` classes set by server/build, read by JS for context
- `.has-recently-modified-icon`, `.recently-modified-icon-hook` for update indicators

### CSS File Dependencies
**Loaded after initial.css (deferred):**
- **[colors-css](colors-css)**: Defines all `--GW-*-color` variables, `--GW-image-*-svg` data URIs
- **[default-css](default-css)**: Non-critical styles (admonitions, code blocks, tables, footnotes, forms, etc.)
- **[dark-mode-adjustments-css](dark-mode-adjustments-css)**: Dark mode color overrides, inverts images/icons
- **Mobile-specific CSS**: Additional mobile optimizations
- **Print CSS**: Print media query styles

**Expects from HTML:**
- Specific class structure: `section.level{1-6}`, `.markdownBody`, `#markdownBody`
- Metadata in `#page-metadata .page-metadata-fields` with specific child `<span>` classes
- TOC structure: `#TOC > ul > li > a` hierarchy
- Figure wrapper: `.figure-outer-wrapper > img/video/svg + .caption-wrapper > figcaption`

### Build Process
- **Generated during build** by [sync-sh](../backend/sync-sh) / Hakyll
- **Inlined into HTML** in critical rendering path (not loaded as external file on initial load)
- **Typography optimization**: Assumes web font loading ([initial-js](initial-js) handles FOUT/FOIT)
- **SVG data URIs**: Embedded from separate files during build

### Performance Considerations
- **Critical CSS**: Entire file must load before first paint
- **~2,600 lines**: Balances comprehensiveness vs. load time (gzips well due to repetition)
- **CSS custom properties**: Slight runtime performance cost, but enable dynamic theming
- **Complex selectors**: Some long descendant selectors may have performance impact on large DOMs

---

## See Also

- [colors-css](colors-css) - Color variable definitions and SVG data URIs
- [default-css](default-css) - Deferred non-critical styles
- [dark-mode-adjustments-css](dark-mode-adjustments-css) - Dark mode overrides
- [reader-mode-css](reader-mode-css) - Reader mode styling
- [sidenotes-js](sidenotes-js) - Sidenote positioning logic
- [collapse-js](collapse-js) - TOC collapse/expand behavior
- [initial-js](initial-js) - Critical-path JavaScript (web font loading, etc.)
- [dark-mode-js](dark-mode-js) - Theme switching that affects these styles
