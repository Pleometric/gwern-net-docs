
# default.css

**Path:** `css/default.css` | **Language:** CSS | **Lines:** ~7,569

> Deferred styles for below-the-fold features

---

## Overview

`default.css` is the main stylesheet for gwern.net, containing styles for below-the-fold features and interactive components. Unlike [initial-css](initial-css) which provides critical above-the-fold styles inlined in the HTML, this file is bundled into `style-GENERATED.css` during the build process. The combined stylesheet is then loaded asynchronously using the `media="print" onload="this.media='all'"` pattern for improved page load performance.

The file implements sophisticated styling for the site's signature features: margin sidenotes, popup windows (popups), inline content expansions (popins), collapsible sections, footnotes, code blocks, tables, image focus overlays, and the page toolbar. It extensively uses CSS custom properties (CSS variables) prefixed with `--GW-*` for theming and layout configuration.

This stylesheet works in close coordination with JavaScript modules like [popups-js](popups-js), [sidenotes-js](sidenotes-js), [collapse-js](collapse-js), and [extracts-js](extracts-js) to create the site's interactive reading experience.

---

## Key Sections

The CSS is organized into major functional sections, roughly in order of appearance:

### Variables (Lines 1-67)
Defines CSS custom properties for all major components:
- Sidenote dimensions (`--GW-sidenotes-max-width: 550px`)
- Popup sizing constraints (min/max widths and heights for various popup types)
- Popin layout parameters (max height, border widths, title bar height)
- Page toolbar timing and animation variables
- Floating header indicators
- Search widget dimensions

### Generic Elements (Lines 70-93)
Resets and base styles for `<button>` elements used throughout the UI.

### Sections (Lines 94-293)
Heading self-links (pilcrow symbols ¶), copy-section-link buttons, and highlighted section borders for extract popups.

### Collapse Blocks (Lines 295-1088)
Extensive styles for collapsible sections (`.collapse-block`):
- Disclosure buttons (top and bottom)
- Abstract previews (`.abstract-collapse`)
- Inline collapses (`.collapse-inline`)
- Special handling for blockquotes, aux-links, and section separators
- Z-index layering for interactive states

### Lists, Triptychs, Figures (Lines 1157-1378)
Specialized list styling, three-column triptych layouts, figure captions, and embedded iframes.

### Interviews, Abstracts, Epigraphs (Lines 1379-1583)
Content-specific formatting for interview transcripts, paper abstracts, and decorative quotations.

### Admonitions (Lines 1584-1688)
Styled alert boxes (`.admonition.note`, `.tip`, `.warning`, `.error`) with color-coded left borders.

### Tables (Lines 1689-1967)
Complex table styling including:
- Zebra striping (`.table-striped`)
- Horizontal scrolling containers
- Column sorting indicators
- Custom scrollbar styling
- Hover effects for rows and columns

### Code Blocks (Lines 1968-2138)
Syntax-highlighted code with:
- Scrollable `<pre>` blocks
- Custom scrollbar theming
- Language-specific highlighting (spans with classes like `.kw`, `.st`, `.co`)

### Math, Citations, Numbers (Lines 2140-2326)
LaTeX math rendering adjustments and citation formatting.

### Dropcaps (Lines 2435-2604)
Large decorative initial letters for the first paragraph of articles.

### Footnotes (Lines 2605-2878)
Complete footnote system:
- Superscript citation links (`.footnote-ref`) with expanded hover areas
- Footnotes section styling with decorative horizontal rule
- Numbered footnote list with dotted borders
- Back-to-citation links (`.footnote-back`)
- Highlighting for targeted footnotes

### Sidenotes (Lines 2880-3170)
Margin note positioning system:
- Left/right sidenote columns (`#sidenote-column-left`, `#sidenote-column-right`)
- Sidenote blocks (`.sidenote`) with hover/highlight states
- Scroll wrappers for long sidenotes with custom scrollbars
- Sidenote self-links (numbers)
- Cut-off indicators ("…") for truncated notes
- Displacement and z-index management

### Footer (Lines 3171-3188)
Page footer spacing.

### Transcluded TOCs (Lines 3189-3210)
Styles for tables of contents embedded in popups.

### Content Transforms (Lines 3211-3417)
Various content-specific transforms and adjustments.

### Annotations (Lines 3418-3531)
Annotation metadata styling for the popup system.

### X of the Day (Lines 3532-3579)
Special section for daily featured content.

### Bottom Decoration, Sequential Nav (Lines 3680-3803)
Page footer decorations and previous/next navigation UI.

### Pop-Frames (Lines 3944-4337)
Base styles for popup/popin content containers:
- Title bar elements
- Loading states
- Type-specific content adjustments (annotations, footnotes, code files, objects)
- Special handling for Wikipedia entries, tweets, local pages, tags

### Popups (Lines 4338-5136)
Floating popup window system:
- Popup positioning and sizing
- Shadow/border effects
- Title bars (full and mini versions)
- Resize handles
- Type-specific popup styles (video, audio, image, annotation, etc.)
- Popup stacking and z-index management

### Popins (Lines 5137-5692)
Inline content expansion system:
- Popin outer frame (`.popin`)
- Title and footer bars
- Scroll view with custom scrollbars
- Backdrop overlays for open popins
- Partial annotation appends
- Stack counters

### UI Elements Container (Lines 5693-5715)
Container for fixed UI elements.

### Page Toolbar (Lines 5716-6117)
Vertical toolbar on the right side:
- Toggle buttons (gear icon)
- Collapse/expand animations
- Widget buttons with icons and labels
- Hover states and active states
- Flash effects for widget notifications
- Mobile and desktop layouts

### Inline Widgets, Virtual Widgets (Lines 6118-6144)
Small in-content widgets and their virtual counterparts.

### Image Focus (Lines 6145-6755)
Full-screen image viewer overlay:
- Hover states for focusable images
- Dark overlay background
- Centered image display with loading spinner
- Caption bar with image URLs
- Slideshow controls (prev/next buttons)
- Help box
- Keyboard shortcut indicators
- Image number display for galleries

### Floating Header (Lines 6756-6893)
Scroll-position header with progress indicator.

### Reader Mode (Lines 6894-6944)
Simplified reading view adjustments.

### Mode Selectors (Lines 6945-7016)
Color scheme and theme selectors.

### Links Widgets, Search Widget, Help Widget (Lines 7187-7232)
Toolbar widgets for site navigation, search interface, and help documentation.

### General Activity Indicator (Lines 7233-7266)
Loading spinner used across multiple components.

### Transclusion (Lines 7267-7375)
Styles for transcluded content sections.

### Print Styles (Lines 7376-7485)
Optimizations for printed pages (show URLs, hide interactive elements).

### Console (Lines 7486-7569)
Developer console widget styles.

---

## CSS Custom Properties

### Sidenotes
- `--GW-sidenotes-max-width`: Maximum width of sidenote columns (550px)

### Popups
- `--GW-popups-popup-max-width`: Default popup max width (640px)
- `--GW-popups-popup-max-height`: Default popup max height (480px)
- `--GW-popups-popup-border-width`: Popup border thickness (3px)
- `--GW-popups-popup-title-bar-height`: Full title bar height (calc(1.5rem + 1px))
- `--GW-popups-popup-mini-title-bar-height`: Mini title bar height (calc(1rem + 1px))
- Type-specific dimensions:
  - `--GW-popups-annotation-popup-min-width/height`
  - `--GW-popups-wikipedia-entry-popup-min-width/height`
  - `--GW-popups-tweet-popup-min-height`, `--GW-popups-tweet-avatar-size`
  - `--GW-popups-video-popup-min-width/height`
  - `--GW-popups-video-youtube-iframe-width/height`
  - `--GW-popups-audio-popup-min-width/height`
  - `--GW-popups-image-popup-min-size`

### Popins
- `--GW-popins-popin-max-height`: Maximum popin height (75vh)
- `--GW-popins-popin-min-height`: Minimum popin height (120px)
- `--GW-popins-popin-border-width`: Popin border thickness (3px)
- `--GW-popins-popin-title-bar-height`: Popin title bar height
- `--GW-popins-popin-footer-bar-height`: Popin footer bar height

### Page Toolbar
- `--GW-page-toolbar-minimum-width`: Minimum toolbar width (46px)
- `--GW-page-toolbar-collapse-duration`: Collapse animation duration (0.25s)
- `--GW-page-toolbar-slow-collapse-duration`: Slow collapse duration (1s)
- `--GW-page-toolbar-fade-after-collapse-duration`: Fade timing (0.25s)
- `--GW-page-toolbar-widget-flash-rise-duration`: Widget flash in (1s)
- `--GW-page-toolbar-widget-flash-fall-duration`: Widget flash out (1s)
- `--GW-page-toolbar-widget-flash-filter`: Flash visual effect

### Other
- `--GW-iframe-background-color`: Background for embedded iframes (#fff)
- `--GW-floating-header-scroll-indicator-thickness`: Progress bar thickness (3px)
- `--GW-search-iframe-height`: Search widget iframe height (128px)

### Color Variables
Many color-related custom properties reference values from [colors-css](colors-css):
- `--GW-collapse-disclosure-button-color`
- `--GW-section-highlighted-border-color`
- `--GW-sidenote-highlight-box-shadow-color`
- `--GW-footnote-highlighted-border-color`
- `--GW-popins-popin-background-color`
- `--GW-page-toolbar-border-color`
- And many more for theming interactive states

---

## Key Selectors

### Sections and Headings
- `.heading`: Section headings with self-link pilcrows
- `.heading a::after`: Pilcrow symbol (¶) on hover
- `.heading .copy-section-link-button`: Copy link button
- `section.highlighted`: Sections highlighted by extract popups

### Collapse Blocks
- `.collapse-block`: Container for collapsible sections
- `.collapse-block.expanded` / `.expanded-not`: Collapse state classes
- `.collapse-block > .disclosure-button`: Top/bottom expand/collapse buttons
- `.collapse-block > .abstract-collapse`: Preview content shown when collapsed
- `.collapse-inline`: Inline collapsible elements

### Footnotes
- `a.footnote-ref`: Superscript citation links
- `section.footnotes`: Footnotes section container
- `section.footnotes > hr:first-child`: Decorative rule with circle
- `section.footnotes > ol > li`: Individual footnote items
- `.footnote-back`: Back-to-citation arrows

### Sidenotes
- `#sidenote-column-left`, `#sidenote-column-right`: Margin columns
- `.sidenote`: Individual sidenote blocks
- `.sidenote.displaced`: Sidenote moved to avoid overlap
- `.sidenote.cut-off`: Truncated sidenote with scrollbar
- `.sidenote-self-link`: Number link in sidenote

### Pop-Frames (base class for popups and popins)
- `.popframe`: Base container
- `.popframe-title-bar`: Title bar container
- `.popframe-title-link`: Link in title
- `.popframe-scroll-view`: Scrollable content area
- `.popframe-content-view`: Inner content container
- `.popframe-body`: Actual content

### Popups
- `.popup`: Floating popup window
- `.popup.spawned`: Active popup
- `.popup .popup-title-bar`: Popup title bar
- `.popup.has-footer`: Popup with footer bar
- `.popup.resized`: User-resized popup
- Type classes: `.annotation`, `.wikipedia-entry`, `.tweet`, `.video`, `.audio`, `.image`, `.local-page`, etc.

### Popins
- `.popin`: Inline expansion container
- `.popin-open`: Active popin
- `.popin-ancestor`: Parent element of active popin
- `.popin .popin-footer-bar`: Popin footer
- `.popin-stack-counter`: Popin nesting depth indicator

### Page Toolbar
- `#page-toolbar`: Main toolbar container
- `#page-toolbar.vertical`: Vertical layout mode
- `#page-toolbar .widgets`: Widget container
- `#page-toolbar .widget`: Individual widget
- `.widget-button`: Widget button element
- `#page-toolbar.collapsed`: Collapsed state

### Image Focus
- `.markdownBody img.focusable`: Images with click-to-zoom
- `#image-focus-overlay`: Full-screen image viewer
- `#image-focus-overlay.engaged`: Active overlay
- `#image-focus-overlay.slideshow`: Slideshow mode
- `.image-in-focus`: Centered image
- `.caption`: Image caption bar
- `.slideshow-buttons`: Previous/next controls

### Tables
- `.table-scroll-wrapper`: Horizontal scroll container
- `table.stripe-not`: No zebra striping
- `table.table-zebra-soft`: Soft striping
- `table.table-zebra-bichrome`: Two-color striping
- `thead.sort-enabled th`: Sortable column headers

### Code
- `div.sourceCode`: Syntax-highlighted code container
- `pre > code`: Code block content
- `code span.kw`: Keyword highlighting
- `code span.st`: String highlighting
- `code span.co`: Comment highlighting

### Admonitions
- `.admonition.note`: Note box
- `.admonition.tip`: Tip box
- `.admonition.warning`: Warning box
- `.admonition.error`: Error box

---

## Integration Points

### JavaScript Modules

**[sidenotes-js](sidenotes-js)**
- Manages `.sidenote` positioning, displacement, and cut-off states
- Adds/removes classes: `.displaced`, `.cut-off`, `.hidden`, `.highlighted`, `.targeted`

**[popups-js](popups-js)**
- Creates and positions `.popup` elements
- Manages popup lifecycle: `.spawned`, `.fading`, `.despawning`
- Handles resize interactions: `.resized`, `.resizing`

**[extracts-js](extracts-js)**
- Coordinates between popups (`.popup`) and popins (`.popin`)
- Manages `.popin-open` state and `.popin-ancestor` markers
- Adds `.highlighted` class to sections

**[collapse-js](collapse-js)**
- Toggles `.expanded` / `.expanded-not` classes on `.collapse-block`
- Handles disclosure button interactions

**[image-focus-js](image-focus-js)** (likely)
- Manages `#image-focus-overlay.engaged` state
- Controls `.slideshow` mode
- Handles `.image-in-focus.loading` states

**Page toolbar JS** (likely)
- Manages `#page-toolbar.collapsed` / `.expanded-temp` states
- Handles widget button selections: `.selected`, `.flashing`

**[dark-mode-js](dark-mode-js)** (likely)
- Switches between light/dark CSS custom property values
- References color variables from [colors-css](colors-css)

### Shared State via Classes

Many modules communicate state via shared CSS classes:
- `.highlighted`: Applied to sections/elements to indicate focus
- `.targeted`: Applied to elements that are URL hash targets
- `.loading`, `.loading-failed`: Content loading states
- `.expanded`, `.expanded-not`: Collapse states
- `.spawned`, `.despawning`: Lifecycle states for popups

### Custom Properties for Theming

JavaScript can dynamically modify `--GW-*` variables for:
- Runtime theme switching
- User preference persistence
- Responsive layout adjustments

### Media Queries

Extensive responsive design with breakpoints:
- `max-width: 649px`: Mobile layout
- `min-width: 650px`: Desktop layout
- Orientation-specific rules for image focus overlay
- Hover capability detection: `@media (hover: hover)`

---

## See Also

- [initial-css](initial-css) - Critical above-the-fold CSS
- [colors-css](colors-css) - Color scheme variables
- [dark-mode-adjustments-css](dark-mode-adjustments-css) - Dark mode overrides
- [reader-mode-css](reader-mode-css) - Reader mode styles
- [popups-js](popups-js) - Popup window JavaScript
- [sidenotes-js](sidenotes-js) - Sidenote positioning logic
- [collapse-js](collapse-js) - Collapsible section behavior
- [dark-mode-js](dark-mode-js) - Theme switching
