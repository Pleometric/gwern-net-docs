
# reader-mode-initial.css

**Path:** `css/reader-mode-initial.css` | **Language:** CSS | **Lines:** ~157

> Reader mode styles for distraction-free reading

---

## Overview

The reader-mode-initial.css file provides the visual styling layer for gwern.net's distraction-free reader mode. When activated, reader mode progressively hides navigational chrome, sidebars, metadata displays, and visual link indicators to create a minimalist reading experience focused solely on the article text. The stylesheet implements two distinct modes: a base reader mode that hides UI elements, and an enhanced "masked links" mode that visually conceals hyperlinks to reduce distractions.

This CSS file works in concert with reader-mode-initial.js (which handles early activation based on localStorage settings) and reader-mode.js (which provides the full interactive controls, including Alt-key toggling of masked links and automatic deactivation on scroll). The styles are scoped to activate only when `body.reader-mode-active` class is present, allowing for instant visual changes without page reflow.

The stylesheet implements careful distinctions between elements that should be completely hidden (`display: none`) versus those that should preserve layout but be invisible (`visibility: hidden` or `opacity: 0`), ensuring smooth transitions and preventing layout shifts when reader mode is toggled.

---

## Key Changes

### Complete Element Removal (`display: none`)

**Navigation and chrome:**
- `#sidebar` - Left sidebar with logo and site navigation
- `#navigation` - Main navigation bar
- `#footer` - Page footer with links and metadata
- `.reader-mode-not` - Generic class for elements explicitly excluded from reader mode

**Page metadata decorations:**
- `.page-status`, `.page-confidence`, `.page-importance` - Quality indicators
- `.page-backlinks`, `.page-link-bibliography`, `.page-similars` - Cross-reference widgets
- `.page-date-range::after`, `.page-metadata-fields::after` - Decorative pseudo-elements

**Marginal content:**
- `#sidenote-column-left`, `#sidenote-column-right` - Margin note columns (sidenotes are hidden)
- `#TOC ul li::before` - Table of contents bullet decorations
- `#TOC .toc-collapse-toggle-button` - TOC expand/collapse controls

**Link decorations:**
- `.link-icon-hook` - Icons indicating link types (PDF, external, etc.)
- `.recently-modified-icon-hook` - Icons for recently updated pages
- `.indicator-hook` (when masked-links-hidden) - Popup/annotation indicators

**Misc. visual elements:**
- `.inflation-adjusted .subsup` - Inflation adjustment superscripts
- `.date-range sub` - Date range subscripts
- `.file-includes` - File inclusion metadata
- `.annotation.blog-post .data-field.aux-links` - Auxiliary links in blog post annotations

### Visibility-Only Hiding (layout preserved)

**Selective hiding with `visibility: hidden`:**
- `.date-range .subsup sub` - Date subscripts (preserves spacing)

**Opacity-based hiding (allows smooth transitions):**
- `.footnote-ref` (when masked-links-hidden) - Footnote reference superscripts
- `.footnote-back`, `.footnote-back-block` - Footnote return links

### Link Masking System

When `body.reader-mode-active.masked-links-hidden` is active:

**Visual concealment:**
- Links inherit text color (`color: inherit`)
- Background removed (`background: none`)
- Cursor changed to text cursor (`cursor: text`)
- Text shadows removed (`text-shadow: none`)
- Indicator hooks hidden (`visibility: hidden`)

**Exceptions (links that remain visible):**
- `.popup-open`, `.popin-open` - Currently active popup/popin links remain styled
- Links are still interactive when hovered (Alt key reveals them)

### Citation Formatting Changes

Reader mode adjusts citation display for readability:
- `.cite-joiner` - Changed from `display: none` to `display: initial` (shows separators)
- `.cite-date` - Removes vertical-align, font-size, and margin adjustments for inline display
- `.cite-date::before` - Adds space before date
- `.cite-author-plural::after` - Adds space after multiple authors

### Simplified Inline Code

- Paragraph code blocks (`p code`) lose background color and border
- Maintains readability while reducing visual noise

---

## Key Selectors

### Body State Classes

**`body.reader-mode-active`**
- Primary activation class added by `ReaderMode.activate()` in reader-mode-initial.js
- Enables all base reader mode styles

**`body.reader-mode-active.masked-links-hidden`**
- Secondary class for enhanced link masking
- Added simultaneously with `reader-mode-active` during activation
- Can be toggled via Alt key (handled in reader-mode.js)

### Content Containers

**`.markdownBody`**
- Main content container where all article text lives
- Most link and text styling changes are scoped within this container
- Ensures reader mode only affects primary content, not UI elements

### Link States

**`a.has-indicator-hook`**
- Links with popup/annotation indicators
- Receives special positioning (`position: relative`, `z-index: 111`) in reader mode

**`a:not(.popup-open):not(.popin-open)`**
- Targets links that are NOT currently showing a popup
- These are the links that get masked when masked-links-hidden is active
- Active popups remain styled so users don't lose context

### Special UI Elements

**`.reader-mode-disable-when-clicked`**
- Elements that should exit reader mode when clicked
- Gets `cursor: pointer` in reader mode to signal interactivity
- Integrated with click handlers in reader-mode.js

**`.reader-mode-style-not`**
- Anti-style zone: all descendants inherit font-weight, font-style, font-variant, and color
- Prevents unwanted styling overrides in specific sections

### Metadata and Navigation

**`#page-metadata` descendants:**
- `.page-status`, `.page-confidence`, `.page-importance` - Quality indicators
- `.page-backlinks`, `.page-link-bibliography`, `.page-similars` - Link collections
- Selectively hidden to clean up header area while preserving core metadata

**`#TOC` (Table of Contents):**
- `ul li` - Padding reduced from default to `0.125em`
- `ul li::before` - Decorative bullets removed
- `.toc-collapse-toggle-button` - Expand/collapse controls hidden

---

## Integration Points

### JavaScript Activation

**reader-mode-initial.js** (early/critical path):
```javascript
// Adds both classes simultaneously on activation
document.body.classList.add("reader-mode-active", "masked-links-hidden");
```
- Runs before DOMContentLoaded to minimize flash of unstyled content
- Checks `localStorage.getItem("reader-mode-setting")` and body classes to determine activation
- Fires `ReaderMode.didActivate` event for downstream handlers

**reader-mode.js** (full interactive features):
- `ReaderMode.setup()` - Initializes UI controls and event handlers
- `ReaderMode.activate()` / `ReaderMode.deactivate()` - Toggles body classes
- Alt key handler - Temporarily removes `masked-links-hidden` class to reveal links
- Scroll observer - Auto-deactivates reader mode when scrolling to footer/appendices
- Mode selector widget - Allows choosing between "Auto", "On", "Off" modes

### CSS Variables

The stylesheet uses CSS custom properties for theming:
- `--link-underline-background-color` - Used for `.indicator-hook` backgrounds in reader mode
- Inherits color scheme from main theme system (light-mode.css / dark-mode.css)

### Load Order

1. **initial.css** - Base styles loaded in `<head>`
2. **reader-mode-initial.css** - Loaded in `<head>` for critical reader mode styles
3. **reader-mode-initial.js** - Runs early to add classes before render
4. **reader-mode.js** - Loaded later with main script bundle for full interactivity

### Event Integration

**Listens for:**
- `ReaderMode.didLoad` event (from reader-mode.js) - Triggers mode initialization
- Body class mutations - Styles automatically apply when classes change

**Fires:**
- No CSS-triggered events (styles are purely reactive)

### Responsive Behavior

**Mobile adjustments** (`@media all and (max-width: 649px)`):
```css
body.reader-mode-active article {
    margin-top: 0;
}
```
- Removes top margin on article to maximize screen space on small devices
- Reader mode is even more valuable on mobile where screen real estate is limited

---

## See Also

- [reader-mode-initial-js](reader-mode-initial-js) - Early reader mode activation logic
- [reader-mode-js](reader-mode-js) - Full interactive reader mode system
- [initial-css](initial-css) - Base stylesheet loaded before reader mode
- [dark-mode-adjustments-css](dark-mode-adjustments-css) - Dark mode filters
- [colors-css](colors-css) - Color variables used by reader mode
- [sidenotes-js](sidenotes-js) - Sidenote JavaScript that reader mode interacts with
