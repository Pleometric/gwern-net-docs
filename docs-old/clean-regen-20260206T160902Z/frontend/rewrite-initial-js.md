
# rewrite-initial.js

**Path:** `js/rewrite-initial.js` | **Language:** JavaScript | **Lines:** ~537

> Fast non-block-layout DOM processors run before full rewrite.js system

---

## Overview

This module defines "rewrite processors" - a faster variant of the standard layout processors that handle DOM transformations that don't affect block layout. These processors run early in the page load process, before heavier layout-affecting rewrites, to quickly apply styling and inject simple UI elements.

The core distinction is that rewrite processors have `blockLayout: false`, meaning they can safely run without triggering expensive layout recalculations. They handle tasks like adding CSS classes, injecting inline icons, wrapping text nodes, and other lightweight DOM manipulations.

The module implements several categories of fast rewrites: inline icon processing (including SVG-based progress indicators), recently-modified link decorations, list styling, horizontal rule customization, and inline mode selector wrapping. It also includes early page setup logic for thumbnails, TOC visibility, and special page designations.

---

## Public API

### Core Registration Functions

**`addRewriteProcessor(processorName, processor, options = null)`**
- Registers a non-block layout processor
- Automatically sets `blockLayout: false` option
- Delegates to `addLayoutProcessor()` from layout.js
- **Called by:** Module-level code registering processors
- **Calls:** `addLayoutProcessor()`

**`processMainContentAndAddRewriteProcessor(processorName, processor)`**
- Runs processor on `document.main` immediately
- Then registers it for future content loads
- **Called by:** Code needing both immediate and future processing
- **Calls:** `processor()`, `addRewriteProcessor()`

### Progress Indicator API

**`arcSVGForProgressPercent(percent: number) → string`**
- Generates SVG source for circular progress arc (0-100%)
- Uses different stroke widths for mobile vs desktop
- Returns complete `<svg>` element as string
- **Called by:** `renderProgressPercentageIcon()`

**`renderProgressPercentageIcon(progressIndicatorElement: Element)`**
- Injects progress icon into element with `data-progress-percentage` attribute
- Creates inline icon with data URL containing SVG
- Marks element with `.progress-percentage-rendered`
- **Called by:** `injectProgressIcons` processor

**`renderProgressPercentageIconIfNeeded(progressIndicatorElement: Element)`**
- Conditional wrapper that checks if already rendered
- **Called by:** `injectProgressIcons` processor

**`isProgressPercentageIconRendered(progressIndicatorElement: Element) → boolean`**
- Returns true if element has `.progress-percentage-rendered` class

**`invalidateProgressPercentageIconForElement(progressIndicatorElement: Element)`**
- Removes `.progress-percentage-rendered` class
- Forces re-render on next `renderProgressPercentageIconIfNeeded()` call

### Recently-Modified Icon API

**`addRecentlyModifiedIconToLink(link: Element)`**
- Injects `.recently-modified-icon-hook` span at start of link
- Adds U+2060 WORD JOINER to prevent line breaks
- Handles coordination with indicator hooks if present
- Marks with `.has-recently-modified-icon`
- **Called by:** `enableRecentlyModifiedLinkIcons` processor

**`removeRecentlyModifiedIconFromLink(link: Element)`**
- Removes icon hook span and WORD JOINER
- Cleans up coordinating text nodes
- **Called by:** `enableRecentlyModifiedLinkListIcons` processor

### Page Thumbnail API

**`pageThumbnailAttributesFromDocument(doc: Document) → Object|null`**
- Extracts Open Graph image metadata from document
- Returns object with `src`, `title`, `width`, `height`, `class`, `style`
- Returns `null` if no `og:image` meta tag found
- **Called by:** Page initialization code

**`injectThumbnailIntoPageAbstract(pageAbstract, pageThumbnailAttributes, options) → Element|null`**
- Creates `<figure class="page-thumbnail-figure">` with image
- Options: `atEnd` (default true), `floatClass` (default "float-not")
- Skips if already injected or if logo image
- Returns injected `<img>` element or `null`
- **Called by:** Page abstract initialization

### TOC Visibility

**`updateTOCVisibility(TOC: Element)`**
- Hides TOC if no entries, or if main TOC with ≤1 entry
- Checks `body.toc-not` class
- **Called by:** `doWhenElementExists("#TOC")`

---

## Internal Architecture

### Processor Registry

Processors are registered at module load time and run via the layout.js system. All processors in this module have `blockLayout: false` set automatically by `addRewriteProcessor()`.

**Registered processors:**
1. `processInlineIconsInContainer` - Enables inline icons with `icon-*` classes
2. `injectProgressIcons` - Renders progress percentage indicators
3. `enableRecentlyModifiedLinkIcons` - Adds star icons to recently-modified links
4. `enableRecentlyModifiedLinkListIcons` - Converts icons to list-item styling
5. `designateListStyles` - Adds `.dark-mode-invert` to list items
6. `designateHorizontalRuleStyles` - Configures `<hr>` styling and custom images
7. `wrapParenthesizedInlineModeSelectors` - Wraps mode selector buttons

### Processing Order

Processors run in registration order when content loads:
1. **Inline icons** - Base icon system setup
2. **Progress indicators** - Specialized icon injection
3. **Recently-modified (standalone)** - Icon injection for non-list links
4. **Recently-modified (lists)** - Detection and conversion for list items
5. **List styling** - Dark mode classes
6. **Horizontal rules** - Styling and custom images
7. **Mode selectors** - Parenthesization wrapping

### Special Inline Icons

Icons with class `icon-special` have their `--icon-url` CSS variable set elsewhere. The module handles:
- **Progress indicators** - Dynamically generated SVG data URLs
- **Recently-modified star** - White star on black circle (`.icon-single-white-star-on-black-circle`)

Standard icons get their CSS variable set from `/static/img/icon/icons.svg#${iconName}`.

### Recently-Modified Link Handling

Links with `.link-modified-recently` get different treatment based on context:

**Standalone links** (not in lists):
- Get `.recently-modified-icon-hook` span injected
- Hook contains CSS-positioned star icon
- U+2060 WORD JOINER prevents line breaks

**List item links** (`li > p > a` or similar):
- Containing `<li>` gets `.link-modified-recently-list-item` class
- Link gets `.in-list` class
- Icon hook removed if present (CSS uses list marker instead)

Detection logic handles complex cases like transcluded annotations within list items.

### Progress Indicator Math

`arcSVGForProgressPercent()` generates:
- **Backdrop circle** - Grayscale value varies with progress (darker = more complete)
- **Progress arc** - Black stroke from 12 o'clock clockwise
- Uses trigonometry to calculate arc endpoint: `angle = 2π × (percent/100 - 0.25)`
- At 100%, renders full circle instead of arc

Stroke width: 64px mobile, 56px desktop.

### Horizontal Rule Customization

HR elements can have type classes like `horizontal-rule-{type}`:
- **Sequence classes** (`nth-1`, `nth-2`, etc.) - Left as-is
- **Special classes** (`horizontal-rule-small`) - Left as-is
- **Image classes** (any other) - Sets `--icon-image: var(--GW-image-{type})`

If the class is on a containing `<div>`, the div is unwrapped and classes moved to `<hr>`.

### Early Page Setup

Module runs initialization code outside processors:

**Page thumbnails** (non-index pages):
- Extracts `og:image` metadata on body load
- Pre-fetches image via `doAjax()`
- Injects into `.page-abstract` when found
- Skips homepage and `/index` pages

**Blog pages:**
- Adds `.blog-page` class to `<body>`

**Ref pages:**
- Clears `<title>` and `<h1>` for ID-based loading

**TOC visibility:**
- Hides if no entries or main TOC with ≤1 entry

---

## Key Patterns

### Word Joiner for Icon Styling

The U+2060 WORD JOINER character is critical for icon positioning. It's injected as a Unicode character (not HTML entity) directly into the first text node of the link:

```javascript
linkFirstTextNode.textContent = "\u{2060}" + linkFirstTextNode.textContent;
```

This prevents:
- Line breaks between icon hook and text
- Multiple text nodes (which cause text shadow artifacts)
- CSS layout issues

When multiple hooks exist (recently-modified + indicator), a separate text node with WORD JOINER is inserted between hooks.

### SVG Data URL Embedding

Progress indicators embed SVG directly in CSS:

```javascript
style: `--icon-url: url("data:image/svg+xml;utf8,${encodeURIComponent(svgSrc)}")`
```

This avoids external requests and allows dynamic generation based on progress percentage.

### Conditional Icon Removal

When a recently-modified link is detected in a list context, any existing standalone icon is removed:

```javascript
if (link.classList.contains("has-recently-modified-icon"))
    removeRecentlyModifiedIconFromLink(link);
```

This prevents duplicate styling (list marker vs inline icon).

### Progressive Enhancement

The module uses `doWhenElementExists()` and `doWhenBodyExists()` to progressively enhance content:
- Runs immediately when selector matches
- Handles dynamic content loading
- No waiting for full page load

### Option Field Merging

Functions accept option objects merged with defaults:

```javascript
options = Object.assign({
    atEnd: true,
    floatClass: "float-not"
}, options);
```

Allows partial override while maintaining defaults.

---

## Configuration

### CSS Variables

**Icon URLs:**
- `--icon-url` - Set per icon, either from SVG sprite or data URL
- `--GW-image-{type}` - Custom horizontal rule images defined in CSS

**Progress Indicator Colors:**
- Backdrop: Grayscale `110 + (percent × 0.64)` on mobile, `170 + (percent × 0.64)` on desktop
- Arc: Always black (`#000`)

### Mode Selectors

The `wrapParenthesizedInlineModeSelectors` processor targets:
- `.dark-mode-selector-inline`
- `.reader-mode-selector-inline`
- `.extracts-mode-selector-inline`
- `.search-mode-selector-inline`
- `.help-mode-selector-inline`
- `.toolbar-mode-selector-inline`

### Recently-Modified Detection

Links must have:
- `.link-modified-recently` class (set by backend)
- Either standalone or within `li > p:only-of-type` or `.data-field` in list context

### Special Page Paths

- `/blog/*` - Gets `.blog-page` class
- `/ref/*` - Title/header cleared for ID-based loading
- `/`, `/index` - Thumbnail injection skipped
- Logo URLs (`/static/img/logo/*`) - Thumbnail injection skipped

---

## Integration Points

### Events and Timing

**`doWhenBodyExists(callback)`** - Used for:
- Page thumbnail metadata extraction
- Blog page class application

**`doWhenMainExists(callback)`** - Used for:
- Ref page title clearing

**`doWhenElementExists(callback, selector)`** - Used for:
- Page abstract thumbnail injection (`#markdownBody > .abstract:first-child + *`)
- TOC visibility updates (`#TOC`)

### Shared with layout.js

**`addLayoutProcessor(processorName, processor, options)`**
- Called by `addRewriteProcessor()`
- Receives processors with `blockLayout: false`
- Manages actual processor execution

### Shared with rewrite.js

This module is the "fast" counterpart to rewrite.js:
- **rewrite-initial.js** - Non-block-layout processors
- **rewrite.js** - Block-layout-affecting processors

Both use the same registration system but different performance profiles.

### CSS Dependencies

Requires styles for:
- `.inline-icon`, `.dark-mode-invert`
- `.recently-modified-icon-hook`, `.has-recently-modified-icon`
- `.link-modified-recently-list-item`
- `.progress-indicator-icon`, `.progress-percentage-rendered`
- `.page-thumbnail-figure`, `.float-not`, `.float-left`, `.float-right`
- `.horizontal-rule-nth-1`, etc.
- `.inline-mode-selector`

### Backend Data Requirements

**HTML attributes:**
- `data-progress-percentage` - Integer 0-100 on elements needing progress indicators
- `class="link-modified-recently"` - On recently updated links

**Meta tags (for thumbnails):**
- `<meta property="og:image" content="URL">`
- `<meta property="og:image:alt" content="...">`
- `<meta property="og:image:width" content="...">`
- `<meta property="og:image:height" content="...">`
- `<meta property="gwern:thumbnail:css-classes" content="...">`
- `<meta name="og:title" content="...">` (fallback alt text)

### Image Resources

- `/static/img/icon/icons.svg` - Sprite sheet for standard inline icons
- `/static/img/logo/*` - Logo images (excluded from thumbnails)
- Custom HR images referenced via `--GW-image-{type}` CSS variables

---

## See Also

- [rewrite.js](/frontend/rewrite-js) - Block-layout rewrite processors; the "heavy" counterpart to this module
- [initial.js](/frontend/initial-js) - Earlier initialization that defines the notification center and utilities
- [layout.js](/frontend/layout-js) - Core layout processor system that addRewriteProcessor delegates to
- [utility.js](/frontend/utility-js) - Defines newElement, unwrap, URLFromString used by processors
- [dark-mode.js](/frontend/dark-mode-js) - Uses .dark-mode-invert classes set by this module
- [content.js](/frontend/content-js) - Content loading that triggers processor execution
