
# misc.js

**Path:** `js/misc.js` | **Language:** JavaScript | **Lines:** ~2,804

> Utility modules and UI components: inject triggers, image handling, clipboard, toolbar, floating header, and miscellaneous helpers

---

## Overview

`misc.js` is a collection of standalone utility modules and UI components that don't fit elsewhere. Despite its name, it contains substantial functionality: element injection triggers, asset versioning, image thumbnail/inversion/outlining helpers, clipboard processing, the page toolbar (gear icon with collapsible settings), the floating header (breadcrumb navigation), pop-frame spawn widgets (help/search), keyboard command handling, and various DOM/document utilities.

The file follows a modular organization, with each feature in its own section. Many features integrate with the notification center (`GW.notificationCenter`) for loose coupling. The page toolbar and floating header are the most complex subsystems, providing the site's primary UI controls on both desktop and mobile.

Key design decisions include: lazy SVG icon loading with placeholders, an external API for image inversion/outlining decisions (`invertornot.com`), a sophisticated toolbar demo system (auto-collapses after showing new users the controls), and a widget system for spawning help/search popups from toolbar buttons or keyboard shortcuts.

---

## Public API

### Inject Triggers

#### `onInject(uuid, f) → uuid`

Registers a function to run when an element with matching `data-uuid` is injected into the DOM.

```javascript
let uuid = onInject(null, (element) => {
    element.classList.add("processed");
});
// Returns auto-generated UUID if null passed
```

**Called by:** `placeholder()`, internal systems
**Calls:** MutationObserver system

---

#### `placeholder(replaceFunction, wrapperFunction) → string`

Returns HTML for a placeholder `<span>` that self-replaces when injected.

```javascript
let html = placeholder(
    (element) => elementFromHTML("<div>Replacement</div>"),
    doWhenSVGIconsLoaded  // Optional delay wrapper
);
```

**Called by:** `GW.svg()` (for deferred icon rendering)

---

### Asset Management

#### `versionedAssetURL(pathname) → URL`

Returns a fully-qualified URL with version query parameter for cache busting.

```javascript
versionedAssetURL("/static/img/icon/icons.svg")
// → URL object: ".../icons.svg?v=abc123"
```

**Called by:** Icon loading, dropcap system, special occasions

---

#### `getAssetPathname(pattern, options) → string|null`

Returns an asset pathname matching a regex pattern with `%R` as number placeholder.

```javascript
getAssetPathname("/static/img/logo/logo-%R.svg", {
    sequenceIndex: 1,      // Get specific index
    sequenceCurrent: null  // Or use "next"/"previous" with current path
});
```

**Called by:** Dropcap system, special occasion logos

---

#### `GW.svg(icon) → string`

Returns SVG markup for a named icon from the loaded icon file.

```javascript
GW.svg("gear-solid")  // Returns <svg>...</svg> string
// Returns placeholder if icons not yet loaded
```

**Called by:** Page toolbar, floating header, all UI components

---

### Images

#### `Images.thumbnailURLForImage(image, size) → URL|null`

Returns thumbnail URL for an image, or null if SVG or external.

---

#### `Images.thumbnailifyImage(image) → void`

Replaces image src with thumbnail URL, saving original in `data-src-size-full`.

---

#### `Images.unthumbnailifyImage(image) → void`

Restores original full-size image src.

---

#### `requestImageInversionJudgmentsForImagesInContainer(container) → void`

Sends images to `invertornot.com` API to determine if they should be inverted in dark mode.

**Fires:** `GW.imageInversionJudgmentsAvailable`

---

#### `applyImageInversionJudgment(image) → boolean|null`

Applies `.invert-auto` or `.invert-not-auto` class based on API response.

---

### Clipboard

#### `copyTextToClipboard(text) → void`

Copies text to clipboard using a hidden scratchpad element.

---

#### `addCopyProcessor(processor) → void`

Registers a function to process clipboard content on copy events.

```javascript
addCopyProcessor((event, selection) => {
    // Modify selection DocumentFragment
    return true;  // Continue to next processor
});
```

---

#### `registerCopyProcessorsForDocument(doc) → void`

Sets up copy event handling for a document (main or shadow root).

---

### Notes & Citations

#### `Notes.allNotesForCitation(citation) → Element[]`

Returns all footnote/sidenote elements associated with a citation link.

---

#### `Notes.noteNumberFromHash(hash) → string`

Extracts note number from URL hash (`#fn3` → `"3"`, `#fnref3` → `"3"`).

---

### Table of Contents

#### `updatePageTOC(container) → void`

Updates main TOC with any new sections from transcluded content.

**Called by:** `transclude.js`, rewrite handlers

---

### Page Toolbar

#### `GW.pageToolbar.addWidget(widget) → Element`

Adds a widget element to the toolbar.

```javascript
GW.pageToolbar.addWidget(`<div id="my-widget">...</div>`);
```

---

#### `GW.pageToolbar.removeWidget(widgetID) → Element|null`

Removes and returns the widget with given ID.

---

#### `GW.pageToolbar.toggleCollapseState(collapse, options) → void`

Collapses or expands the toolbar.

```javascript
GW.pageToolbar.toggleCollapseState(true, {
    delay: 500,   // Delay before action
    temp: false,  // Temporary expand (re-collapse on unhover)
    slow: false   // Slow collapse animation
});
```

---

#### `GW.pageToolbar.flashWidget(widgetID, options) → void`

Flashes a widget to draw attention.

---

### Floating Header

#### `GW.floatingHeader.updateState(event, maxChainLength) → void`

Updates header visibility and breadcrumb chain based on scroll position.

**Called by:** Scroll listener

---

### Pop-Frame Spawn Widgets

#### `GW.popFrameSpawnWidgets.addWidgetType(name, spec) → object`

Registers a new widget type for toolbar/inline use.

```javascript
GW.popFrameSpawnWidgets.addWidgetType("help", {
    linkHref: "/help",
    iconName: "question-solid",
    addToolbarWidget: true,
    toolbarWidgetLabel: "Help",
    keyCommand: "?"
});
```

---

### Miscellaneous Helpers

#### `scrollContainerOf(element) → Element|null`

Returns the scroll container for an element (pop-frame scroll view, or null for viewport).

---

#### `getPageScrollPosition() → number`

Returns scroll position as integer percentage (0-100).

---

#### `getSavedCount(key) → number` / `incrementSavedCount(key)` / `resetSavedCount(key)`

Local storage helpers for tracking counts (e.g., toolbar demo views).

---

#### `addUIElement(element, options) → Element`

Adds element to `#ui-elements-container`.

---

## Internal Architecture

### Inject Trigger System

```
GW.elementInjectTriggers = { uuid: callback, ... }
GW.defunctElementInjectTriggers = { uuid: callback, ... }

MutationObserver watches document
  → On added node with data-uuid
    → Look up callback in elementInjectTriggers
    → Move to defunctElementInjectTriggers
    → Execute callback(element)
```

The system enables deferred rendering—SVG icons return placeholder spans that self-replace once the icon file loads.

---

### Page Toolbar State Machine

```
States:
  - expanded (default for first N visits)
  - collapsed
  - expanded-temp (on hover, will re-collapse)
  - faded (while scrolling down)

Transitions:
  - Page load → Demo flash sequence → Auto-collapse (slow)
  - Scroll up → Unfade
  - Scroll down → Fade
  - Hover toggle button → Expand temp
  - Unhover → Collapse (if temp)
  - Click toggle → Toggle collapsed/expanded
  - Click while temp-expanded → Lock expanded
```

Demo count tracked in localStorage; after `maxDemos` (1), toolbar starts collapsed.

---

### Floating Header

On mobile: positioned at bottom, shows breadcrumb trail to current section.
On desktop: positioned at top, shows nested section links.

```
getTrail(lookAhead) → ["header", "#section1", "#section1.1"]

constructLinkChain(trail, maxLength) → [<a href="#top">, <a href="#section1">, ...]

If trail too long:
  - Splice with "…" ellipsis link
  - On mobile: also set truncate-page-title class
```

---

### Widget Type Registration

```javascript
widgetTypes = {
    help: { linkHref, iconName, addToolbarWidget, keyCommand, ... },
    search: { ..., additionalSetup, additionalWidgetActivation, ... }
}

setup(widgetType):
  - If addToolbarWidget: add to GW.pageToolbar
  - If inlineWidgetReplacedElementSelector: replace matching elements
  - If keyCommand: register keyboard handler
  - Run additionalSetup if provided
```

---

## Key Patterns

### Placeholder-Based Deferred Rendering

```javascript
GW.svg = (icon) => {
    if (GW.svgIconFile == null)
        return placeholder(
            element => elementFromHTML(GW.svg(icon)),
            doWhenSVGIconsLoaded
        );
    // ... return actual SVG
};
```

When icons aren't loaded yet, returns a placeholder that auto-replaces. The wrapper function (`doWhenSVGIconsLoaded`) delays replacement until icons are available.

---

### Asset Sequencing

For assets with variants (dropcaps, logos), supports:
- Random selection (default)
- Sequential access (`next`/`previous`)
- Persistent index via localStorage

```javascript
processAssetSequenceOptions(options, { assetSavedIndexKey }) → {
    sequenceIndex: number|string,
    sequenceCurrent: string
}
```

---

### Conditional Event Handlers

```javascript
GW.notificationCenter.addHandlerForEvent("Popups.popupDidSpawn", handler, {
    once: true,
    condition: (info) => (info.popup.spawningTarget == widget.widgetLink)
});
```

Pattern used throughout for targeted one-shot event handling.

---

## Configuration

### Page Toolbar Timing

```javascript
GW.pageToolbar = {
    maxDemos: 1,
    hoverUncollapseDelay: 400,
    unhoverCollapseDelay: 2500,
    demoCollapseDelay: 9000,
    collapseDuration: 250,        // Synced with CSS
    demoCollapseDuration: 1000,   // Synced with CSS
    widgetFlashRiseDuration: 1000,
    widgetFlashFallDuration: 1000,
    widgetFlashStayDuration: 500
};
```

---

### Floating Header

```javascript
GW.floatingHeader = {
    minimumYOffset: 0,    // Calculated from page header height
    maxChainLength: 6,    // Max breadcrumb links
    maxHeaderHeight: 60   // Mobile only, triggers chain truncation
};
```

---

### Margin Notes

```javascript
GW.marginNotes = {
    minimumAggregatedNotesCount: 3  // Hide block if fewer notes
};
```

---

### Image APIs

```javascript
GW.invertOrNotAPIEndpoint = "https://invertornot.com/api/url";
GW.outlineOrNotAPIEndpoint = "https://api.obormot.net/outlineornot/url";
```

---

## Integration Points

### Events Fired

| Event | When | Payload |
|-------|------|---------|
| `GW.SVGIconsLoaded` | Icon file fetched | — |
| `GW.imageInversionJudgmentsAvailable` | API response received | `{ judgments }` |
| `GW.imageOutliningJudgmentsAvailable` | API response received | `{ judgments }` |
| `GW.pageToolbarCollapseStateDidChange` | Toolbar expand/collapse | `{ collapse, collapseOptions }` |
| `GW.keyWasPressed` | Keyboard command | `{ key, altKey, ... }` |
| `GW.hashDidChange` | URL hash changed | `{ oldHash }` |
| `GW.hashHandlingSetupDidComplete` | Hash system ready | — |

---

### Events Listened

| Event | Handler |
|-------|---------|
| `DarkMode.computedModeDidChange` | Dropcap image updates |
| `Popups.popupDidSpawn` | Widget popup pinning |
| `Popups.popupWillDespawn` | Search popup cleanup |

---

### Shared State

- `GW.svgIconFile` — Parsed SVG icon document
- `GW.assetVersions` — Asset path → version hash map
- `GW.invertOrNot` / `GW.outlineOrNot` — Image URL → judgment cache
- `GW.scrollState` — Current scroll position/direction (from initial.js)
- `GW.activities` — Active operation stack
- `GW.TOC.mainTOC` — Cached TOC element reference
- `Notes.notesForCitations` — Citation → notes cache

---

## See Also

- [initial.js](/frontend/initial-js) - `GW` namespace, notification center, and scroll state
- [rewrite.js](/frontend/rewrite-js) - DOM rewrite handlers (TOC updates, dropcaps)
- [utility.js](/frontend/utility-js) - Core DOM helpers used throughout misc.js
- [popups.js](/frontend/popups-js) - Popup windowing system (toolbar spawns popups)
- [extracts.js](/frontend/extracts-js) - Pop-frame coordinator (widget links are extract targets)
- [dark-mode.js](/frontend/dark-mode-js) - Dark mode system (image inversion integration)
- [console.js](/frontend/console-js) - Developer console utility module
