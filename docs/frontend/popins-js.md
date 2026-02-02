
# popins.js

**Path:** `js/popins.js` | **Language:** JavaScript | **Lines:** ~666

> Mobile-friendly inline content expansion - the touch-first alternative to floating popups.

---

## Overview

Popins provide a mobile-optimized alternative to the desktop popup system. Rather than hovering to spawn floating windows (awkward on touch devices), users tap a link and a popin slides into place directly in the document flow, pushing page content down. This approach is natural for touch interfaces where users expect content to expand inline.

The module mirrors the `Popups` API surface closely—both implement the "popframe" abstraction—allowing `extracts.js` to use either system interchangeably depending on device capabilities. Where popups use hover events and absolute positioning, popins use click events and relative positioning within the DOM tree.

Key design decisions: popins use the same Shadow DOM isolation as popups (preventing style leakage), stack vertically when spawned from within other popins (creating a "drill-down" navigation feel), and automatically scroll the page to keep the active popin visible. Tapping the semi-transparent backdrop dismisses the popin.

---

## Public API

### `Popins.setup()`

Initializes the popin system. Cleans up any remnants, registers the Escape key handler, and fires `Popins.setupDidComplete`.

**Called by:** Page initialization (`doWhenPageLoaded`)
**Calls:** `cleanup()`

---

### `Popins.addTarget(target, prepareFunction)`

Registers an element as a popin trigger. The element will spawn a popin on click (tap).

```javascript
Popins.addTarget(linkElement, (popin) => {
    // Fill popin.body with content
    // Set popin.titleBarContents = [buttons...]
    return popin;  // or null to cancel
});
```

- Binds `onclick` event to `targetClicked`
- Stores `prepareFunction` as `target.preparePopin`
- Adds class `spawns-popin` to target

**Called by:** `extracts.js`
**Calls:** Event binding

---

### `Popins.removeTarget(target)`

Unregisters an element as a popin trigger. Removes any existing popin, clears event handlers, and removes the `spawns-popin` class.

**Called by:** `extracts.js`

---

### `Popins.setPopFrameContent(popin, content)`

Replaces the popin body content. Returns `true` on success, `false` if content is null.

```javascript
Popins.setPopFrameContent(popin, contentElement);
```

**Called by:** `extracts.js`, `extracts-content.js`

---

### `Popins.containingPopFrame(element)`

Returns the popin containing the given element, or null. Handles both regular DOM children and Shadow DOM content via the `.shadow-body` reference.

```javascript
let popin = Popins.containingPopFrame(event.target);
```

**Called by:** Many internal methods, `extracts.js`

---

### `Popins.allSpawnedPopins()`

Returns the array of all spawned popins (the `spawnedPopins` stack).

**Called by:** `extracts.js`, internal cleanup

---

### `Popins.scrollElementIntoViewInPopFrame(element, alwaysRevealTopEdge)`

Scrolls a popin's internal scroll view to make the given element visible. Used for deep-linking within popin content.

**Called by:** `extracts.js`

---

### State Methods

| Method | Purpose |
|--------|---------|
| `popFrameStateLoading(popin)` | Returns `true` if `.loading` class present |
| `popFrameStateLoadingFailed(popin)` | Returns `true` if `.loading-failed` class present |
| `setPopFrameStateLoading(popin)` | Adds `.loading`, removes `.loading-failed` |
| `setPopFrameStateLoadingFailed(popin)` | Adds `.loading-failed`, removes `.loading` |
| `clearPopFrameState(popin)` | Removes both loading classes, clears provisional height |

**Called by:** `extracts.js` during content loading lifecycle

---

## Internal Architecture

### Popin DOM Structure

```
div.popin.popframe                 (backdrop via ::before/::after)
  div.popframe-title-bar           (optional, positioned absolutely above)
    span.popin-stack-counter       ("2", "3", etc. for nested popins)
    button.close-button
    [other title bar contents]
  div.popframe-scroll-view
    div.popframe-content-view
      #shadow-root (open)
        style                      (CSS reset: all: initial)
        div.popframe-body.popin-body.shadow-body
          [content goes here]
  div.popframe-loading-spinner-view
  div.popframe-loading-failed-message-view
  div.popin-footer-bar             (optional, "Open in new tab" link)
```

### Stack Model

Unlike popups which use a tree model for nested windows, popins use a simpler vertical stack. When you tap a link inside popin A, popin B is inserted *before* A in the DOM, pushing A down:

```javascript
// DOM order (visual: top to bottom)
containingDocument.popin.parentElement.insertBefore(popin, containingDocument.popin);

// spawnedPopins array order (most recent first)
Popins.spawnedPopins.unshift(popin);
```

The stack counter in the title bar shows nesting depth ("2" for second level, etc.).

### Insertion Point Algorithm

Popins must be inserted carefully to avoid breaking page layout. The module avoids inserting inside:
- `.table-wrapper` (would break table scrolling)

```javascript
let cannotInsertIntoTheseThingsSelector = [
    ".table-wrapper"
].join(", ");
do {
    insertionTarget = insertWhere ?? target;
    insertWhere = insertionTarget.parentElement;
} while (insertWhere.closest(cannotInsertIntoTheseThingsSelector));
```

### Ancestor Marking

When a popin spawns, all its ancestors up to `<MAIN>` or `<ARTICLE>` get the `.popin-ancestor` class. This enables CSS tricks like adding bottom margin to push the rest of the page content down:

```css
.markdownBody .popin-ancestor {
    position: relative !important;
    z-index: 100 !important;
    margin-bottom: 75vh;  /* room for popin content */
}
```

---

## Key Patterns

### Backdrop Dismissal

Popins create a backdrop via CSS `::before` and `::after` pseudo-elements. The popin element itself is sized to fill available space, with its visual content positioned inside. Clicking the popin element (not its children) dismisses it:

```javascript
popinClicked: (event) => {
    // Only dismiss if click is directly on popin backdrop
    if (event.target.classList.contains("popin") == false)
        return;

    event.stopPropagation();
    Popins.removePopin(event.target);
}
```

### Scroll State Preservation

When pushing a popin down the stack, its scroll position is saved:

```javascript
containingDocument.popin.lastScrollTop = containingDocument.popin.scrollView.scrollTop;
```

When popping (removing the top popin), the scroll position is restored:

```javascript
if (popinBelow) {
    popinBelow.scrollView.scrollTop = popinBelow.lastScrollTop;
}
```

### Window Scroll Compensation

Popins track how much they've scrolled the main window and restore it on dismissal:

```javascript
// On inject: record scroll offset
popin.dataset.windowScrollOffset = windowScrollOffsetForThisPopin + scrollWindowBy;

// On remove: restore window scroll
window.scrollBy(0, -1 * parseInt(popin.dataset.windowScrollOffset ?? '0'));
```

### Height Inheritance

When spawning a popin from within another popin while content is still loading, the new popin inherits the parent's height to avoid layout thrashing:

```javascript
if (options.inheritInitialHeight && Popins.popFrameStateLoading(popin))
    popin.style.height = Math.round(containingDocument.popin.clientHeight) + "px";
```

---

## Configuration

| Property | Default | Purpose |
|----------|---------|---------|
| `windowTopPopinPositionMargin` | `0.0` | Top viewport margin for scroll positioning |
| `windowBottomPopinPositionMargin` | `0.0` | Bottom viewport margin for scroll positioning |
| `rootDocument` | `document` | Root document for `containingDocumentForTarget` |

CSS variables control visual appearance (in `default.css`):

| Variable | Purpose |
|----------|---------|
| `--GW-popins-popin-background-color` | Popin background |
| `--GW-popins-popin-border-color` | Border color |
| `--GW-popins-popin-border-width` | Border thickness |
| `--GW-popins-popin-title-bar-background-color` | Title bar background |
| `--GW-popins-popin-title-bar-height` | Title bar height |
| `--GW-popins-popin-title-bar-button-color` | Button icon color |

---

## Integration Points

### Events Fired (via GW.notificationCenter)

| Event | Payload | When |
|-------|---------|------|
| `Popins.didLoad` | — | Module loaded |
| `Popins.setupDidComplete` | — | `setup()` finished |
| `Popins.cleanupDidComplete` | — | `cleanup()` finished |
| `Popins.popinDidInject` | `{ popin }` | Popin injected into DOM |
| `Popins.popinWillDespawn` | `{ popin }` | About to remove popin |

### Hooks for Targets

Targets can provide optional methods for customization:

```javascript
target.preparePopin = (popin) => { ... };  // Required: fill content
target.adjustPopinWidth = (popin) => { ... };  // Optional: width adjustment
```

### Shared State

- `Popins.spawnedPopins`: Array of all spawned popins (stack order)
- `Popins.rootDocument`: Base document reference

### Title Bar Components

Factory methods for title bar elements (parallel to Popups):

| Method | Returns |
|--------|---------|
| `titleBarComponents.genericButton()` | Base button element |
| `titleBarComponents.closeButton()` | Close button with "×" icon |
| `titleBarComponents.optionsButton()` | Gear icon button (no default action) |

---

## Popins vs Popups

| Aspect | Popups | Popins |
|--------|--------|--------|
| Trigger | Hover (mouseenter) | Click/tap (onclick) |
| Position | Floating, absolute | Inline, in document flow |
| Nesting | Tree (popup stack) | Vertical stack |
| Dismissal | Mouse leave + timer | Tap backdrop or Escape |
| Features | Pin, tile, drag, resize, minimize | Close only |
| Target class | `spawns-popup` | `spawns-popin` |
| Container | Global popup container | Inserted near target |
| Use case | Desktop, mouse users | Mobile, touch users |

The simpler feature set (no windowing, no tiling) is intentional—popins are meant to be quick, ephemeral views rather than persistent windows.

---

## See Also

- [popups.js](/frontend/popups-js) - Desktop popup system with hover-based triggering and full windowing
- [extracts.js](/frontend/extracts-js) - Content coordinator that chooses popups vs popins based on device
- [extracts-options.js](/frontend/extracts-options-js) - User preference UI for enabling/disabling popins
- [extracts-content.js](/frontend/extracts-content-js) - Content type definitions for popin content
- [initial.js](/frontend/initial-js) - GW namespace and notification center for popin events
- [content.js](/frontend/content-js) - Content loading system that provides popin data

