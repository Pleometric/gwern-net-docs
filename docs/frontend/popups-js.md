
# popups.js

**Path:** `js/popups.js` | **Language:** JavaScript | **Lines:** ~2,700

Hover-triggered popup windows with full window-management capabilities (pinning, tiling, minimizing).

---

## Overview

Popups began as simple footnote tooltips (2010, by Lukas Mathis) and evolved into a complete windowing system. When users hover over annotated links, a popup spawns showing the link's content. Users can then pin popups to keep them visible, tile them to screen regions, minimize them to a dock, or drag/resize them freely.

The module handles only popup mechanics—positioning, lifecycle, z-ordering, and user interaction. It does *not* decide what content goes in a popup; that responsibility belongs to `extracts.js`, which registers targets via `Popups.addTarget()` and provides a `preparePopup` callback to fill content.

Key design decisions: popups use Shadow DOM for content isolation (preventing style leakage), maintain a popup stack for nested popups (hovering a link inside a popup spawns a child), and fire events through `GW.notificationCenter` so other modules can react to popup lifecycle changes.

---

## Public API

### `Popups.setup()`

Initializes the popup system. Creates the popup container, registers global event listeners (Escape key, scroll, mousemove), and fires `Popups.setupDidComplete`.

**Called by:** Page initialization
**Calls:** `cleanup()`, creates popup container element

---

### `Popups.addTarget(target, prepareFunction)`

Registers an element as a popup trigger. The element will spawn a popup on hover.

```javascript
Popups.addTarget(linkElement, (popup) => {
    // Fill popup.body with content
    // Set popup.titleBarContents = [buttons...]
    return popup;  // or null to cancel
});
```

- Binds `mouseenter`/`mouseleave`/`mousedown` events
- Stores `prepareFunction` as `target.preparePopup`
- Adds class `spawns-popup` to target

**Called by:** `extracts.js`
**Calls:** Event binding

---

### `Popups.removeTarget(target)`

Unregisters an element as a popup trigger. Removes event listeners and despawns any existing popup.

**Called by:** `extracts.js`

---

### `Popups.spawnPopup(target, spawnPoint)`

Creates and displays a popup for the given target at the specified spawn point.

```javascript
Popups.spawnPopup(target, { x: event.clientX, y: event.clientY });
```

Returns the popup element, or undefined if preparation failed.

**Called by:** Timer callback, `extracts.js`, `extracts-annotations.js`
**Calls:** `newPopup()`, `target.preparePopup()`, `attachPopupToTarget()`, `injectPopup()`, `positionPopup()`

---

### `Popups.despawnPopup(popup)`

Removes a popup from the page. Fires `popupWillDespawn` before removal.

**Called by:** Timer callback, close button, Escape key, various cleanup paths

---

### `Popups.setPopFrameContent(popup, content)`

Replaces the popup body content. Returns `true` on success, `false` if content is null.

```javascript
Popups.setPopFrameContent(popup, contentElement);
```

**Called by:** `extracts.js`, `extracts-content.js`

---

### `Popups.containingPopFrame(element)`

Returns the popup containing the given element, or null. Handles both regular popup children and Shadow DOM content.

```javascript
let popup = Popups.containingPopFrame(event.target);
```

**Called by:** Many internal methods, `extracts.js`

---

### `Popups.allSpawnedPopups()`

Returns array of all visible popups, sorted by z-index (back to front). Excludes fading popups.

**Called by:** Many internal methods, `extracts.js`

---

### State Query Methods

| Method | Returns |
|--------|---------|
| `popupIsPinned(popup)` | `true` if popup has `.pinned` class |
| `popupIsZoomed(popup)` | `true` if popup has `.zoomed` class |
| `popupIsMaximized(popup)` | `true` if zoomed to "full" |
| `popupIsCollapsed(popup)` | `true` if popup has `.collapsed` class |
| `popupIsMinimized(popup)` | `true` if popup has `.minimized` class |
| `popupWasResized(popup)` | `true` if popup has `.resized` class |

---

## Internal Architecture

### Popup DOM Structure

```
div.popup.popframe
  div.popframe-title-bar          (optional)
    button.close-button
    button.zoom-button
    button.pin-button
    button.minimize-button
    ...
  div.popframe-scroll-view
    div.popframe-content-view
      #shadow-root (open)
        style                     (CSS reset)
        div.popframe-body.popup-body.shadow-body
          [content goes here]
  div.popframe-loading-spinner-view
  div.popframe-loading-failed-message-view
```

### Popup Stack Model

Popups form a stack based on nesting. When you hover a link inside popup A, the new popup B joins A's stack:

```javascript
popup.popupStack = [popupA, popupB];  // B is child of A
```

When the mouse leaves a popup, the entire ancestor stack starts fading. When a new popup spawns, all non-pinned popups outside its stack are despawned. This creates the "tooltip chain" behavior where moving through nested popups keeps the chain alive.

Pinned popups are removed from their stack (`popup.popupStack.remove(popup)`) and become independent.

### State Model (CSS Classes)

Popup state is tracked via CSS classes on both the popup element and its `.shadow-body`:

| Class | Meaning |
|-------|---------|
| `pinned` / `unpinned` | Fixed position, won't auto-despawn |
| `zoomed` | Tiled to a screen region |
| `full`, `left`, `right`, `top`, `bottom`, `top-left`... | Zoom position |
| `collapsed` | Only title bar visible |
| `minimized` | Docked at screen bottom |
| `resized` | User manually resized |
| `focused` | Currently focused popup |
| `dragging` | Being dragged |
| `resizing` | Being resized |
| `fading` | In fadeout animation |
| `loading` | Content loading |
| `loading-failed` | Content failed to load |

Use `Popups.addClassesToPopFrame()` and `Popups.removeClassesFromPopFrame()` to modify both the popup and its shadow body simultaneously.

### Position Lifecycle

1. **Initial spawn:** `positionPopup()` calculates position relative to target
   - Prefers above/below target for top-level popups
   - Prefers left/right of target for nested popups
   - Clamps to viewport edges
2. **Pinning:** Position becomes `fixed`, stored in `popup.viewportRect`
3. **Zooming:** Position calculated from zoom region (e.g., `top-left` = 0,0)
4. **Restoring:** Returns to saved `originalXPosition`/`originalYPosition`
5. **Unminimizing:** Returns to saved `previousXPosition`/`previousYPosition`

---

## Key Patterns

### Shadow DOM Content Isolation

Popup content lives in a Shadow DOM to prevent style conflicts:

```javascript
popup.document = popup.contentView.attachShadow({ mode: "open" });
popup.document.body = popup.body = popup.shadowBody =
    popup.document.appendChild(newElement("DIV", {
        class: "popframe-body popup-body shadow-body"
    }));

// CSS reset injected into shadow root
popup.document.insertBefore(
    newElement("STYLE", null, { innerHTML: `.shadow-body { all: initial; }` }),
    popup.body
);
```

The `popup.document` reference acts like `document` for content within the popup. `popup.body` is the root element for popup content.

### Bidirectional References

Popups maintain cross-references for navigation:

```javascript
// Target -> Popup
target.popup = popup;
target.popFrame = popup;  // alias

// Popup -> Target
popup.spawningTarget = target;

// Shadow body -> Popup (for containingPopFrame)
popup.body.popup = popup;
popup.document.popup = popup;
```

### Timer-Based Hover Lifecycle

```javascript
// On target mouseenter: start spawn timer
target.popupSpawnTimer = setTimeout(() => {
    Popups.spawnPopup(target, ...);
}, Popups.popupTriggerDelay);  // 750ms default

// On target/popup mouseleave: start fade timer
target.popupFadeTimer = setTimeout(() => {
    // After fadeoutDelay (100ms), start despawn timer
    target.popupDespawnTimer = setTimeout(() => {
        Popups.despawnPopup(popup);
    }, Popups.popupFadeoutDuration);  // 250ms
}, Popups.popupFadeoutDelay);

// On re-enter: clear all timers
Popups.clearPopupTimers(target);
```

### Title Bar Component Factory

Title bar buttons are created via factory methods in `Popups.titleBarComponents`:

```javascript
popup.titleBarContents = [
    Popups.titleBarComponents.closeButton(),
    Popups.titleBarComponents.zoomButton().enableSubmenu(),
    Popups.titleBarComponents.pinButton(),
    Popups.titleBarComponents.minimizeButton()
];
```

Each button has:
- `buttonAction`: Click handler
- `updateState()`: Sync visual state with popup state

---

## Configuration

All configuration lives in the `Popups` object:

| Property | Default | Purpose |
|----------|---------|---------|
| `popupContainerID` | `"popup-container"` | ID of container element |
| `popupContainerParentSelector` | `"body"` | Where to inject container |
| `popupContainerZIndex` | `"10000"` | Base z-index |
| `popupBreathingRoomX` | `12.0` | Horizontal gap from target |
| `popupBreathingRoomY` | `8.0` | Vertical gap from target |
| `popupBreathingRoomYTight` | `-4.0` | Vertical gap in tight mode |
| `popupTriggerDelay` | `750` | ms before popup spawns |
| `popupFadeoutDelay` | `100` | ms before fadeout starts |
| `popupFadeoutDuration` | `250` | ms of fadeout animation |
| `popupTilingControlKeys` | `"aswdqexzfrcvtgb"` | Keyboard shortcuts (localStorage) |

Minimized popup dimensions are in `minimizedPopupsArrangements`:
- Vertical layout: fixed 480px width
- Horizontal layout: 320-640px flexible width

---

## Integration Points

### Events Fired (via GW.notificationCenter)

| Event | Payload | When |
|-------|---------|------|
| `Popups.didLoad` | — | Module loaded |
| `Popups.setupDidComplete` | — | `setup()` finished |
| `Popups.cleanupDidComplete` | — | `cleanup()` finished |
| `Popups.popupDidSpawn` | `{ popup }` | Popup injected and positioned |
| `Popups.popupWillDespawn` | `{ popup }` | About to remove popup |

### Events Listened

The module registers handlers via `GW.notificationCenter.addHandlerForEvent()` for its own events to coordinate behavior (e.g., despawning non-stack popups when a new one spawns).

### Hooks for Targets

Targets can customize behavior via optional methods:

```javascript
target.preferPopupSidePositioning = () => true;  // spawn left/right instead of above/below
target.cancelPopupOnClick = () => false;         // don't despawn on click
target.keepPopupAttachedOnPin = () => true;      // maintain target.popup reference when pinned
target.specialPopupTriggerDelay = 500;           // custom spawn delay (ms or function)
```

### Shared State

- `Popups.popupContainer`: The container element for all popups
- `Popups.popupBeingDragged`: Currently dragged popup (or null)
- `Popups.popupBeingResized`: Currently resized popup (or null)
- `Popups.hoverEventsActive`: Disabled during scroll, re-enabled on mousemove

### Coordinate with extracts.js

`extracts.js` is the primary consumer:
- Calls `Popups.addTarget()` to register links
- Implements `preparePopup` callbacks to fill content
- Uses `Popups.allSpawnedPopups()` and `Popups.containingPopFrame()` for context

---

## Additional Features (Not Covered in Detail)

- **Dragging:** Title bar mousedown initiates drag; pins popup automatically
- **Resizing:** Edge/corner mousedown on pinned popups; uses `edgeOrCorner()` to determine direction
- **Tiling:** Keyboard shortcuts (default: `aswdqexzfrcvtgb`) zoom popups to screen regions
- **Minimizing:** Popups dock at screen bottom in configurable arrangements (vertical/horizontal)
- **Z-ordering:** `bringPopupToFront()`, `sendPopupToBack()`, focus management

---

## See Also

- [extracts.js](/frontend/extracts-js) - Content coordinator that decides what content appears in popups
- [popins.js](/frontend/popins-js) - Mobile-friendly alternative using inline expansion instead of floating windows
- [extracts-annotations.js](/frontend/extracts-annotations-js) - Annotation content type for popups
- [extracts-content.js](/frontend/extracts-content-js) - Content type definitions (images, videos, local pages)
- [initial.js](/frontend/initial-js) - GW namespace and notification center used for popup events
- [content.js](/frontend/content-js) - Content loading system that provides popup data
- [annotations.js](/frontend/annotations-js) - Annotation data loading and caching
