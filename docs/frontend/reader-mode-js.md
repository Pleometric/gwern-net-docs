
# reader-mode.js

**Path:** `js/reader-mode.js` | **Language:** JavaScript | **Lines:** ~573

> Full reader mode UI with link masking, mode selector widget, and auto-deactivation

---

## Overview

`reader-mode.js` extends the minimal `ReaderMode` object from `reader-mode-initial.js` with the complete reader mode experience. Its primary feature is **link masking**—hiding hyperlinks within prose paragraphs to create a cleaner reading experience similar to a printed book. When active, links appear as plain text until the user hovers over them or holds the Alt key.

The module provides a tri-state mode selector (Auto/On/Off) that can be injected into both the page toolbar and inline positions within content. In "auto" mode, reader mode respects author tagging (the `.reader-mode` body class) while also **automatically deactivating** when the user scrolls past the main content into sections like "See Also" or "External Links"—acknowledging that reference sections need visible links.

The design prioritizes non-intrusiveness: links reveal themselves on hover with a delay, a tooltip explains the Alt key toggle, and popup trigger delays are extended to prevent accidental popups while exploring masked text.

---

## Public API

### `ReaderMode.setup()`

Initializes the full reader mode system. Activates reader mode if enabled, injects the toolbar mode selector, and sets up rewrite processors for inline selectors.

**Called by:** Script initialization (after Extracts loads)
**Calls:** `activate`, `injectModeSelector`, `processMainContentAndAddRewriteProcessor`

---

### `ReaderMode.active() -> boolean`

Returns `true` if reader mode is currently active (checks for `reader-mode-active` body class).

**Called by:** `setMode`, mode selector state updates
**Calls:** `document.body.classList.contains`

---

### `ReaderMode.setMode(selectedMode?) -> void`

Sets the reader mode to the given mode (or current mode if not specified). Saves to localStorage, activates/deactivates as needed, and manages the scroll-based deactivation observer for "auto" mode.

**Called by:** `modeSelectButtonClicked`, `ReaderMode.didLoad` handler (from initial.js)
**Calls:** `saveMode`, `activate`, `deactivate`, `spawnObserver`, `despawnObserver`

---

### `ReaderMode.activate() -> void`

Fully activates reader mode: adds body classes, masks all links matching `maskedLinksSelector`, installs hover/keyboard handlers, injects the Alt-key info alert, and fires the `didActivate` event.

**Called by:** `setup`, `setMode`
**Calls:** `updateVisibility`, fires `ReaderMode.didActivate`

---

### `ReaderMode.deactivate() -> void`

Fully deactivates reader mode: removes body classes, unmasks links, removes event handlers, updates document title, and fires `didDeactivate`. Triggers sidenote re-layout.

**Called by:** `setMode`, intersection observer callback, click-to-disable elements
**Calls:** Fires `ReaderMode.didDeactivate`, `Sidenotes.updateSidenotePositionsIfNeeded`

---

### `ReaderMode.maskedLinksVisible() -> boolean`

Returns `true` if masked links are currently visible (user is hovering or Alt is pressed).

**Called by:** `updateVisibility`, link `onclick` handlers, popup delay calculation
**Calls:** `document.body.classList.contains`

---

## Internal Architecture

### State Object

```javascript
ReaderMode.state = {
    hoveringOverLink: false,  // Mouse is over a masked link
    altKeyPressed: false      // Alt/Option key is held down
};
```

### Mode Options

Three modes are available, each with display labels and icons:

| Mode | Behavior |
|------|----------|
| `auto` | Enable on pages with `.reader-mode` class; auto-deactivate on scroll past main content |
| `on` | Enable on all pages |
| `off` | Disable everywhere |

### Link Masking Flow

```
ReaderMode.activate()
    │
    ├─→ Add body classes: reader-mode-active, masked-links-hidden
    │
    ├─→ Query all links matching maskedLinksSelector ("p a")
    │
    ├─→ For each link (desktop only):
    │       ├─→ Add mouseenter handler (show links after 250ms delay)
    │       ├─→ Add mouseleave handler (hide links immediately)
    │       ├─→ Add specialPopupTriggerDelay (2400ms when masked)
    │       └─→ Override onclick (block clicks when masked)
    │
    ├─→ Inject Alt-key info alert panel
    │
    ├─→ Add keydown/keyup listeners for Alt toggle
    │
    └─→ Fire ReaderMode.didActivate event
```

### Auto-Deactivation via Intersection Observer

When in "auto" mode, the module uses an IntersectionObserver to detect when the user scrolls past the main content:

```javascript
deactivateTriggerElementSelector: [
    ".reader-mode-disable-when-here",
    "#see-also",
    "#external-links",
    "#appendix",
    "#appendices",
    "#navigation",
    "#footer",
    "#footer-decoration-container"
].join(", ")
```

Only the first matching element is observed (`document.querySelector`), so reader mode deactivates when that single element becomes fully visible (`threshold: 1.0`).

---

## Key Patterns

### Progressive Link Revelation

Links don't reveal immediately on hover—there's a 250ms delay to prevent visual flicker during casual mouse movement:

```javascript
link.removeMouseEnterEvent = onEventAfterDelayDo(link, "mouseenter",
    ReaderMode.showMaskedLinksDelay, ReaderMode.updateState, {
        cancelOnEvents: [ "mouseleave" ]
    });
```

### Click Suppression on Masked Links

When links are masked, clicking does nothing. This prevents accidental navigation when the user might be selecting text:

```javascript
link.onclick = (event) => {
    return (ReaderMode.maskedLinksVisible() == true);
};
```

The original onclick handler is saved in `link.savedOnClick` and restored on deactivation.

### Extended Popup Delay

To prevent popups from triggering while exploring masked text, a custom delay function is installed:

```javascript
link.specialPopupTriggerDelay = () => {
    return (ReaderMode.maskedLinksVisible() == false
            ? ReaderMode.adjustedPopupTriggerDelay  // 2400ms
            : Popups.popupTriggerDelay);            // normal delay
};
```

### Dual Trigger Element Deactivation

The auto-deactivation uses both a primary observer and a backup observer. If the page lacks the primary trigger elements (e.g., no "#see-also" section), the backup observer on `#footer-decoration-container` ensures deactivation still happens.

---

## Configuration

| Property | Default | Description |
|----------|---------|-------------|
| `maskedLinksSelector` | `"p a"` | CSS selector for links to mask |
| `showMaskedLinksDelay` | `250` | ms delay before showing links on hover |
| `adjustedPopupTriggerDelay` | `2400` | ms popup delay when links masked |
| `deactivateTriggerElementSelector` | See above | Elements that trigger auto-deactivation |
| `deactivateOnClickTriggerElementSelector` | `[".reader-mode-disable-when-clicked"]` | Elements that deactivate on click |

**localStorage key:** `reader-mode-setting` (values: `"on"`, `"off"`, or absent for auto)

---

## Integration Points

### Events Fired

| Event | When | Payload |
|-------|------|---------|
| `ReaderMode.didLoad` | Script finishes loading | None |
| `ReaderMode.didSetMode` | After `setMode()` completes | None |
| `ReaderMode.didActivate` | After `activate()` completes | None |
| `ReaderMode.didDeactivate` | After `deactivate()` completes | None |

### Events Listened

| Event | Handler |
|-------|---------|
| `Extracts.didLoad` | Calls `setup()` (ensures popup handlers exist first) |

### Body Classes

| Class | Meaning |
|-------|---------|
| `reader-mode-active` | Reader mode is currently on |
| `masked-links-hidden` | Links in masked areas appear as plain text |

### Shared State

- Modifies `document.title` (appends " [reader mode]")
- Coordinates with `Sidenotes` for re-layout after deactivation
- Coordinates with `Popups` via `specialPopupTriggerDelay` property on links
- Integrates with `GW.pageToolbar` for the mode selector widget

### DOM Elements Injected

- Mode selector widget in page toolbar (id: `reader-mode-selector`)
- Inline mode selectors (class: `.reader-mode-selector-inline`)
- Alt-key info alert (id: `masked-links-key-toggle-info-alert`)

---

## See Also

- [reader-mode-initial-js](reader-mode-initial-js) - Bootstrap module for FOUC prevention
- [reader-mode-css](reader-mode-css) - CSS styles for reader mode visuals
- [initial-js](initial-js) - Core utilities (doWhenBodyExists, notification center)
- [extracts-js](extracts-js) - Popup/popin system that coordinates with link masking
- [sidenotes-js](sidenotes-js) - Re-layouts after reader mode deactivation
- [dark-mode-js](dark-mode-js) - Similar tri-state mode selector pattern
