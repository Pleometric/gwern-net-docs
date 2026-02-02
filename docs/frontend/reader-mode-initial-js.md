
# reader-mode-initial.js

**Path:** `js/reader-mode-initial.js` | **Language:** JavaScript | **Lines:** ~55

> Early reader mode bootstrap that prevents FOUC by activating before full JS loads

---

## Overview

`reader-mode-initial.js` is a small bootstrap module that handles early reader mode activation. Its primary purpose is **FOUC prevention** (Flash Of Unstyled Content)—ensuring that pages tagged for reader mode display correctly from the first paint, before the full `reader-mode.js` loads.

The module defines the core `ReaderMode` object with minimal functionality: checking localStorage for saved preferences, determining if reader mode should be enabled, and adding the necessary body classes. Once `<body>` exists, it immediately activates reader mode if appropriate. The full UI (mode selector widget, link masking interactions, auto-deactivation on scroll) is deferred to `reader-mode.js`.

This follows the same split-module pattern used by `dark-mode-initial.js`—a lightweight early script for visual correctness, with heavier UI/interaction code loaded later.

---

## Public API

### `ReaderMode.currentMode() -> string`

Returns the saved mode setting from localStorage, or the default mode ("auto").

**Called by:** `ReaderMode.enabled`, `reader-mode.js`
**Calls:** `localStorage.getItem`

---

### `ReaderMode.enabled() -> boolean`

Determines if reader mode should be active for the current page. Returns `true` if:
- Mode is "on", OR
- Mode is "auto" AND the page has the `reader-mode` body class

**Called by:** `doWhenBodyExists` callback, `reader-mode.js`
**Calls:** `ReaderMode.currentMode`

---

### `ReaderMode.activate()`

Adds `reader-mode-active` and `masked-links-hidden` classes to `<body>`, appends a title note, and fires `ReaderMode.didActivate` event.

**Called by:** `doWhenBodyExists` callback (if enabled)
**Calls:** `GWLog`, `GW.notificationCenter.fireEvent`

---

## Internal Architecture

### Object Structure

```javascript
ReaderMode = {
    active: false,                        // Flag (superseded by class check in reader-mode.js)
    readerModeTitleNote: " [reader mode]", // Appended to document.title
    defaultMode: "auto",                  // Overridable default
    currentMode: () => {...},             // Get saved/default mode
    enabled: () => {...},                 // Should activate?
    activate: () => {...}                 // Apply reader mode
};
```

### Bootstrap Flow

```
Page Load
    │
    ├─→ Script executes immediately (defines ReaderMode object)
    │
    └─→ doWhenBodyExists() callback registered
            │
            └─→ When <body> exists:
                    │
                    ├─→ Check if page has .reader-mode class (author tag)
                    ├─→ Check localStorage for user preference
                    └─→ If enabled: call ReaderMode.activate()
                            │
                            ├─→ Add body classes
                            ├─→ Update document title
                            └─→ Fire ReaderMode.didActivate event
```

---

## Key Patterns

### FOUC Prevention via Early Activation

The critical pattern here is activating **before** the full JavaScript loads:

```javascript
doWhenBodyExists(() => {
    if (ReaderMode.enabled() == true)
        ReaderMode.activate();
});
```

By using `doWhenBodyExists` (a mutation observer wrapper), the activation happens as soon as `<body>` is parsed—before DOMContentLoaded, before later scripts, and crucially before first paint in most browsers. This prevents the jarring flash of seeing unmasked links before reader mode kicks in.

### Hybrid Mode Logic

The `enabled()` function implements a hybrid author/user preference system:

```javascript
enabled: () => {
    let currentMode = ReaderMode.currentMode();
    return (   currentMode == "on"
            || (   currentMode == "auto"
                && document.body.classList.contains("reader-mode")))
}
```

- **"on"**: User wants reader mode everywhere → always enable
- **"off"**: User wants no reader mode → never enable
- **"auto"**: Defer to author via `.reader-mode` body class

### Event-Based Handoff

The initial module fires an event when `reader-mode.js` loads:

```javascript
GW.notificationCenter.addHandlerForEvent("ReaderMode.didLoad", (eventInfo) => {
    ReaderMode.setMode();
}, { once: true });
```

This ensures the full `setMode()` (from `reader-mode.js`) runs after everything is in place, re-syncing any state if needed.

---

## Configuration

| Property | Default | Description |
|----------|---------|-------------|
| `defaultMode` | `"auto"` | Mode when no localStorage value exists |
| `readerModeTitleNote` | `" [reader mode]"` | Text appended to document title |

**localStorage key:** `reader-mode-setting`
**Values:** `"on"`, `"off"`, or absent (defaults to auto)

---

## Integration Points

### Events Fired

| Event | When | Payload |
|-------|------|---------|
| `ReaderMode.didActivate` | After activate() completes | None |

### Events Listened

| Event | Handler |
|-------|---------|
| `ReaderMode.didLoad` | Calls `ReaderMode.setMode()` once |

### Body Classes Applied

- `reader-mode-active` — Reader mode is currently on
- `masked-links-hidden` — Links in prose should appear as plain text

### Dependencies

- `GW.notificationCenter` (from `initial.js`)
- `doWhenBodyExists` (from `initial.js`)
- `GWLog` (from `initial.js`)

---

## See Also

- [reader-mode-js](reader-mode-js) - Full reader mode UI and interactions (loads later)
- [reader-mode-css](reader-mode-css) - CSS styles for reader mode visuals
- [dark-mode-initial-js](dark-mode-initial-js) - Same split-module FOUC prevention pattern
- [initial-js](initial-js) - Provides `doWhenBodyExists` and notification center
- [initial-css](initial-css) - Core styles that interact with reader mode
