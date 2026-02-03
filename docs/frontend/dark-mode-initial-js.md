
# dark-mode-initial.js

**Path:** `js/dark-mode-initial.js` | **Language:** JavaScript | **Lines:** ~139

> Early-loading dark mode bootstrap that prevents flash of unstyled content (FOUC)

---

## Overview

This file solves a critical UX problem: preventing "flash of white" when a user with dark mode preference loads the page. It must execute synchronously in `<head>` before any content renders, reading the saved preference from localStorage and immediately applying the correct theme.

The file is intentionally minimal—it contains only the code necessary for immediate theme application. The UI widget, scroll-triggered activation, and other features live in the companion `dark-mode.js` which loads later. This split architecture ensures the blocking portion of JavaScript is as small as possible.

A key design decision: the system works even with JavaScript disabled. The CSS uses `@media (prefers-color-scheme: dark)` as a fallback, so the JavaScript is only needed for manual override functionality (force light/force dark). Users who block JavaScript get automatic system-following behavior.

---

## Public API

### `DarkMode.currentMode() → "auto" | "light" | "dark"`

Returns the currently selected mode by reading the `media` attribute of switched elements.

**Called by:** `DarkMode.setMode`, event handlers
**Calls:** `document.querySelector`

---

### `DarkMode.savedMode() → "auto" | "light" | "dark"`

Returns the mode saved in localStorage. Returns `"auto"` if no preference is stored.

**Called by:** `DarkMode.setMode` (on initial load), `dark-mode.js` observers
**Calls:** `localStorage.getItem`

---

### `DarkMode.saveMode(newMode) → void`

Persists mode to localStorage. Auto mode removes the key entirely (localStorage absence = auto).

**Called by:** `DarkMode.setMode` (when `save = true`)
**Calls:** `localStorage.setItem` / `localStorage.removeItem`

---

### `DarkMode.setMode(selectedMode?, save = false) → void`

Sets the display mode.

1. Remembers previous mode
2. Returns early if mode unchanged
3. Updates `media` attribute on all switched elements
4. Optionally saves to localStorage
5. Fires `DarkMode.didSetMode` event

**Parameters:**
- `selectedMode` — Mode to set (`"auto"`, `"light"`, or `"dark"`). Returns if undefined.
- `save` — If `true`, persists to localStorage

**Called by:** Immediate execution, body exists callback, `dark-mode.js` button handlers
**Calls:** `DarkMode.saveMode`, `GW.notificationCenter.fireEvent`

---

### `DarkMode.computedMode(modeSetting?, systemDarkModeActive?) → "light" | "dark"`

Returns the actual displayed mode (not the setting). When mode is "auto", checks the system preference.

```javascript
// Returns "dark" if:
//   modeSetting == "dark", OR
//   modeSetting == "auto" AND systemDarkModeActive == true
// Otherwise returns "light"
```

**Parameters:**
- `modeSetting` — Mode setting to compute from (defaults to `currentMode()`)
- `systemDarkModeActive` — System dark mode state (defaults to current media query)

**Called by:** Event handlers, `dark-mode.js` widget
**Calls:** `GW.mediaQueries.systemDarkModeActive.matches`

---

## Internal Architecture

### Media Attribute Switching

The core mechanism controls theme by manipulating `media` attributes on `<style>` and `<link>` elements:

**Switched Elements (`switchedElementsSelector`):**
- `#inlined-styles-colors-dark` — Dark mode CSS variables
- `#favicon-dark` — Dark mode favicon
- `#favicon-apple-touch-dark` — Dark mode Apple touch icon

**Media Attribute Values:**

| Mode | Value | Effect |
|------|-------|--------|
| auto | `"all and (prefers-color-scheme: dark)"` | Follows system preference |
| dark | `"all"` | Always applies |
| light | `"not all"` | Never applies |

### Two-Phase Initialization

```
Phase 1: Script Load (synchronous, blocking)
├── DarkMode.setMode(DarkMode.savedMode()) — Apply saved preference immediately
└── Prevents FOUC

Phase 2: Body Exists (via doWhenBodyExists callback)
├── Check <body> class for "dark-mode"
├── If present, set pageDefaultMode to "dark"
├── If currentMode is "auto" and pageDefaultMode is set
└── DarkMode.setMode(pageDefaultMode) — Apply page default
```

### Event Wiring

Two event handlers set up change detection:

1. **Mode setting changes** — When `DarkMode.didSetMode` fires, compare previous and current computed modes. Fire `computedModeDidChange` if they differ.

2. **System preference changes** — Via `doWhenMatchMedia` on `GW.mediaQueries.systemDarkModeActive`. When OS theme toggles, fire `computedModeDidChange` if computed mode changed.

---

## Key Patterns

### FOUC Prevention via Immediate Execution

The file executes `setMode()` at the top level (not in an event handler):

```javascript
//  Activate saved mode.
DarkMode.setMode(DarkMode.savedMode());
```

This runs synchronously during HTML parsing, before render. The browser applies the correct `media` attributes before painting.

### Page Default Mode Override

Some pages force dark mode via markup:

```javascript
doWhenBodyExists(() => {
    if (document.body.classList.contains("dark-mode"))
        DarkMode.pageDefaultMode = "dark";

    // Page default mode takes effect if the user hasn't picked a mode.
    if (   DarkMode.currentMode() == "auto"
        && DarkMode.pageDefaultMode != null)
        DarkMode.setMode(DarkMode.pageDefaultMode);
});
```

This allows per-page dark mode forcing while still respecting explicit user overrides stored in localStorage. The `pageDefaultMode` property is separate from the saved user preference.

### Careful Change Detection

The event system only fires `computedModeDidChange` when the actual displayed mode changes, not just the setting:

```javascript
let previousComputedMode = DarkMode.computedMode(eventInfo.previousMode, GW.mediaQueries.systemDarkModeActive.matches);
if (previousComputedMode != DarkMode.computedMode())
    GW.notificationCenter.fireEvent("DarkMode.computedModeDidChange");
```

This prevents spurious events when switching between "auto" and the mode it would resolve to.

---

## Configuration

### `DarkMode.pageDefaultMode`

Default: `null`

Set to `"dark"` if `<body>` has class `dark-mode`. Used to force dark mode on specific pages when user hasn't explicitly chosen a mode.

### `DarkMode.switchedElementsSelector`

CSS selector matching elements whose `media` attribute controls theming:

```javascript
[
    "#inlined-styles-colors-dark",
    "#favicon-dark",
    "#favicon-apple-touch-dark"
].join(", ")
```

### `DarkMode.mediaAttributeValues`

Map of mode names to their `media` attribute values:

```javascript
{
    "auto":  "all and (prefers-color-scheme: dark)",
    "dark":  "all",
    "light": "not all"
}
```

### localStorage

- **Key:** `"dark-mode-setting"`
- **Values:** `"light"` | `"dark"` | (absent = auto)

---

## Integration Points

### Events Fired

| Event | When | Payload |
|-------|------|---------|
| `DarkMode.didSetMode` | After any mode change | `{ previousMode: string }` |
| `DarkMode.computedModeDidChange` | When displayed mode changes (including system preference changes) | none |

### Events Listened

| Event | Handler |
|-------|---------|
| `DarkMode.didSetMode` | Self-handler for computing mode change |

### Dependencies

- `GW.notificationCenter` — Event system (from `initial.js`)
- `GWLog()` — Logging function (from `initial.js`)
- `doWhenBodyExists()` — Callback scheduler (from `initial.js`)
- `doWhenMatchMedia()` — Media query listener (from `initial.js`)
- `GW.mediaQueries.systemDarkModeActive` — System theme preference query (from `initial.js`)

### Extended By

`dark-mode.js` extends the `DarkMode` object with:
- `setup()` — Injects UI widget
- `modeSelectorHTML()` — Generates widget markup
- `modeSelectButtonClicked()` — Button handler
- `injectModeSelector()` — Widget injection
- `activateModeSelector()` — Widget event binding
- `updateModeSelectorState()` — Widget state updates
- `spawnObservers()` — Scroll-triggered mode activation
- Mode options configuration array

---

## See Also

- [dark-mode-js](/frontend/dark-mode-js) - Main dark mode module with UI widget
- [dark-mode-adjustments-css](/frontend/dark-mode-adjustments-css) - CSS that dark mode activates
- [initial-js](/frontend/initial-js) - Defines `GW.notificationCenter`, `doWhenBodyExists`, `doWhenMatchMedia`
- [reader-mode-initial-js](/frontend/reader-mode-initial-js) - Similar early-loading pattern for reader mode
- [colors-css](/frontend/colors-css) - Color variable definitions toggled by this module
