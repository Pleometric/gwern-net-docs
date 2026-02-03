
# dark-mode.js

**Path:** `js/dark-mode.js` | **Language:** JavaScript | **Lines:** ~291

> UI and advanced features for theme switching: mode selector widget, scroll-triggered activation, and inline selectors

---

## Overview

This file extends the `DarkMode` object (initialized by `dark-mode-initial.js`) with the full user interface and advanced features. It loads after the initial script and provides:

1. **Mode selector widget** — A three-button toggle (Auto/Light/Dark) injected into the page toolbar
2. **Inline mode selectors** — Smaller selectors that can be embedded within content
3. **Scroll-triggered mode activation** — Dark mode can be forced when scrolling to specific elements
4. **Light mode re-activation** — Light mode can be re-enabled when scrolling past dark mode trigger zones

The file is split from `dark-mode-initial.js` to keep the critical path (FOUC prevention) minimal. This file can load deferred without affecting initial render.

---

## Public API

### `DarkMode.setup()`

Main initialization function. Called automatically on script load.

1. Injects the primary mode selector widget into page toolbar
2. Spawns IntersectionObservers for scroll-triggered mode changes
3. Sets up inline mode selectors via content rewrite processor

**Called by:** Script load (automatic)
**Calls:** `injectModeSelector`, `spawnObservers`, `processMainContentAndAddRewriteProcessor`

---

### `DarkMode.modeSelectorHTML(inline = false) → string`

Generates the HTML for a mode selector widget.

**Parameters:**
- `inline` — If `true`, generates compact inline version with short labels

**Returns:** HTML string with three buttons (Auto/Light/Dark)

**Called by:** `injectModeSelector`

---

### `DarkMode.injectModeSelector(replacedElement = null)`

Injects a mode selector into the DOM.

**Parameters:**
- `replacedElement` — If provided, replaces this element with an inline selector. If `null`, adds to page toolbar.

**Called by:** `setup`, rewrite processor for inline selectors

---

### `DarkMode.activateModeSelector(modeSelector)`

Wires up event handlers for a mode selector widget.

1. Adds click handlers to buttons
2. Registers event handler for `DarkMode.didSetMode` to update state
3. Sets up media query listener for system dark mode changes

**Called by:** `injectModeSelector`, rewrite processor

---

### `DarkMode.updateModeSelectorState(modeSelector?)`

Updates the visual state of a mode selector to reflect current mode.

1. Clears all button states
2. Marks correct button as selected/disabled
3. Sets accesskey on next button
4. In auto mode, marks the currently active (light/dark) button

**Called by:** `DarkMode.didSetMode` event handler, system dark mode media query

---

### `DarkMode.spawnObservers(container = document.body)`

Creates IntersectionObservers for scroll-triggered mode changes within a container.

**Behavior:**
- Elements matching `.dark-mode-enable-when-here` trigger dark mode when 100% visible
- Elements matching `.light-mode-re-enable-when-here` re-enable light mode (only if user's saved mode would show light)

**Called by:** `setup`, rewrite processor for dynamically loaded content

---

### `DarkMode.modeSelectButtonClicked(event)`

Click handler for mode selector buttons.

Uses `doIfAllowed()` pattern to prevent rapid clicks during mode transition. For accesskey presses (no pointer), expands the toolbar before changing mode.

**Called by:** Button activate event

---

## Internal Architecture

### Mode Options Configuration

```javascript
modeOptions: [
    // [name, shortLabel, unselectedLabel, selectedLabel, description, iconName]
    [ "auto", "Auto", "Auto Light/Dark", "Auto Light/Dark",
      "Set light or dark mode automatically, according to system-wide setting...",
      "adjust-solid" ],
    [ "light", "Light", "Light Mode", "Light Mode",
      "Light mode at all times (black-on-white)",
      "sun-solid" ],
    [ "dark", "Dark", "Dark Mode", "Dark Mode",
      "Dark mode at all times (inverted: white-on-black)",
      "moon-solid" ]
]
```

### Widget HTML Structure

```html
<div id="dark-mode-selector" class="dark-mode-selector mode-selector">
    <button type="button" class="select-mode-auto selected" disabled>
        <span class="icon">[SVG]</span>
        <span class="label">Auto Light/Dark</span>
    </button>
    <button type="button" class="select-mode-light selectable active">
        <span class="icon">[SVG]</span>
        <span class="label">Light Mode</span>
    </button>
    <button type="button" class="select-mode-dark selectable">
        <span class="icon">[SVG]</span>
        <span class="label">Dark Mode</span>
    </button>
</div>
```

**Classes:**
- `selected` — Currently chosen mode
- `selectable` — Available for selection
- `active` — In auto mode, indicates which of light/dark is currently displayed

### Inline Mode Selectors

Inline selectors are placed within content using:

```html
<span class="dark-mode-selector-inline"></span>
```

The rewrite processor replaces these with compact mode selectors using short labels.

### Scroll-Triggered Mode Activation

```javascript
// Dark mode triggers
enableDarkModeTriggerElementsSelector: ".dark-mode-enable-when-here"

// Light mode re-enable triggers
reEnableLightModeTriggerElementsSelector: ".light-mode-re-enable-when-here"
```

Uses `lazyLoadObserver()` with `threshold: 1.0` (element must be 100% visible).

The light mode re-enable only fires if `DarkMode.computedMode(DarkMode.savedMode())` would be "light" — i.e., the user hasn't explicitly chosen dark mode.

---

## Key Patterns

### Rate-Limited Mode Switching

```javascript
doIfAllowed(() => {
    DarkMode.setMode(selectedMode, true);
}, DarkMode, "modeSelectorInteractable");
```

The `doIfAllowed()` pattern prevents rapid clicking from causing visual glitches during CSS transitions.

### Accesskey Toolbar Expansion

```javascript
if (event.pointerId == -1) {
    button.blur();
    GW.pageToolbar.expandToolbarFlashWidgetDoThing("dark-mode-selector", () => {
        DarkMode.setMode(selectedMode, true);
    });
} else {
    DarkMode.setMode(selectedMode, true);
}
```

When the mode is changed via keyboard (accesskey), the toolbar briefly expands to show the user what changed.

### Dual-Phase Observer Spawning

```javascript
processMainContentAndAddRewriteProcessor("DarkMode.spawnObserversForTriggerElementsInLoadedContent", (container) => {
    DarkMode.spawnObservers(container);
});
```

Observers are spawned both on initial content and on dynamically loaded content (transclusions, popups).

---

## Configuration

### Trigger Element Selectors

| Selector | Purpose |
|----------|---------|
| `.dark-mode-enable-when-here` | Element triggers dark mode when scrolled into view |
| `.light-mode-re-enable-when-here` | Element re-enables light mode when scrolled into view |

### Mode Selector State

| Property | Purpose |
|----------|---------|
| `modeSelector` | Reference to main toolbar widget element |
| `modeSelectorInteractable` | Boolean flag for rate limiting |

### Selected Mode Note

```javascript
selectedModeOptionNote: " [This option is currently selected.]"
```

Appended to the tooltip of the currently selected mode button.

---

## Integration Points

### Events Fired

| Event | When | Payload |
|-------|------|---------|
| `DarkMode.didLoad` | After script finishes loading | none |

### Events Listened

| Event | Handler |
|-------|---------|
| `DarkMode.didSetMode` | Updates mode selector state |

### Dependencies

**From `dark-mode-initial.js`:**
- `DarkMode.currentMode()`
- `DarkMode.computedMode()`
- `DarkMode.setMode()`
- `DarkMode.savedMode()`

**From `initial.js`:**
- `GW.notificationCenter`
- `GW.pageToolbar`
- `GW.svg()`
- `GW.mediaQueries.systemDarkModeActive`
- `doWhenMatchMedia()`
- `doIfAllowed()`
- `lazyLoadObserver()`
- `processMainContentAndAddRewriteProcessor()`

**From `utility.js`:**
- `elementFromHTML()`

---

## See Also

- [dark-mode-initial-js](/frontend/dark-mode-initial-js) - Core dark mode logic and FOUC prevention
- [initial-js](/frontend/initial-js) - Core utilities and notification center
- [reader-mode-js](/frontend/reader-mode-js) - Similar tri-state mode selector pattern
- [colors-css](/frontend/colors-css) - CSS custom properties controlled by dark mode
- [dark-mode-adjustments-css](/frontend/dark-mode-adjustments-css) - Image filters for dark mode
