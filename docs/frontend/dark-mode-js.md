
# dark-mode.js / dark-mode-initial.js

**Path:** `js/dark-mode.js` | **Language:** JavaScript | **Lines:** ~280

> Two-file JavaScript system for theme switching with FOUC prevention, CSS variable theming, and intelligent image inversion

---

## Overview

The dark mode system provides theme switching between light, dark, and auto (system-follows) modes. It's split into two files for a critical reason: **FOUC prevention** (Flash Of Unstyled Content). The initial file loads synchronously in `<head>` before any content renders, reading localStorage and immediately applying the correct theme. The main file loads later with the rest of the JavaScript and handles the UI widget.

The system works by manipulating the `media` attribute of `<style>` blocks containing dark mode CSS variables. When set to `"all"`, dark mode styles apply; when set to `"not all"`, they're ignored; when set to `"all and (prefers-color-scheme: dark)"`, they follow the system preference.

A notable design choice: the CSS always includes both light and dark variable definitions. Rather than toggling classes on `<body>`, the system controls which `<style>` block is active via media queries. This allows dark mode to work even with JavaScript disabled—the CSS `@media (prefers-color-scheme: dark)` query provides a fallback.

---

## Public API

### `DarkMode.currentMode() → "auto" | "light" | "dark"`

Returns the currently selected mode (not the computed/displayed mode).

**Called by:** `DarkMode.setMode`, `DarkMode.saveMode`, `DarkMode.modeSelectorHTML`, `DarkMode.updateModeSelectorState`
**Calls:** `document.querySelector` (reads `media` attribute)

---

### `DarkMode.computedMode(modeSetting?, systemDarkModeActive?) → "light" | "dark"`

Returns the actual displayed mode. When `currentMode()` is "auto", this checks the system preference.

```javascript
// Returns "dark" if:
//   modeSetting == "dark", OR
//   modeSetting == "auto" AND system dark mode is active
// Otherwise returns "light"
```

**Called by:** `DarkMode.modeSelectorHTML`, event handlers
**Calls:** `GW.mediaQueries.systemDarkModeActive.matches`

---

### `DarkMode.setMode(selectedMode?) → void`

Sets the display mode. If no argument, uses `DarkMode.defaultMode` (normally "auto").

1. Saves to localStorage
2. Updates `media` attribute on dark mode style elements
3. Fires `DarkMode.didSetMode` event

**Called by:** Initial load (twice: once immediately, once after body exists), button clicks, scroll observers
**Calls:** `DarkMode.saveMode`, `GW.notificationCenter.fireEvent`

---

### `DarkMode.saveMode(newMode?) → void`

Persists mode to localStorage. Key: `"dark-mode-setting"`. Auto mode removes the key entirely (localStorage absence = auto).

---

### `DarkMode.setup() → void`

Called on load. Injects the mode selector widget into the page toolbar and sets up inline selectors.

---

## Internal Architecture

### Two-Phase Loading

```
1. HEAD (dark-mode-initial.js, synchronous)
   ├── Read localStorage
   ├── Set media attributes immediately (prevents FOUC)
   ├── Define core DarkMode object
   └── Set up mode change event handlers

2. BODY LOADED (dark-mode.js, deferred)
   ├── Extend DarkMode object with UI
   ├── Inject toolbar widget
   └── Set up scroll-triggered dark mode activation
```

### Media Attribute Switching

The core mechanism manipulates these elements (defined in `switchedElementsSelector`):

- `#inlined-styles-colors-dark` — The dark mode CSS variables
- `#favicon-dark` — Dark mode favicon
- `#favicon-apple-touch-dark` — Dark mode Apple touch icon

Each gets its `media` attribute set to one of:

| Mode | Media Attribute Value |
|------|----------------------|
| auto | `"all and (prefers-color-scheme: dark)"` |
| dark | `"all"` |
| light | `"not all"` |

### Mode Selector Widget

The widget shows three buttons (Auto/Light/Dark) with icons. The currently selected mode's button is disabled. In "auto" mode, an additional "active" indicator shows which of light/dark is currently displayed based on system preference.

### Scroll-Triggered Dark Mode

Certain pages can force dark mode when scrolled to a specific element:

```javascript
activateTriggerElementsSelector: ".dark-mode-enable-when-here"
```

When an element with this class scrolls into view (using IntersectionObserver at 100% threshold), dark mode is activated.

---

## Key Patterns

### FOUC Prevention via Immediate Execution

`dark-mode-initial.js` runs synchronously in `<head>`:

```javascript
// Runs immediately on script load, before any rendering
DarkMode.setMode();

// Runs again once <body> exists (for body class detection)
doWhenBodyExists(() => {
    if (document.body.classList.contains("dark-mode"))
        DarkMode.defaultMode = "dark";
    DarkMode.setMode();
});
```

### Auto Mode with System Preference Tracking

The system listens for OS theme changes:

```javascript
doWhenMatchMedia(GW.mediaQueries.systemDarkModeActive, {
    name: "DarkMode.fireComputedModeDidChangeEventForSystemDarkModeChange",
    ifMatchesOrAlwaysDo: (mediaQuery) => {
        // Fire event only if computed mode actually changed
        let previousComputedMode = DarkMode.computedMode(DarkMode.currentMode(), !(mediaQuery.matches));
        if (previousComputedMode != DarkMode.computedMode())
            GW.notificationCenter.fireEvent("DarkMode.computedModeDidChange");
    }
});
```

### Image Inversion Classes

Images are classified for dark mode handling via server-side analysis:

| Class | Behavior in Dark Mode |
|-------|----------------------|
| `.invert` | Inverted (manually marked) |
| `.invert-auto` | Inverted (auto-detected by ImageMagick color counting) |
| `.invert-not` | No filtering (manually marked, e.g., artwork) |
| `.invert-not-auto` | No filtering (auto-detected as non-invertible) |
| (none) | 50% grayscale only |

The CSS applies:
```css
figure img.invert,
figure img.invert-auto {
    filter: grayscale(50%) invert(100%) brightness(95%) hue-rotate(180deg);
}
figure img:not(.invert):not(.invert-auto) {
    filter: grayscale(50%);
}
```

All filters are removed on hover to show original image. SVG images with `.invert` classes maintain their inversion on hover (they look wrong uninverted).

### `.dark-mode-invert` Utility Class

Any element can opt into dark mode inversion by adding this class. The actual filter is controlled by the `--dark-mode-invert-filter` CSS variable, allowing per-element customization:

```css
audio {
    --dark-mode-invert-filter: invert(1) brightness(1.5) contrast(1.5);
}
```

Used by: list bullet points, inline icons, horizontal rules, audio elements, loading spinners, and more.

---

## Configuration

### Mode Options Array

```javascript
modeOptions: [
    // [name, shortLabel, unselectedLabel, selectedLabel, description, iconName]
    [ "auto", "Auto", "Auto Light/Dark", "Auto Light/Dark", "Set light or dark...", "adjust-solid" ],
    [ "light", "Light", "Light Mode", "Light Mode", "Light mode at all times...", "sun-solid" ],
    [ "dark", "Dark", "Dark Mode", "Dark Mode", "Dark mode at all times...", "moon-solid" ]
]
```

### localStorage Key

- **Key:** `"dark-mode-setting"`
- **Values:** `"light"` or `"dark"` (absence = auto)

### CSS Variables

All theme colors are CSS custom properties on `:root`, prefixed with `--GW-`. The dark mode stylesheet redefines ~100+ variables. Key ones:

- `--GW-body-background-color`: `#000` (pure black)
- `--GW-body-text-color`: `#fff` (pure white)

These are the primary values in the generated CSS. Some alternative color schemes may use softer values like `#161616`/`#f1f1f1` to reduce OLED scrolling jank, but the default dark mode uses pure black and white.

---

## Integration Points

### Events Fired

| Event | When | Payload |
|-------|------|---------|
| `DarkMode.didLoad` | After `dark-mode.js` finishes loading | none |
| `DarkMode.didSetMode` | After mode is set | `{ previousMode }` |
| `DarkMode.computedModeDidChange` | When displayed mode changes (including system preference changes in auto mode) | none |

### Events Listened

| Event | Handler |
|-------|---------|
| `DarkMode.didSetMode` | Updates mode selector widget state |

### Media Queries Used

- `GW.mediaQueries.systemDarkModeActive` — `matchMedia("(prefers-color-scheme: dark)")`

### Shared State

- `GW.pageToolbar` — Used to inject the mode selector widget
- `GW.notificationCenter` — Event system
- `localStorage["dark-mode-setting"]` — Persisted preference

### Body Class Detection

If `<body>` has class `dark-mode`, `DarkMode.defaultMode` is set to `"dark"`. This allows pages to force dark mode by default.

---

## See Also

- [dark-mode-initial-js](dark-mode-initial-js) - Early-loading bootstrap that prevents FOUC
- [dark-mode-adjustments-css](dark-mode-adjustments-css) - Image filters and color adjustments for dark mode
- [colors-css](colors-css) - CSS custom properties this module controls
- [initial-js](initial-js) - Defines `GW.mediaQueries.systemDarkModeActive` and `doWhenMatchMedia()`
- [reader-mode-js](reader-mode-js) - Similar tri-state mode selector pattern
- [special-occasions-js](special-occasions-js) - Listens to dark mode changes for holiday theming
