
# special-occasions.js

**Path:** `js/special-occasions.js` | **Language:** JavaScript | **Lines:** ~692

> Date-driven holiday theming system for Halloween, Christmas, Easter, and April Fools

---

## Overview

This module applies seasonal visual themes to gwern.net based on the current date and time. It supports four holidays: Halloween, Christmas, Easter, and April Fools' Day. Each occasion has its own date-detection logic, CSS class additions, logo replacements, and optional element colorization.

The system activates automatically when the page loads by checking each occasion's test function. If active, it applies body classes like `special-halloween-dark` or `special-christmas-light` that trigger corresponding CSS rules. The module also handles logo replacement with themed SVG or bitmap alternatives, supporting randomized or sequenced logo selection across page loads via localStorage.

A key design decision is that special occasions are mode-aware: Halloween and Christmas have separate light/dark variants, and the theme re-applies whenever `DarkMode.computedModeDidChange` fires. This ensures the correct variant is always shown even if the user toggles dark mode mid-session.

---

## Public API

### `colorizeElements(colorizationSpecs, container?)`

Colorizes inline SVG icons by transforming color values in CSS custom properties.

```js
colorizeElements([
  [ "li:nth-of-type(odd)", "--list-bullet", "#f00" ],
  [ "li:nth-of-type(even)", "--list-bullet", "#0f0" ]
]);
```

Each spec is `[selector, cssVariable, referenceColor, undesiredDarkModeProperty?]`. The function finds elements matching the selector, extracts the SVG from the CSS variable's `url()` value, applies color transforms, and sets the result as an inline style.

**Called by:** Christmas occasion handler
**Calls:** `colorizeElement()`, `Color.processColorValue()`

---

### `uncolorizeElements(uncolorizationSpecs, container?)`

Reverses `colorizeElements()` by removing inline style overrides.

**Called by:** Christmas cancellation handler
**Calls:** `uncolorizeElement()`

---

### `injectSpecialPageLogo(logoType, options)`

Replaces the sidebar logo with a themed variant. Supports multiple selection modes:

| Option | Description |
|--------|-------------|
| `mode` | `"light"`, `"dark"`, or `null` for single-mode logos |
| `identifier` | Specific numbered logo (e.g., `"3"`) |
| `randomize` | Boolean; pick randomly from available logos |
| `sequence` | `"nextAfterSaved"`, `"previousBeforeSaved"`, `"nextAfterCurrent"`, `"previousBeforeCurrent"` |
| `link` | URL to point the logo link to (default: unchanged) |

Logo files must follow the naming convention:
- SVG: `/static/img/logo/{type}/logo-{type}.svg` or `/static/img/logo/{type}/{mode}/logo-{type}-{mode}-{n}.svg`
- Bitmap: `logo-{type}-small-{scale}x.{png|jpg|webp}` where scale is 1-3

**Called by:** Halloween handler, Christmas handler
**Calls:** `replacePageLogoWhenPossible()`, `getAssetPathname()`, `versionedAssetURL()`, `processAssetSequenceOptions()`

---

### `resetPageLogo()`

Restores the default logo (`/static/img/logo/logo-smooth.svg`) and resets the logo link to `/index`.

**Called by:** Occasion cancellation handlers
**Calls:** `replacePageLogoWhenPossible()`, `versionedAssetURL()`

---

### `resetPageLogoSequenceIndex(logoType)`

Clears the localStorage key tracking logo sequence position for a given type.

**Called by:** Halloween/Christmas cancellation handlers

---

### `toggleSpecialOccasionTest(specialOccasionName?, enable?)`

Debug utility to force-enable a special occasion regardless of date.

```js
toggleSpecialOccasionTest("halloween", true);  // Enable Halloween mode
toggleSpecialOccasionTest("halloween", false); // Disable
toggleSpecialOccasionTest();                   // Disable all
```

Uses localStorage keys like `special-occasion-test-halloween`.

---

### Date Detection Functions

| Function | Active Period |
|----------|---------------|
| `isItHalloween()` | Oct 31 6PM – Nov 1 6AM (Halloween-celebrating locales only) |
| `isItChristmas()` | Dec 24 6PM – end of Dec 25 |
| `isItAprilFools()` | Apr 1 8AM – 3PM |
| `isItEaster()` | Hardcoded dates 2024–2050 (all day) |

Each also returns `true` if:
- The body has class `test-{occasion}` (test pages like `/lorem-halloween`)
- localStorage has `special-occasion-test-{occasion}` set to `"true"`

---

## Internal Architecture

### Occasion Registry

```js
GW.specialOccasions = [
  [ name, testFn, applyFn, removeFn ],
  ...
];
```

Each entry defines:
1. **name** - Identifier string (`"halloween"`, `"christmas"`, etc.)
2. **testFn** - Returns boolean; checks if occasion is active
3. **applyFn** - Called when active; adds classes, replaces logo, colorizes elements
4. **removeFn** - Called when inactive; cleans up all modifications

### Control Flow

```
doWhenBodyExists()
    └── applySpecialOccasionClasses()
            ├── For each occasion in GW.specialOccasions:
            │       if testFn() → applyFn()
            │       else        → removeFn()
            └── Subscribe to DarkMode.computedModeDidChange
                    └── Re-run applySpecialOccasionClasses()
```

### Logo Replacement Pipeline

```
injectSpecialPageLogo()
    └── replacePageLogoWhenPossible()
            ├── Wait for #sidebar .logo-image and helper functions
            ├── Build logo pathname pattern with mode/identifier/sequence
            ├── Call getAssetPathname() to resolve actual file
            ├── Call versionedAssetURL() for cache-busting
            ├── Replace <svg> or inject <img> wrapper
            ├── Update logo link href if specified
            └── brightenLogoTemporarily(20s, 1s fade)
```

---

## Key Patterns

### Locale-Aware Halloween

Halloween only activates for users whose browser language suggests they're from a Halloween-celebrating region:

```js
const halloweenLangs = new Set(['en','ga','gd','de','nl','fr','es','ja','ko']);
let langCode = (window.navigator.userLanguage ?? window.navigator.language).slice(0, 2).toLowerCase();
```

This prevents Halloween styling from appearing to users in regions where Halloween isn't celebrated.

### Inline SVG Color Transformation

The colorization system works by:
1. Extracting SVG source from a CSS `url('data:image/svg+xml;utf8,...')` value
2. Parsing hex color codes with regex: `/(?<!href=)"(#[0-9A-Fa-f]+)"/g`
3. Applying `Color.ColorTransform.COLORIZE` to shift hues
4. Re-encoding the modified SVG as a data URL
5. Setting it as an inline style

This allows list bullets and horizontal rule icons to be recolored to red/green for Christmas without modifying source SVGs.

### Viewport-Responsive Logo Selection

Halloween uses `doWhenMatchMedia()` to show different logos at different viewport widths:

```js
doWhenMatchMedia(matchMedia("(min-width: 1180px)"), {
    ifMatchesOrAlwaysDo: () => { /* sequenced logo for wide screens */ },
    otherwiseDo: () => { /* fixed logo-1 for narrow screens */ },
    whenCanceledDo: () => { resetPageLogo(); }
});
```

---

## Configuration

### Global Constants

| Constant | Value | Purpose |
|----------|-------|---------|
| `GW.specialOccasionTestLocalStorageKeyPrefix` | `"special-occasion-test-"` | localStorage key prefix for debug mode |
| `GW.specialOccasionTestPageNamePrefix` | `"test-"` | Body class prefix for test pages |
| `GW.allowedAssetSequencingModes` | Array of 4 strings | Valid values for `sequence` option |

### Test Pages

Each occasion has a test page that forces activation regardless of date:
- `/lorem-halloween`
- `/lorem-christmas`
- `/lorem-april-fools`
- `/lorem-easter`

---

## Integration Points

### Events Listened

| Event | Source | Purpose |
|-------|--------|---------|
| `DarkMode.computedModeDidChange` | dark-mode.js | Re-apply occasion classes for new mode |

### External Dependencies

| Function | Source | Purpose |
|----------|--------|---------|
| `Color.processColorValue()` | color.js | Color transformation for SVG colorization |
| `DarkMode.computedMode()` | dark-mode.js | Determine light/dark variant |
| `DarkMode.defaultMode` | dark-mode.js | Set default to dark during Halloween |
| `getAssetPathname()` | misc.js | Resolve logo file from pattern |
| `versionedAssetURL()` | misc.js | Add cache-busting version query |
| `processAssetSequenceOptions()` | misc.js | Handle randomize/sequence logic |
| `doWhenBodyExists()` | initial.js | Lifecycle hook |
| `doWhenPageLoaded()` | initial.js | Lifecycle hook |
| `doWhenMatchMedia()` | initial.js | Media query listener |
| `doWhenElementExists()` | initial.js | Element availability hook |
| `Extracts.addTargetsWithin()` | extracts.js | April Fools popup target |
| `Popups.spawnPopup()` | popups.js | April Fools popup |

### Body Classes Added

| Class | Occasion | Condition |
|-------|----------|-----------|
| `special-halloween-light` | Halloween | Light mode active |
| `special-halloween-dark` | Halloween | Dark mode active |
| `special-christmas-light` | Christmas | Light mode active |
| `special-christmas-dark` | Christmas | Dark mode active |
| `special-april-fools` | April Fools | Always (single variant) |
| `special-easter` | Easter | Always (currently no styling) |

### localStorage Keys

| Key Pattern | Purpose |
|-------------|---------|
| `special-occasion-test-{name}` | Debug mode toggle |
| `logo-sequence-index-{type}` | Track logo sequence position |

---

## See Also

- [special-occasions-css](special-occasions-css) - CSS styles for holiday themes
- [dark-mode-js](dark-mode-js) - Theme system that triggers occasion re-application
- [color-js](color-js) - Color transformations for SVG colorization
- [colors-css](colors-css) - Base colors overridden by holiday themes
- [initial-js](initial-js) - Provides lifecycle hooks used throughout
- [popups-js](popups-js) - Used by April Fools joke popup
