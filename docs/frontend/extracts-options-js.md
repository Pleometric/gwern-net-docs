
# extracts-options.js

**Path:** `js/extracts-options.js` | **Language:** JavaScript | **Lines:** ~275

> User preference UI and persistence for enabling/disabling the popup/popin system.

---

## Overview

extracts-options.js handles user preferences for gwern.net's popup/popin extract system. It provides the mode selector widget (the "On/Off" toggle in the page toolbar), persists user choices to localStorage, and orchestrates the enable/disable lifecycle by calling into the main extracts.js module.

The module extends the `Extracts` object (defined in extracts.js) with option-related functionality. It supports two modes—"on" (popups/popins enabled) and "off" (disabled)—and automatically adapts UI labels based on whether the system is running in popup mode (desktop) or popin mode (mobile). The mode selector can appear in the toolbar as a full widget or inline as a compact button pair.

A notable UX feature is the "disable from popup" flow: clicking the eye-slash button inside a popup dismisses all popups, expands the toolbar, flashes the mode selector widget, then directly calls `Extracts.disableExtractPopFrames()` after a delay. This teaches users where the setting lives while respecting their intent to disable.

---

## Public API

### `Extracts.setMode(selectedMode)`

Switches the extract system on or off based on `selectedMode`.

```javascript
setMode: (selectedMode) => {
    if (selectedMode == "on")
        Extracts.enableExtractPopFrames();
    else
        Extracts.disableExtractPopFrames();
}
```

**Called by:** `modeSelectButtonClicked`

### `Extracts.injectModeSelector(replacedElement?)`

Creates and injects the mode selector widget. If `replacedElement` is provided, replaces it inline; otherwise adds to the page toolbar.

**Called by:** `Extracts.setup()` in extracts.js

### `Extracts.activateModeSelector(modeSelector)`

Attaches click handlers and event listeners to a mode selector element. Registers for `Extracts.didSetMode` events to keep UI in sync.

**Called by:** `injectModeSelector`, `Extracts.setup()` in extracts.js

### `Extracts.extractPopFramesEnabled() → boolean`

Returns whether extract pop-frames are currently enabled.

```javascript
extractPopFramesEnabled: () => {
    return (localStorage.getItem(Extracts.extractPopFramesDisabledLocalStorageItemKey()) != "true");
}
```

**Called by:** `modeSelectorHTML`, `updateModeSelectorState`

### `Extracts.enableExtractPopFrames()`

Enables the extract system: clears the "disabled" localStorage flag, fires `Extracts.didSetMode`, calls `Extracts.setup()`, and processes existing targets.

**Called by:** `setMode`

### `Extracts.disableExtractPopFrames()`

Disables the extract system: sets the "disabled" localStorage flag, fires `Extracts.didSetMode`, and calls `Extracts.cleanup()`.

**Called by:** `setMode`, `disableExtractPopFramesPopFrameTitleBarButton`

### `Extracts.disableExtractPopFramesPopFrameTitleBarButton() → Element`

Creates a title bar button (eye-slash icon) that disables popups when clicked. Used in popup title bars to give users an in-context way to disable the system.

**Called by:** extracts.js (title bar construction)
**Calls:** `GW.pageToolbar.toggleCollapseState`, `GW.pageToolbar.flashWidget`, `disableExtractPopFrames`

---

## Internal Architecture

### Mode Options Definition

```javascript
modeOptions: [
    [ "on", "On", "Enable Pop-frames", "Pop-frames Enabled", "Enable link pop-frames.", "message-lines-solid" ],
    [ "off", "Off", "Disable Pop-frames", "Pop-frames Disabled", "Disable link pop-frames.", "message-slash-solid" ],
]
```

Each option is a tuple: `[name, shortLabel, unselectedLabel, selectedLabel, description, iconName]`. Labels adapt to popup vs popin mode via string replacement of "-frame" with the current suffix.

### State Flow

```
User clicks mode button
        ↓
modeSelectButtonClicked()
        ↓
doIfAllowed() check       → Debounces rapid clicks via modeSelectorInteractable
        ↓
setMode("on" | "off")
        ↓
enableExtractPopFrames()  → Removes localStorage flag
   or                        Fires Extracts.didSetMode
disableExtractPopFrames() → Sets localStorage flag
        ↓
Extracts.setup() or cleanup()
        ↓
GW.contentDidInject handler reprocesses targets (if enabling)
```

### localStorage Key Selection

The module dynamically selects the correct localStorage key based on current mode:

```javascript
extractPopFramesDisabledLocalStorageItemKey: () => {
    return (Extracts.popFrameProvider == Popups
            ? Extracts.popupsDisabledLocalStorageItemKey    // "extract-popups-disabled"
            : Extracts.popinsDisabledLocalStorageItemKey); // "extract-popins-disabled"
}
```

This means popup and popin preferences are stored separately—disabling popups on desktop doesn't affect popin behavior on mobile.

---

## Key Patterns

### Button State Management

`updateModeSelectorState()` does a full reset-and-rebuild of button states rather than incremental updates:

```javascript
// Clear ALL buttons
modeSelector.querySelectorAll("button").forEach(button => {
    button.classList.remove("active");
    button.swapClasses([ "selectable", "selected" ], 0);
    button.disabled = false;
    // ... reset title, label, accesskey
});

// Then set the correct one
modeSelector.querySelectorAll(`.select-mode-${currentMode}`).forEach(button => {
    button.swapClasses([ "selectable", "selected" ], 1);
    button.disabled = true;
    // ... update title, label
});
```

This pattern is defensive—it guarantees correct state regardless of previous state.

### Accesskey Cycling

The non-selected button gets accesskey "p", creating a toggle:

```javascript
let buttons = Array.from(modeSelector.querySelectorAll("button"));
buttons[(buttons.findIndex(button => button.classList.contains("selected")) + 1) % buttons.length].accessKey = "p";
```

Pressing Alt+P always toggles to the other mode.

### Animated Disable Flow

The "disable from popup" button orchestrates a complex animation sequence:

1. Add `.disabled` class to button (visual feedback)
2. Expand toolbar (`toggleCollapseState(false)`)
3. Wait `popFramesDisableDespawnDelay` (1000ms) while popups clean up
4. Flash the mode selector widget in toolbar
5. Wait `popFramesDisableAutoToggleDelay` (1000ms)
6. Actually disable extract pop-frames
7. Collapse toolbar after a delay

This teaches users where the setting lives while providing smooth visual feedback.

---

## Configuration

### Timing Constants

| Constant | Value | Purpose |
|----------|-------|---------|
| `popFramesDisableDespawnDelay` | 1000ms | Delay before toolbar animation starts |
| `popFramesDisableWidgetFlashStayDuration` | 3000ms | How long toolbar widget stays highlighted |
| `popFramesDisableAutoToggleDelay` | 1000ms | Delay before calling `disableExtractPopFrames()` |

### localStorage Keys

| Key | Module | Effect |
|-----|--------|--------|
| `extract-popups-disabled` | extracts.js | Disables popups when "true" |
| `extract-popins-disabled` | extracts.js | Disables popins when "true" |

Note: Keys are defined in extracts.js as `popupsDisabledLocalStorageItemKey` and `popinsDisabledLocalStorageItemKey`. This module references them dynamically.

---

## Integration Points

### Events Fired

| Event | When |
|-------|------|
| `Extracts.didSetMode` | After enable or disable completes |

### Events Listened

| Event | Handler |
|-------|---------|
| `Extracts.didSetMode` | `updateModeSelectorState` — syncs button UI |

### Dependencies

- **GW.pageToolbar** — Toolbar widget management (`addWidget`, `flashWidget`, `toggleCollapseState`, `expandToolbarFlashWidgetDoThing`)
- **GW.svg()** — Icon rendering
- **Extracts.popFrameProvider** — Determines if we're in popup or popin mode
- **Extracts.setup() / cleanup()** — Lifecycle management (from extracts.js)
- **doIfAllowed()** — Rate-limiting utility (likely from GW namespace)

### Shared State

- `Extracts.modeSelector` — Reference to the injected toolbar widget element
- `Extracts.modeSelectorInteractable` — Boolean flag for click debouncing

---

## See Also

- [extracts.js](/frontend/extracts-js) - Main extract system that defines setup/cleanup lifecycle
- [popups.js](/frontend/popups-js) - Desktop popup display system
- [popins.js](/frontend/popins-js) - Mobile popin display system
- [extracts-load.js](/frontend/extracts-load-js) - Bootstrap module that triggers setup
- [initial.js](/frontend/initial-js) - GW namespace and notification center
- [dark-mode.js](/frontend/dark-mode-js) - Similar options pattern for theme preferences
