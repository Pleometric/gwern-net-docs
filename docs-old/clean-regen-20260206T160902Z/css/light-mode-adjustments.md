---
sidebar_position: 6
---

# light-mode-adjustments.css

**Path:** `css/light-mode-adjustments.css` | **Language:** CSS | **Lines:** ~7

Light mode pattern and texture URLs for popup title bars and scrollbars.

---

## Overview

This is a short stylesheet that defines light-mode-specific visual patterns (popup title bars, checkerboard scrollbars) and includes a small `@media` rule for `.poem` and `.editorial`. It complements `dark-mode-adjustments.css`, ensuring that light mode has the correct texture references.

The file mostly sets CSS custom properties pointing to image assets (GIFs with 2x pixel density for crisp rendering), plus a small layout tweak under `@media`. These patterns add subtle visual texture to otherwise flat UI elements, enhancing depth perception without adding visual noise.

## Key Variables

### `:root` Custom Properties

- `--GW-popups-popup-title-bar-pattern`:
  - `var(--GW-image-pattern-dotted-e6e6e6-on-fff-2x-gif)`
  - Dotted pattern for popup window title bars (unfocused state)
  - Light dots on white background

- `--GW-popups-popup-title-bar-pattern-focused`:
  - `var(--GW-image-pattern-dotted-fff-on-e6e6e6-2x-gif)`
  - Inverted pattern for focused popup windows
  - White dots on light gray background

- `--GW-checkerboard-scrollbar-background-image`:
  - `var(--GW-image-checkerboard-777-fff-2x-gif)`
  - Checkerboard pattern for scrollbar tracks
  - Gray and white checkerboard (unfocused state)

- `--GW-checkerboard-scrollbar-hover-background-image`:
  - `var(--GW-image-checkerboard-000-fff-2x-gif)`
  - Darker checkerboard on hover
  - Black and white checkerboard

### Referenced Image Variables

The `--GW-image-*` variables referenced here are defined elsewhere (likely in generated CSS or inline data URIs) and point to actual GIF image data. The `-2x` suffix indicates these are 2x pixel density images for retina/HiDPI displays.

## Loading

This file is loaded as part of the default CSS bundle (either inlined or in the main stylesheet). It's always present in light mode, but its variables are overridden when dark mode is active.

The pattern URLs are swapped out in `dark-mode-adjustments.css` to provide dark-appropriate textures.

---

## See Also

- [dark-mode-adjustments](/css/dark-mode-adjustments) - Provides dark mode equivalents of these patterns
- [colors](/css/colors) - Defines the color variables that work alongside these patterns
- [default](/css/default) - Uses these pattern variables for title bars and scrollbars
- [build-mode-css](/php/build-mode-css) - Combines this with colors.css for final output
- [dark-mode-js](/frontend/dark-mode-js) - Toggles between light and dark modes
