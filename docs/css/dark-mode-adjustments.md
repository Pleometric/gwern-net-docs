---
sidebar_position: 5
---

# dark-mode-adjustments.css

**Path:** `css/dark-mode-adjustments.css` | **Language:** CSS | **Lines:** ~162

Dark mode color and filter overrides for gwern.net's dark theme.

---

## Overview

This stylesheet contains all the color and visual adjustments necessary to transform gwern.net from its default light theme to a dark theme. It's applied when the user toggles dark mode via the theme switcher in the page toolbar.

Rather than duplicating all styles, this file strategically overrides CSS custom properties defined in `colors.css` and applies color inversion filters to images and certain UI elements. The dark mode overrides set `--GW-body-background-color: #161616` and `--GW-body-text-color: #f1f1f1`.

The file is relatively compact (~160 lines) because it leverages the existing color variable system—most components automatically adapt when their color variables are changed. The file also handles special cases like image filtering (with progressive enhancement on hover) and icon inversions.

## Key Selectors/Variables

### Core Colors (`:root` overrides)
- `--GW-body-background-color: #161616`
- `--GW-body-text-color: #f1f1f1`

### Pattern/Texture Variables
- `--GW-popups-popup-title-bar-pattern`: Dotted pattern for popup title bars (dark variant)
- `--GW-popins-popin-backdrop-color`: Darker backdrop (60% black instead of 40%)
- `--GW-checkerboard-scrollbar-background-image`: Inverted scrollbar checkerboard pattern

### Filter Classes
- `.dark-mode-invert`, `.dark-mode-invert::before`, `.dark-mode-invert::after`:
  - Apply `filter: var(--dark-mode-invert-filter, none)` to invert specified elements
  - Used for icons, list markers, and UI elements that need inversion

### Image Handling

The sophisticated image filtering system in dark mode:

- **`.invert` / `.invert-auto` images**:
  - `filter: grayscale(50%) invert(100%) brightness(95%) hue-rotate(180deg)`
  - Converts photos to dark-friendly versions while preserving recognizability
  - Hover removes filter to show original

- **Non-invertible images** (no class):
  - `filter: grayscale(50%)` - Desaturated but not inverted
  - Hover removes filter

- **`.invert-not` / `.invert-not-auto` images**:
  - `filter: none` - No filtering (for artwork, diagrams with color-coded legends)

- **Image alt-text**: Inverted separately to maintain readability

- **Transition behavior**: A filter transition is defined, but later rules set `transition: none` for `figure img` and variants, disabling transitions for those images

### Special Elements

- **Admonition icons**: Selective inversion (tip inverted, note/warning/error not inverted)
- **Table sorting icons**: Uses dark-mode-specific arrow sprites
- **Loading spinners**: Inverted and made more visible (40% opacity)
- **Recently-modified icons**: Inverted
- **Manicule (pointing hand) icons**: Inverted

## Loading

This stylesheet is concatenated into `dark-mode-GENERATED.css` at build time and inlined in the head include. JavaScript toggles the `media` attribute on the inlined dark-mode style block to force dark, force light, or follow system preference.

---

## See Also

- [colors](/css/colors) - Base color variable definitions that this file overrides
- [light-mode-adjustments](/css/light-mode-adjustments) - Light mode pattern/texture URLs
- [dark-mode-js](/frontend/dark-mode-js) - JavaScript that toggles dark mode via media attributes
- [build-mode-css](/php/build-mode-css) - PHP script that generates mode CSS files
- [color-scheme-convert](/php/color-scheme-convert) - Oklch color transformation for dark mode generation
