---
sidebar_position: 5
---

# dark-mode-adjustments.css

**Path:** `css/dark-mode-adjustments.css` | **Language:** CSS | **Lines:** ~162

Dark mode color and filter overrides for gwern.net's dark theme.

---

## Overview

This stylesheet contains all the color and visual adjustments necessary to transform gwern.net from its default light theme to a dark theme. It's applied when the user toggles dark mode via the theme switcher in the page toolbar.

Rather than duplicating all styles, this file strategically overrides CSS custom properties defined in `colors.css` and applies color inversion filters to images and certain UI elements. The dark mode uses a near-black background (`#161616`) instead of pure black to prevent pixel response time issues on OLED screens and reduce eye strain from excessive contrast.

The file is relatively compact (~160 lines) because it leverages the existing color variable system—most components automatically adapt when their color variables are changed. The file also handles special cases like image filtering (with progressive enhancement on hover) and icon inversions.

## Key Selectors/Variables

### Core Colors (`:root` overrides)
- `--GW-body-background-color: #161616`: Near-black background (not pure #000 to avoid OLED jank)
- `--GW-body-text-color: #f1f1f1`: Near-white text (not pure #fff to reduce contrast harshness)

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

- **Non-invertible images** (no class or `.invert-not`):
  - `filter: grayscale(50%)` - Desaturated but not inverted
  - Hover removes filter

- **`.invert-not` / `.invert-not-auto` images**:
  - `filter: none` - No filtering (for artwork, diagrams with color-coded legends)

- **Image alt-text**: Inverted separately to maintain readability

- **Transition behavior**: 0.25s ease filter transition, with 0.25s delay on hover restoration

### Special Elements

- **Admonition icons**: Selective inversion (tip inverted, note/warning/error not inverted)
- **Table sorting icons**: Uses dark-mode-specific arrow sprites
- **Loading spinners**: Inverted and made more visible (40% opacity)
- **Recently-modified icons**: Inverted
- **Manicule (pointing hand) icons**: Inverted

## Loading

This stylesheet is loaded dynamically by `dark-mode.js` when the user enables dark mode. It's injected as a `<link>` element in the document `<head>` and removed when switching back to light mode.

The loading is instant (no fade-in) to avoid jarring visual transitions. Dark mode preference is persisted to localStorage so returning visitors see their preferred theme immediately.

---

## See Also

- [colors](/css/colors) - Base color variable definitions that this file overrides
- [light-mode-adjustments](/css/light-mode-adjustments) - Light mode pattern/texture URLs
- [dark-mode-js](/frontend/dark-mode-js) - JavaScript that toggles dark mode and loads/unloads this stylesheet
- [build-mode-css](/php/build-mode-css) - PHP script that generates mode CSS files
- [color-scheme-convert](/php/color-scheme-convert) - Oklch color transformation for dark mode generation
