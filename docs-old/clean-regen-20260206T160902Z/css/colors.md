---
sidebar_position: 1
---

# colors.css

**Path:** `css/colors.css` | **Language:** CSS | **Lines:** ~334

CSS custom property definitions for the complete gwern.net color palette, supporting both light and dark modes.

---

## Overview

This file serves as the central color system for gwern.net, defining CSS custom properties (variables) for every component and element across the site. It provides the light mode color values that are later overridden by dark mode adjustments.

The color system is comprehensive and semantic, with variables organized by component rather than by raw color values. This means colors are named for their purpose (e.g., `--GW-TOC-border-color`) rather than their appearance (e.g., `--color-gray-200`), making it easier to maintain thematic consistency and adapt the palette.

The file contains approximately 335 lines defining over 200 color variables covering everything from basic body text and links to complex UI elements like popups, sidenotes, code blocks, and custom admonitions. All colors use the `--GW-` prefix to namespace them from potential conflicts.

## Key Variable Groups

### General
- `--GW-body-background-color`: Main page background (white `#fff` in light mode)
- `--GW-body-text-color`: Main text color (black `#000` in light mode)
- `--GW-text-selection-background-color` / `--GW-text-selection-color`: Text selection styling

### Links
- `--GW-body-link-color`: Default link color (`#333`)
- `--GW-body-link-hover-color`: Hover state (`#888`)
- `--GW-body-link-visited-color`: Visited links (`#666`)
- `--GW-body-link-inverted-*`: Variants for dark-on-light contexts

### Layout Components
- **Blockquotes**: 4-level nested emphasis system with progressively darker backgrounds and borders
- **Table of Contents**: Border, background, button colors, number colors, hover states
- **Collapse blocks**: Disclosure buttons, hover states, special variants for blockquotes and inline collapses
- **Headings**: Border colors for H1/H2
- **Figures**: Outline colors for images and captions
- **Tables**: Comprehensive table styling including zebra striping, sorted columns, scrollbars

### Code & Syntax
- **Code blocks**: Background, border, scrollbar, line highlighting
- **Syntax highlighting**: 20+ token types with a custom light-mode-friendly theme (mostly blues, greens, grays)

### Interactive Elements
- **Popups/Popins**: Title bars, borders, shadows, scrollbars (with focused/unfocused states)
- **Page toolbar**: Button icons, text, active/disabled states
- **Footnotes/Sidenotes**: Borders, highlights, scrollbars, backlinks
- **Admonitions**: 4 types (note, tip, warning, error) with distinct color schemes

### Special Features
- **Math blocks**: Background colors and flash animation
- **Dropcaps**: 5 different dropcap font families with custom colors
- **Reader mode**: Masked links key toggle panel
- **Image focus**: Hover drop shadows
- **X of the day**: Border color

## Loading

This stylesheet is loaded in the `<head>` as part of the critical inline CSS bundle. It must load before any other stylesheets to ensure color variables are available when component styles are parsed.

The colors defined here are theme-neutral base values (effectively the light mode palette). Dark mode overrides are applied via `dark-mode-adjustments.css` when the user enables dark mode.

---

## See Also

- [dark-mode-adjustments](/css/dark-mode-adjustments) - Overrides these variables for dark mode
- [light-mode-adjustments](/css/light-mode-adjustments) - Additional light mode styling (patterns/textures)
- [initial](/css/initial) - Core layout and typography that references these color variables
- [default](/css/default) - Large generated file with remaining component styles
- [dark-mode-js](/frontend/dark-mode-js) - JavaScript that toggles between light/dark color schemes
- [build-mode-css](/php/build-mode-css) - Generates dark mode variants from these colors
