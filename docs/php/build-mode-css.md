---
sidebar_position: 3
---

# build_mode_css.php

**Path:** `build/build_mode_css.php` | **Language:** PHP | **Lines:** ~21

Generates light and dark mode CSS files from base color definitions.

## Overview

This script orchestrates the generation of color mode stylesheets for gwern.net's light/dark theme system. It performs three key operations: converting base colors to dark mode using an Oklch color transformation algorithm, combining color definitions with mode-specific adjustment rules, and outputting ready-to-use CSS files for each theme.

The script delegates the actual color conversion to a separate PHP utility (`color-scheme-convert.php`) that implements perceptually-uniform color space transformations. This ensures dark mode colors maintain appropriate contrast and visual hierarchy while adapting to reduced lightness levels (0.55 factor).

Unlike the font CSS generators, this script uses shell command execution (backticks) to invoke external tools and combine files with `cat`, making it more of a build orchestrator than a pure generator. The resulting CSS files are used as critical initial styles loaded in the document head.

## Key Functions/Variables

This script contains no custom functions—it's a simple procedural script that executes three shell commands in sequence.

**Global variables:**
- `$build_dir`: Path to the build scripts directory
- `$css_dir`: Path to the CSS output directory

**External tools:**
- `color-scheme-convert.php`: Converts colors between color spaces and applies lightness transformations using the Oklch color model

## Input/Output

**Inputs:**
- `/css/colors.css` - Base color definitions (light mode)
- `/css/light-mode-adjustments.css` - Light mode-specific CSS adjustments
- `/css/dark-mode-adjustments.css` - Dark mode-specific CSS adjustments

**Outputs:**
- `/css/colors-dark-GENERATED.css` - Dark mode color definitions (Oklch-transformed)
- `/css/light-mode-GENERATED.css` - Complete light mode initial CSS (colors + adjustments)
- `/css/dark-mode-GENERATED.css` - Complete dark mode initial CSS (dark colors + adjustments)

**Command 1:** Generate dark colors via Oklch transformation
```bash
php build/color-scheme-convert.php css/colors.css 1 "Oklch" 0.55 > css/colors-dark-GENERATED.css
```
Converts all colors in `colors.css` to Oklch color space, applies a 0.55 lightness factor, and outputs the dark variants.

**Command 2:** Concatenate light mode files
```bash
cat css/colors.css css/light-mode-adjustments.css > css/light-mode-GENERATED.css
```

**Command 3:** Concatenate dark mode files
```bash
cat css/colors-dark-GENERATED.css css/dark-mode-adjustments.css > css/dark-mode-GENERATED.css
```

## Usage

Invoked by `sync.sh` during CSS generation phase:

```bash
php build/build_mode_css.php
```

The generated mode-specific CSS files are inlined into HTML templates to provide instant theme support without FOUC (flash of unstyled content).

---

## See Also

- [color-scheme-convert](/php/color-scheme-convert) - Oklch color space transformation utility
- [dark-mode-js](/frontend/dark-mode-js) - Client-side theme switcher
- [colors](/css/colors) - Base color definitions (light mode input)
- [dark-mode-adjustments](/css/dark-mode-adjustments) - Dark mode adjustments combined with generated colors
- [light-mode-adjustments](/css/light-mode-adjustments) - Light mode adjustments combined with base colors
- [sync-sh](/backend/sync-sh) - Build orchestrator that invokes this script
