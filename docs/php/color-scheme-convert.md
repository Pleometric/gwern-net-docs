---
sidebar_position: 5
---

# color-scheme-convert.php

**Path:** `build/color-scheme-convert.php` | **Language:** PHP | **Lines:** ~649

Transforms CSS color values through various color spaces to generate dark mode or alternate color scheme stylesheets.

## Overview

This sophisticated script implements color space transformations to automatically generate alternate color schemes (particularly dark modes) from a source CSS file. Rather than manually duplicating and inverting colors, this tool mathematically transforms colors through perceptually uniform color spaces like CIE Lab, Oklab, or YCoCg.

The script parses CSS files, extracts hex colors (e.g., `#1a2b3c`) and rgba() values, transforms them according to specified modes (lightness inversion, hue rotation, colorization), and outputs a new stylesheet. This enables maintainable dark mode generation where changes to the light theme automatically propagate to the dark theme through algorithmic transformation.

Originally authored by Said Achmiz and adapted for gwern.net, it supports multiple color spaces, transformation modes, and gamma correction for fine-tuning lightness curves. The script is particularly valuable for sites with extensive custom styling where manual dark mode maintenance would be impractical.

## Key Functions

### Main Processing

- `ProcessColorValue($m)` - Transforms hex colors (`#rrggbb` or `#rgb`)
- `ProcessColorValue_RGBA($m)` - Transforms rgba() color values
- `CVT($value, $color_space)` - Core Color Value Transform function

### Transformation Operations

The `$mode` parameter is a bitmask controlling transformations:

- `0x0001` - Lightness inversion (in Lab/YCC/Oklab/Oklch)
- `0x0002` - Hue inversion (flips a/b channels or rotates hue 180°)
- `0x0004` - Map whites to red (H=0°, S=max in HSV)
- `0x0008` - Map whites to yellow (H=60°)
- `0x0010` - Map whites to green (H=120°)
- `0x0020` - Map whites to cyan/teal (H=180°)
- `0x0040` - Map whites to blue (H=240°)
- `0x0080` - Map whites to magenta (H=300°)

### Color Space Conversions

The script implements complete bidirectional conversion between:
- **RGB** ↔ HSV, HSL (color wheel models)
- **RGB** ↔ XYZ ↔ Lab (CIE perceptually uniform)
- **RGB** ↔ XYZ ↔ Oklab ↔ Oklch (modern perceptually uniform)
- **RGB** ↔ YCoCg (YCC, luma-chroma)

Key conversion functions:
- `fromRGB($rgb, $target_space)` - Dispatch to appropriate converter
- `RGBFrom($color, $source_space)` - Convert back to RGB
- Space-specific converters: `LabFromXYZ()`, `OklabFromXYZ()`, `HSVFromRGB()`, etc.

### Utilities

- `RGBFromHex($hex)` - Parse hex strings to RGB triplets
- `HexFromRGB($rgb)` - Convert RGB back to hex, optimizing 6→3 digit when possible
- `PCC($components)` - "Print Color Components" for debug logging

## Input/Output

**Input:**
- `$argv[1]` (required) - Path to source CSS file
- `$argv[2]` (optional) - Mode bitmask (default: 1 = lightness inversion)
- `$argv[3]` (optional) - Working color space (default: "Lab")
- `$argv[4]` (optional) - Gamma correction factor (default: 0.55)

**Output:**
- Transformed CSS to stdout
- Debug logs to stderr (when `$debug_enabled = true`)

## Usage

Run as a command-line script:

```bash
# Basic dark mode (lightness inversion in Lab space):
php color-scheme-convert.php input.css > dark.css

# Lightness + hue inversion in Oklab space:
php color-scheme-convert.php input.css 3 Oklab > dark.css

# Lightness inversion with colorization to blue:
php color-scheme-convert.php input.css 65 Lab > dark-blue.css
# (0x0001 | 0x0040 = 65 = lightness invert + map whites to blue)

# Custom gamma for brightness curve adjustment:
php color-scheme-convert.php input.css 1 Lab 0.7 > dark-brighter.css
```

Typical integration in build pipeline:

```bash
# Generate dark mode from light mode CSS:
php build/color-scheme-convert.php css/style-light.css 1 Lab 0.55 > css/style-dark.css
```

### Working Color Spaces

- **Lab** (default) - CIE L\*a\*b\*, perceptually uniform, good for general use
- **Oklab** - Modern perceptually uniform space, better hue linearity
- **Oklch** - Oklab in cylindrical (hue-chroma) form
- **YCC** - YCoCg, fast chroma/luma separation
- **RGB** - No color space conversion (rarely useful)

### Gamma Parameter

Controls the darkness curve when inverting lightness. Lower values (0.4-0.5) produce softer darks, higher values (0.6-0.8) produce deeper blacks. Default 0.55 is a balanced compromise.

## Algorithm Details

1. **Parse CSS**: Regex extracts hex colors and rgba() values
2. **Convert to working space**: RGB → XYZ → Lab/Oklab/etc.
3. **Apply transformations** (per mode bits):
   - Invert lightness: `L = (1 - L)^gamma`
   - Invert hue: negate a/b channels or rotate H by 180°
   - Colorize: Set HSV hue or Oklch hue to target color
4. **Convert back to RGB**: Working space → XYZ → RGB
5. **Clamp and round**: Ensure RGB values are valid [0, 255]
6. **Output CSS**: Replace original colors with transformed hex/rgba

The use of perceptually uniform color spaces (Lab, Oklab) ensures that "lightness inversion" produces subjectively equivalent brightness reversals, not just mathematical inversions that appear unbalanced to human vision.

---

## See Also

- [build-mode-css](/php/build-mode-css) - PHP script that invokes this color converter
- [colors](/css/colors) - Base color definitions (input to this script)
- [dark-mode-adjustments](/css/dark-mode-adjustments) - Combined with generated dark colors
- [dark-mode-js](/frontend/dark-mode-js) - Client-side theme toggling
- [color-js](/frontend/color-js) - JavaScript equivalent for runtime color transforms
- [sync-sh](/backend/sync-sh) - Build orchestrator that runs CSS generation
