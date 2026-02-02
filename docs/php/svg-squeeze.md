---
sidebar_position: 3
---

# svg_squeeze.php

**Path:** `build/svg_squeeze.php` | **Language:** PHP | **Lines:** ~64

Reduces SVG file size by intelligently rounding path coordinate precision based on scale.

## Overview

This script optimizes SVG files by reducing numeric precision in path coordinates while preserving visual quality. Unlike naive rounding that uses fixed decimal places, `svg_squeeze.php` analyzes the range of coordinate values in each SVG and dynamically determines the optimal precision based on the image's scale. This can significantly reduce file size for complex vector graphics without noticeable quality loss.

The "squeeze factor" parameter controls how aggressively to round: higher values preserve more precision, while lower values result in more aggressive compression. Values of 2 or 3 work well for most images, balancing size reduction with visual fidelity.

The script processes only `<path d="...">` elements, which contain the coordinate data for vector paths, leaving other SVG attributes untouched.

## Key Functions/Variables

### Functions

- **`file_force_contents($path, $contents)`**: Creates output directory structure if needed and writes the processed SVG file.

### Variables

- **`$squeeze_factor`**: CLI argument (typically 2 or 3) that controls rounding precision. Lower values = more aggressive rounding = smaller files.
- **`$path_regexp`**: Regex pattern `/(<path[^<>]*d=")(.+?)("[^<>]*>)/` to match `<path>` elements and capture the `d` attribute value.
- **`$value_regexp`**: Regex pattern `/(?<![0-9\.])[0-9]+(?:\.[0-9]+)?(?![0-9\.])/` to match numeric coordinate values within path data.
- **`$all_values`**: Array collecting all numeric coordinates from all paths in the SVG.
- **`$scale`**: The range between min and max coordinate values, used to calculate optimal precision.
- **`$precision`**: Dynamically calculated decimal places for rounding: `((round(log10($scale)) - 1) * -1) + $squeeze_factor`.

## Algorithm

1. **Extract all coordinates**: Parse all `<path d="...">` elements and collect numeric values
2. **Calculate scale**: Find the range (`max - min`) of all coordinate values
3. **Determine precision**: Use formula based on scale's order of magnitude:
   ```php
   $precision = ((round(log10($scale)) - 1) * -1) + $squeeze_factor
   ```
   - For large-scale images (e.g., viewBox 0-1000), this might yield precision of 0-1 decimal places
   - For small-scale images (e.g., viewBox 0-10), this preserves more decimal places
4. **Round all values**: Apply `round($value, $precision)` to every coordinate
5. **Output to squeezed/ directory**: Write processed SVG with reduced precision

## Input/Output

### Inputs

- **Squeeze factor**: First CLI argument (e.g., `2` or `3`)
- **SVG files**: Remaining arguments are paths to SVG files to process

### Outputs

- **Processed SVGs**: Written to `squeezed/` subdirectory relative to current working directory, preserving the original path structure

Example:
```bash
# Input: icons/arrow.svg, icons/menu.svg
php svg_squeeze.php 2 icons/arrow.svg icons/menu.svg

# Output: ./squeezed/icons/arrow.svg, ./squeezed/icons/menu.svg
```

## Precision Calculation Example

For an SVG with coordinates ranging from 0 to 1000:
- `$scale = 1000 - 0 = 1000`
- `log10(1000) = 3`
- `round(3) = 3`
- `(3 - 1) * -1 = -2`
- With `$squeeze_factor = 2`: `$precision = -2 + 2 = 0` (round to integers)

For an SVG with coordinates ranging from 0 to 10:
- `$scale = 10 - 0 = 10`
- `log10(10) = 1`
- `round(1) = 1`
- `(1 - 1) * -1 = 0`
- With `$squeeze_factor = 2`: `$precision = 0 + 2 = 2` (preserve 2 decimal places)

This ensures that small-scale graphics retain fidelity while large-scale graphics get more aggressive compression.

## Usage

```bash
# Squeeze multiple SVG files with factor 2
php svg_squeeze.php 2 icon1.svg icon2.svg icon3.svg

# More aggressive squeezing (factor 1)
php svg_squeeze.php 1 large-diagram.svg

# More conservative (factor 3)
php svg_squeeze.php 3 detailed-illustration.svg
```

The script prints each filename as it processes it, and outputs results to a `squeezed/` directory in the current working directory.

---

## See Also

- [svg_strip_background.php](/php/svg-strip-background) - Removes background rectangles from SVGs
- [build_icon_sprite_file.php](/php/build-icon-sprite-file) - Combines SVGs into sprite sheets
- [build_asset_versions.php](/php/build_asset_versions) - Versions SVG assets for cache busting
- [build_unified_assets.php](/php/build_unified_assets) - May reference optimized SVGs
- [sync.sh](/backend/sync-sh) - Build orchestrator that may invoke SVG optimization
- [LinkIcon.hs](/backend/link-icon-hs) - Backend system using SVG icons
