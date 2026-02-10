---
sidebar_position: 4
---

# svg_strip_background.php

**Path:** `build/svg_strip_background.php` | **Language:** PHP | **Lines:** ~29

Removes full-size background rectangles from SVG files.

## Overview

This utility script removes background rectangles that span the entire canvas of an SVG image. Many design tools (Figma, Illustrator, etc.) export SVGs with a full-size background rectangle, which is usually unnecessary for web use and increases file size. This script identifies and removes such backgrounds by detecting `<path>` elements that draw a rectangle matching the SVG's width and height.

The script is particularly useful for icons and graphics that should have transparent backgrounds. It modifies SVG files in-place, preserving all other elements and attributes.

The detection is based on a specific path syntax (`M-0 -0L{width} 0L{width} {height}L-0 {height}L0 -0Z`) that creates a rectangle from corner to corner. This pattern is common in SVG exports that include background layers.

## Key Functions/Variables

### Variables

- **`$svg_tag_regexp`**: Pattern to extract `width` and `height` from the `<svg>` tag: `/<svg ([^<>]* )?width="(.+?)" ([^<>]* )?height="(.+?)"[^<>]*>/`
- **`$path_regexp`**: Dynamic pattern constructed using extracted width/height to match the background rectangle path. Looks for: `M-0 -0L{width} 0L{width} {height}L-0 {height}L0 -0Z`
- **`$file_names`**: Array of SVG file paths from command-line arguments
- **`$width`** and **`$height`**: Extracted from the `<svg>` tag to construct the background path pattern

### Path Patterns

The script looks for this specific path syntax (absolute coordinates):
```
M-0 -0L{width} 0L{width} {height}L-0 {height}L0 -0Z
```

There's also a commented-out alternative pattern for relative coordinates:
```
M0 0h{width}v{height}H0z
```

## Input/Output

### Inputs

- **SVG files**: One or more SVG file paths as command-line arguments

### Outputs

- **Modified SVG files**: Original files are overwritten with the background path removed (if found)

The script prints each filename as it processes it.

## Background Removal Process

1. **Read SVG file**: Load entire file contents
2. **Extract dimensions**: Use regex to find `width="..."` and `height="..."` in `<svg>` tag
3. **Construct path pattern**: Build regex to match a path that draws a rectangle from (0,0) to (width, height)
4. **Match background path**: Search for `<path d="M-0 -0L{width} 0L{width} {height}L-0 {height}L0 -0Z"...>`
5. **Remove matched path**: Replace the entire `<path>` element with empty string
6. **Write back**: Overwrite original file with modified SVG

## Example

Given an SVG with:
```xml
<svg width="100" height="50">
  <path d="M-0 -0L100 0L100 50L-0 50L0 -0Z" fill="#ffffff"/>
  <circle cx="50" cy="25" r="20" fill="#000000"/>
</svg>
```

After processing:
```xml
<svg width="100" height="50">
  <circle cx="50" cy="25" r="20" fill="#000000"/>
</svg>
```

The background rectangle is removed, leaving only the content.

## Usage

```bash
# Process one SVG
php svg_strip_background.php icon.svg

# Process multiple SVGs
php svg_strip_background.php icon1.svg icon2.svg icon3.svg

# Process all SVGs in a directory
php svg_strip_background.php icons/*.svg
```

Files are modified in-place, so make backups if needed.

## Limitations

- Only detects background paths in the specific format `M-0 -0L{width} 0L{width} {height}L-0 {height}L0 -0Z`
- Does not detect backgrounds that use `<rect>` elements
- Does not detect backgrounds with different path syntaxes (though an alternative pattern is commented out)
- Modifies files in-place with no backup

---

## See Also

- [svg_squeeze.php](/php/svg-squeeze) - Reduces SVG file size by rounding coordinate precision
- [build_icon_sprite_file.php](/php/build-icon-sprite-file) - May call this script before combining SVGs into a sprite
- [build_asset_versions.php](/php/build_asset_versions) - Versions SVG assets for cache busting
- [build_unified_assets.php](/php/build_unified_assets) - References optimized SVG assets
- [sync.sh](/backend/sync-sh) - Build orchestrator for SVG processing pipeline
- [LinkIcon.hs](/backend/link-icon-hs) - Backend system using SVG icons
