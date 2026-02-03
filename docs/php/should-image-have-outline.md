---
sidebar_position: 5
---

# should_image_have_outline.php

**Path:** `build/should_image_have_outline.php` | **Language:** PHP | **Lines:** ~188

Heuristic analyzer that determines whether an image should have a CSS outline for better contrast.

## Overview

This script analyzes images (PNG, JPEG, SVG) to automatically decide whether they need a border/outline for proper visual separation from the page background. It's used by the Haskell build system (`Image.hs`) when images don't have explicit `.outline` or `.outline-not` classes, allowing gwern.net to make intelligent styling decisions at build time.

The heuristic is based on corner pixel analysis: images with varied corner colors, transparent corners, or colored corners typically benefit from outlines for contrast, while images with uniform black or white corners usually blend naturally with page backgrounds and don't need borders.

The script returns `1` (needs outline) or `0` (no outline needed) and is designed to be conservative with validation—it includes extensive error checking for file size, dimensions, and image integrity to prevent build failures.

## Key Functions/Variables

### Helper Functions

- **`is_opaque($color)`**: Returns true if alpha channel is 0 (fully opaque in GD library's inverted alpha scale)
- **`is_white($color)`**: Returns true if color is pure white (#FFFFFF)
- **`is_black($color)`**: Returns true if color is pure black (#000000)
- **`is_grey($color)`**: Returns true if R, G, B values are equal (greyscale)
- **`are_all_same($colors)`**: Returns true if all colors in an array are identical (R, G, B, alpha)
- **`are_same($color_a, $color_b)`**: Compares two GD color arrays for exact equality

### Configuration Constants

- **`$MAX_FILESIZE_BYTES`**: 100 MB maximum file size (100 × 1024 × 1024)
- **`$MIN_FILESIZE_BYTES`**: 500 bytes minimum (0.5 KB)
- **`$MAX_WIDTH`**, **`$MAX_HEIGHT`**: 32,768 pixels (common GPU texture size limit)
- **`$MIN_WIDTH`**, **`$MIN_HEIGHT`**: 25 pixels (too small to analyze meaningfully)
- **Memory limit**: 16,384 × 16,384 = 268,435,456 pixels maximum (prevents memory exhaustion)

### Variables

- **`$image_path`**: Input file path from CLI argument
- **`$image`**: GD image resource created from PNG or JPEG
- **`$corner_colors`**: Array of 4 color arrays (top-left, top-right, bottom-left, bottom-right) from `imageColorsForIndex()`

## Decision Logic

The script follows this decision tree:

```
1. SVG? → Always return "0" (no outline)
2. Load PNG/JPEG with GD library
3. Extract 4 corner pixel colors (TL, TR, BL, BR)
4. Decision tree:
   - Corners differ in color? → "1" (outline)
   - Corners same but transparent? → "1" (outline)
   - Corners same and opaque:
     - Pure black (#000000)? → "0" (no outline)
     - Pure white (#FFFFFF)? → "0" (no outline)
     - Any other color? → "1" (outline)
```

### Rationale

- **Varied corners**: Image likely has content extending to edges; needs outline for separation
- **Transparent corners**: Image will blend with background; outline provides definition
- **Black/white opaque corners**: Natural page backgrounds; no outline needed
- **Other opaque corners**: Colored backgrounds; outline prevents clash with page theme
- **SVG**: Vector graphics scale well and usually have intentional backgrounds

## Input/Output

### Inputs

- **Image file**: Single CLI argument with path to `.jpg`, `.jpeg`, `.png`, or `.svg` file
- Extensions must be lowercase (enforced)

### Outputs

- **Exit code 0** with stdout: `"0"` (no outline) or `"1"` (outline)
- **Exit codes 1-11** for errors (see below)

No newline is appended to the output, allowing easy parsing in shell scripts.

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success (printed "0" or "1") |
| 1 | Invalid argument count (requires exactly 1 file) |
| 2 | Unsupported file extension (must be .jpg/.jpeg/.png/.svg) |
| 3 | Image loading failed (corrupt or invalid format) |
| 4 | Uppercase file extension (must be lowercase) |
| 5 | File does not exist or size unreadable |
| 6 | File too small (< 0.5 KB) |
| 7 | File too large (> 100 MB) |
| 8 | Image dimensions too small (< 25×25) |
| 9 | Image dimensions too large (> 32768×32768) |
| 10 | Image requires too much memory (> 16384×16384 pixels) |
| 11 | File not readable (permissions issue) |

## Usage

Called by Haskell build scripts (Image.hs):

```bash
# Returns "0" or "1"
php should_image_have_outline.php /path/to/image.png

# Usage in shell
if [ "$(php should_image_have_outline.php image.jpg)" = "1" ]; then
  echo "Add outline class"
fi
```

The script is designed to be fast and fail safely—any error results in a non-zero exit code rather than producing invalid output.

## Integration with Build System

Called by **Image.hs** during Hakyll build:

1. Image.hs encounters an `<img>` tag without `.outline` or `.outline-not` class
2. Calls `should_image_have_outline.php` with image path
3. If output is "1", adds class="outline" to the `<img>` tag
4. If output is "0", adds class="outline-not"
5. If exit code is non-zero, logs error and falls back to default behavior

## See Also

- [Image.hs](/backend/image-hs) - Haskell module that calls this script during HTML generation
- [build_inlined_images.php](/php/build-inlined-images) - Related PHP image processing
- [invertornot.py](/python/invertornot) - GPT-4-Vision image classification
- [image-margin-checker.py](/python/image-margin-checker) - GPT-4-Vision margin analysis
- [compressPNG](/shell/compress-png) - PNG compression optimization
- [image-focus.js](/frontend/image-focus-js) - Client-side image lightbox
- [Typography.hs](/backend/typography-hs) - AST transforms that may involve images
