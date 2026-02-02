---
sidebar_position: 5
---

# build_inlined_images.php

**Path:** `build/build_inlined_images.php` | **Language:** PHP | **Lines:** ~85

Generates CSS custom property definitions containing base64-encoded images.

## Overview

This script converts image files (SVG and bitmap formats) into CSS custom properties with data URIs, enabling critical images to be embedded directly into stylesheets. This eliminates render-blocking HTTP requests for essential visual elements like ornaments, patterns, and logos, improving perceived performance during initial page load.

The script processes two categories of images: initial images (inlined into `head.css` for immediate rendering) and regular images (inlined into `style.css` for progressive enhancement). Each image becomes a CSS variable named `--GW-image-{filename}` that can be referenced in stylesheets via `var(--GW-image-logo-smooth-svg)`.

SVG files receive special treatment: instead of base64 encoding, they're embedded as UTF-8 text with minimal transformations (hash escaping, whitespace compression). This produces more compact and debuggable CSS compared to base64-encoded SVGs, while still avoiding HTTP requests.

## Key Functions/Variables

**`css_variable_line_from_svg_file($svg_file_path)`**
Generates a CSS custom property line for an SVG file. Reads the SVG contents, applies transformations (escapes `#` as `%23`, collapses whitespace, removes spacing between tags), and constructs a data URI using UTF-8 encoding rather than base64.

**`css_variable_line_from_bitmap_image_file($image_file_path)`**
Generates a CSS custom property line for bitmap images (PNG, JPG, etc.). Reads the file contents, base64-encodes them, and constructs an appropriate data URI with the correct MIME type inferred from the file extension.

**Global variables:**
- `$inlined_images_files_structure`: Associative array mapping output file prefixes to arrays of glob patterns for source images

**Structure:**
```php
[
  "inlined-images-initial" => [
    "/static/img/ornament/inlined-initial/*.*",
    "/static/img/pattern/*.*"
  ],
  "inlined-images" => [
    "/static/img/logo/logo-smooth.svg",
    "/static/img/ornament/inlined/*.*"
  ]
]
```

## Input/Output

**Inputs:**
- `/static/img/ornament/inlined-initial/*.*` - Critical ornamental images for initial render
- `/static/img/pattern/*.*` - Background pattern images
- `/static/img/logo/logo-smooth.svg` - Site logo
- `/static/img/ornament/inlined/*.*` - Additional ornamental images

**Outputs:**
- `/css/inlined-images-initial-GENERATED.css` - CSS variables for critical initial images
- `/css/inlined-images-GENERATED.css` - CSS variables for progressive enhancement images

**Output format:**
```css
/**********/
/* IMAGES */
/**********/

:root {
  --GW-image-logo-smooth-svg: url('data:image/svg+xml;utf8,<svg>...</svg>');
  --GW-image-ornament-dots-png: url('data:image/png;base64,iVBORw0K...');
  ...
}
```

## Usage

Invoked by `sync.sh` during the CSS build phase:

```bash
php build/build_inlined_images.php
```

The generated CSS files are imported into `head.css` and `style.css`, making the image variables available throughout the stylesheet cascade.

**Using inlined images in CSS:**
```css
.ornament::before {
  content: var(--GW-image-ornament-dots-png);
}

.logo {
  background-image: var(--GW-image-logo-smooth-svg);
}
```

## See Also

- [Image.hs](/backend/image-hs) - Server-side image processing pipeline
- [sync.sh](/backend/sync-sh) - Build orchestrator that calls this script
- [should_image_have_outline.php](/php/should-image-have-outline) - Related PHP image tool
- [png.sh](/shell/png) - PNG optimization before inlining
- [colors.css](/css/colors) - CSS that uses inlined image variables
- [initial.css](/css/initial) - Initial CSS that may include inlined images
- [image-focus.js](/frontend/image-focus-js) - Client-side image handling
