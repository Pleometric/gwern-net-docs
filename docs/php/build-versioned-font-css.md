---
sidebar_position: 2
---

# build_versioned_font_css.php

**Path:** `build/build_versioned_font_css.php` | **Language:** PHP | **Lines:** ~40

Adds cache-busting version parameters to font file URLs in generated CSS.

## Overview

This script takes the CSS files produced by `build_font_css.php` and transforms them into versioned variants by appending modification timestamps to font file URLs. This enables aggressive browser caching while ensuring users always receive updated fonts when files change.

The versioning strategy uses query parameters (`?v=timestamp`) based on the actual modification time of each font file on disk. This approach is superior to build-time versioning because it accurately reflects when individual font files were updated, not when the CSS was regenerated.

The script is intentionally simple and focused: it performs a regex replacement on font URLs, delegates actual timestamp retrieval to `filemtime()`, and produces drop-in replacements for the original CSS files.

## Key Functions/Variables

**`VersionAssetURL($m)`**
Callback function for `preg_replace_callback` that receives a regex match array and returns the versioned URL. It extracts the file path, retrieves the modification timestamp using `filemtime()`, and constructs a versioned URL with `?v={timestamp}` appended.

**Global variables:**
- `$static_root`: Root directory path for static assets, used to construct absolute paths for `filemtime()` calls
- `$font_files`: Array of base filenames to process (`['fonts', 'initial-fonts']`)

## Input/Output

**Inputs:**
- `/css/fonts-GENERATED.css` - Generated font CSS from `build_font_css.php`
- `/css/initial-fonts-GENERATED.css` - Initial subset font CSS from `build_font_css.php`
- `/static/*` - Font files on disk (for `filemtime()` calls)

**Outputs:**
- `/css/fonts-VERSIONED.css` - Font CSS with versioned URLs
- `/css/initial-fonts-VERSIONED.css` - Initial font CSS with versioned URLs

## Usage

Invoked by `sync.sh` after `build_font_css.php` completes:

```bash
php build/build_versioned_font_css.php
```

The versioned CSS files are then used as the canonical font stylesheets referenced by HTML templates and build processes.

---

## See Also

- [build_font_css.php](/php/build-font-css) - Generates the base font CSS files this script processes
- [font_spec.php](/php/font-spec) - Font specification database defining font families
- [build_unified_assets.php](/php/build_unified_assets) - Bundles versioned font CSS into main stylesheets
- [build_functions.php](/php/build_functions) - `VersionedAssetHref()` for similar cache-busting logic
- [build_paths.php](/php/build_paths) - Directory path constants for font file locations
- [sync.sh](/backend/sync-sh) - Build orchestrator that invokes this script
