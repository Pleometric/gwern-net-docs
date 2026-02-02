---
sidebar_position: 1
---

# build_paths.php

**Path:** `build/build_paths.php` | **Language:** PHP | **Lines:** ~14

Defines global directory path constants used throughout the PHP build system.

## Overview

This is a simple configuration file that establishes the canonical directory structure for the gwern.net build system. It defines the locations of all major asset directories (JavaScript, CSS, fonts, images, templates) relative to the build directory itself. These global variables are imported by other PHP build scripts via `require_once()` to ensure consistent path resolution across the entire build pipeline.

The script uses PHP's `__DIR__` magic constant to anchor all paths relative to the build directory, making the setup portable and eliminating hardcoded absolute paths. This is one of the first files loaded by other build scripts, as it provides the foundational path infrastructure.

All paths are defined as global variables prefixed with `$` and suffixed with `_dir` or `_root`, making them easy to identify and use throughout the build system.

## Key Variables

- `$build_dir` - The build directory itself (`__DIR__`)
- `$static_root` - Parent directory containing all static assets (`{$build_dir}/..`)
- `$js_dir` - JavaScript source directory (`{$static_root}/js`)
- `$css_dir` - CSS stylesheets directory (`{$static_root}/css`)
- `$font_dir` - Web fonts directory (`{$static_root}/font`)
- `$img_dir` - Images root directory (`{$static_root}/img`)
- `$icon_dir` - Icon assets subdirectory (`{$static_root}/img/icon`)
- `$logo_dir` - Logo assets subdirectory (`{$static_root}/img/logo`)
- `$include_dir` - Server-side includes directory (`{$static_root}/include`)
- `$template_dir` - HTML template fragments directory (`{$static_root}/template/include`)

## Input/Output

**Input:** None (pure configuration)

**Output:** Defines global PHP variables for use by other scripts

## Usage

Imported at the top of other PHP build scripts:

```php
require_once(__DIR__ . '/build_paths.php');
global $static_root, $css_dir, $js_dir;  // Access needed paths
```

Used by:
- `version_asset_links.php` - References `$static_root`, `$css_dir`, `$icon_dir`
- `build_functions.php` - References `$static_root` for asset versioning
- Other build scripts that need consistent directory paths

---

## See Also

- [build_variables.php](/php/build_variables) - Shared state for tracking updated files
- [build_functions.php](/php/build_functions) - Utility functions using these paths
- [build_unified_assets.php](/php/build_unified_assets) - Main bundler that imports these paths
- [version_asset_links.php](/php/version_asset_links) - Asset versioning that depends on these paths
- [build_head_includes.php](/php/build_head_includes) - Include generator using path constants
- [sync.sh](/backend/sync-sh) - Build orchestrator with parallel path structure
