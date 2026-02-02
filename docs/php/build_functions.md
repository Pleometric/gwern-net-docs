---
sidebar_position: 3
---

# build_functions.php

**Path:** `build/build_functions.php` | **Language:** PHP | **Lines:** ~19

Provides utility functions for cache-busting asset URLs via file modification timestamps.

## Overview

This file exports a single utility function `VersionedAssetHref()` that generates cache-busted URLs for static assets by appending a version query parameter based on file modification time. This is critical for browser cache invalidation when assets are updated.

The function implements a fallback strategy for finding asset files, checking for suffixed variants in this order: `-VERSIONED`, `-GENERATED`, and finally the base filename. This allows it to handle files that go through multiple build stages (e.g., CSS that's first generated, then versioned). The function returns an absolute URL string with the `?v={timestamp}` query parameter, or terminates the build with an error if the file cannot be found.

This is typically used in PHP template files or HTML generation scripts that need to emit `<link>`, `<script>`, or other asset references with automatic cache-busting support.

## Key Functions

### `VersionedAssetHref($file_name, $file_extension)`

Returns a versioned href string for a static asset.

**Parameters:**
- `$file_name` (string) - Base name without extension (e.g., `"style"`)
- `$file_extension` (string) - Extension with leading dot (e.g., `".css"`)

**Returns:** String in format `"/static/{name}{ext}?v={mtime}"`

**Behavior:**
1. Checks for `{$file_name}-VERSIONED{$extension}` in `$static_root`
2. Falls back to `{$file_name}-GENERATED{$extension}`
3. Falls back to `{$file_name}{$extension}`
4. Uses `filemtime()` to get file's last modification timestamp
5. Returns the URL with timestamp as version parameter
6. Dies with error message if no file variant exists

**Example:**
```php
echo VersionedAssetHref("style", ".css");
// Output: "/static/style.css?v=1704672000"
```

## Input/Output

**Input:**
- Requires `$static_root` from `build_paths.php`
- Reads file modification times from filesystem

**Output:**
- Returns versioned URL strings for use in HTML templates
- Terminates build process if asset file not found

## Usage

Typically invoked from PHP templates or HTML generation scripts:

```php
require_once(__DIR__ . '/build_paths.php');
require_once(__DIR__ . '/build_functions.php');

global $static_root;

// In template:
<link rel="stylesheet" href=<?php echo VersionedAssetHref("style", ".css"); ?>>
<script src=<?php echo VersionedAssetHref("extracts", ".js"); ?>></script>
```

The function is called during HTML generation (not during asset building) to ensure that rendered HTML contains URLs with current file modification times.

---

## See Also

- [build_paths.php](/php/build_paths) - Defines `$static_root` used by this function
- [build_variables.php](/php/build_variables) - Shared state (often imported together)
- [version_asset_links.php](/php/version_asset_links) - Embeds version timestamps into CSS files themselves
- [build_head_includes.php](/php/build_head_includes) - Uses `VersionedAssetHref()` for head assets
- [build_body_includes.php](/php/build_body_includes) - Uses `VersionedAssetHref()` for deferred assets
- [build_unified_assets.php](/php/build_unified_assets) - Main bundler that imports these functions
