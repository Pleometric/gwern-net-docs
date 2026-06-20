---
title: "build_asset_versions.php"
description: "This script creates a version manifest for static assets (icons, logos, fonts) that enables browser cache invalidation when files change."
sidebar_position: 2
---

# build_asset_versions.php

This script creates a version manifest for static assets (icons, logos, fonts) that enables browser cache invalidation when files change.

<div className="doc-meta">
  <div><strong>Path</strong><code>build/build_asset_versions.php</code></div>
  <div><strong>Language</strong>PHP</div>
  <div><strong>Lines</strong>52</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/build_asset_versions.php">build/build_asset_versions.php</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing PHP asset generation, build hooks, template assembly, or maintenance scripts around build_asset_versions.
</div>

## Overview

This script creates a version manifest for static assets (icons, logos, fonts) that enables browser cache invalidation when files change. It scans specific asset directories for image and SVG files, extracts their modification timestamps, and outputs a JavaScript object (`GW.assetVersions`) that maps URL paths to timestamps.

The generated `asset-versions-GENERATED.js` file gets included in the initial JavaScript bundle and is used by frontend code to append `?v={timestamp}` query parameters to asset URLs. This ensures browsers fetch updated versions of assets when they change, while allowing aggressive long-term caching of unchanged files.

This script must run before `build_unified_assets.php` since the output file (`asset-versions-GENERATED.js`) is included in the `$head_js` array and bundled into `head-GENERATED.js`.

## Key Functions/Variables

### Asset Discovery

- **`$asset_file_paths`**: Array initially containing direct file paths (e.g., `icons.svg`)
- **`$asset_patterns`**: Array of glob patterns for discovering versioned assets:
  - `{$logo_dir}/*/*-small-*.png` - Small logo variants
  - `{$logo_dir}/*/*/*-small-*.png` - Nested small logo variants
  - `{$logo_dir}/*/*.svg` - Logo SVGs
  - `{$logo_dir}/*/*/*.svg` - Nested logo SVGs
  - `{$font_dir}/dropcap/*/*-small-*.png` - Dropcap font images
  - `{$font_dir}/dropcap/*/*/*-small-*.png` - Nested dropcap images
  - `{$font_dir}/dropcap/*/*.svg` - Dropcap SVGs
  - `{$font_dir}/dropcap/*/*/*.svg` - Nested dropcap SVGs

### Asset Processing

- **`$assets`**: Associative array mapping URL paths (relative to `/static/`) to modification timestamps
- **`filemtime()`**: PHP function extracting file modification time in Unix timestamp format
- **`substr($asset_file_path, strlen($static_root))`**: Strips the filesystem prefix to create URL-relative paths

### Output Format

```javascript
GW.assetVersions = {
    "/static/img/icon/icons.svg": "1704672000",
    "/static/img/logo/example/small-variant.png": "1704668400",
    // ... more assets
};
```

## Input/Output

### Input Files

- `/img/icon/icons.svg` (direct path)
- `/img/logo/*/*-small-*.png` (glob pattern)
- `/img/logo/*/*/*-small-*.png` (nested glob pattern)
- `/img/logo/*/*.svg`
- `/img/logo/*/*/*.svg`
- `/font/dropcap/*/*-small-*.png`
- `/font/dropcap/*/*/*-small-*.png`
- `/font/dropcap/*/*.svg`
- `/font/dropcap/*/*/*.svg`

### Output Files

- `/js/asset-versions-GENERATED.js` - JavaScript object with asset URL-to-timestamp mappings

The output file path is appended to `$updated_files[]` for downstream tracking.

## Usage

Invoked during the build process before asset bundling:

```bash
php /path/to/build/build_asset_versions.php
```

No command-line arguments required. The script:
1. Requires `build_paths.php` and `build_variables.php`
2. Globs for asset files matching the specified patterns
3. Extracts modification timestamps using `filemtime()`
4. Generates a JavaScript object literal
5. Writes `asset-versions-GENERATED.js` to the `/js/` directory

**Build order dependency:** Must run before `build_unified_assets.php` since that script includes `asset-versions-GENERATED.js` in the `$head_js` bundle.

---

<details className="generated-section">
<summary>See Also</summary>

- [build_unified_assets.php](/php/build_unified_assets) - Includes the generated file in the head JS bundle
- [build_functions.php](/php/build_functions) - `VersionedAssetHref()` function uses modification times for CSS/JS versioning
- [build_paths.php](/php/build_paths) - Directory path constants used for asset discovery
- [build_variables.php](/php/build_variables) - Shared state for tracking updated files
- [sync.sh](/backend/sync-sh) - Orchestrates the build pipeline and ensures correct execution order
- [pre-commit-hook.php](/php/pre-commit-hook) - Git hook that triggers version regeneration
- [initial.js](/frontend/initial-js) - Consumes `GW.assetVersions` for runtime asset URL construction
</details>
