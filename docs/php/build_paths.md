---
title: "build_paths.php"
description: "This is a simple configuration file that establishes the canonical directory structure for the gwern.net build system."
sidebar_position: 1
---

# build_paths.php

This is a simple configuration file that establishes the canonical directory structure for the gwern.net build system.

<div className="doc-meta">
  <div><strong>Path</strong><code>build/build_paths.php</code></div>
  <div><strong>Language</strong>PHP</div>
  <div><strong>Lines</strong>14</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/build_paths.php">build/build_paths.php</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing PHP asset generation, build hooks, template assembly, or maintenance scripts around build_paths.
</div>

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

<details className="generated-section">
<summary>See Also</summary>

- [build_variables.php](/php/build_variables) - Shared state for tracking updated files
- [build_functions.php](/php/build_functions) - Utility functions using these paths
- [build_unified_assets.php](/php/build_unified_assets) - Main bundler that imports these paths
- [version_asset_links.php](/php/version_asset_links) - Asset versioning that depends on these paths
- [build_head_includes.php](/php/build_head_includes) - Include generator using path constants
- [sync.sh](/backend/sync-sh) - Build orchestrator with parallel path structure
</details>
