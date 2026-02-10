---
sidebar_position: 4
---

# version_asset_links.php

**Path:** `build/version_asset_links.php` | **Language:** PHP | **Lines:** ~34

Adds cache-busting version parameters to asset URLs embedded within CSS files.

## Overview

This script implements CSS-level cache busting by scanning generated CSS files for asset URLs (like SVG icons) and appending version query parameters based on file modification times. Unlike `build_functions.php` which generates versioned URLs for `<link>` and `<script>` tags, this script modifies the CSS files themselves to version-stamp URLs they contain.

The script runs during the build pipeline after CSS generation but before final deployment. It reads `-GENERATED.css` files, identifies asset references (currently just `icons.svg`), calculates their modification timestamps, performs string replacement to add `?v={timestamp}` parameters, and writes the result to `-VERSIONED.css` files.

This two-stage approach (GENERATED → VERSIONED) ensures that CSS files reference the most current versions of assets they depend on, preventing browser cache staleness when icons or other embedded resources are updated.

## Key Operations

1. **Asset inventory**: Builds a map of asset paths to modification times
   - Currently limited to `{$icon_dir}/icons.svg`
   - Can be extended to other embedded assets

2. **CSS processing**: For each target CSS file:
   - Reads the `-GENERATED` variant
   - Replaces asset URLs with versioned variants
   - Writes to `-VERSIONED` output file
   - Records output path in `$updated_files`

3. **Target files**:
   - `{$css_dir}/head-GENERATED.css` → `head-VERSIONED.css`
   - `{$css_dir}/style-GENERATED.css` → `style-VERSIONED.css`

## Input/Output

**Input:**
- `{$css_dir}/head-GENERATED.css` - Generated critical CSS
- `{$css_dir}/style-GENERATED.css` - Generated main stylesheet
- `{$icon_dir}/icons.svg` - SVG icon sprite (for timestamp)

**Output:**
- `{$css_dir}/head-VERSIONED.css` - CSS with versioned asset URLs
- `{$css_dir}/style-VERSIONED.css` - CSS with versioned asset URLs
- Updates `$updated_files` array with output paths

## Usage

Invoked by the build system after CSS generation:

```bash
php build/version_asset_links.php
```

Prints: `Versioning assets links...`

Typically called from `sync.sh` or similar orchestrator during the asset pipeline:

```bash
# After CSS generation:
make css  # Produces *-GENERATED.css files

# Version asset references within CSS:
php build/version_asset_links.php  # Produces *-VERSIONED.css files

# Deploy versioned files
```

The script requires that:
1. The `-GENERATED.css` files already exist (produced by earlier build steps)
2. The asset files being referenced (like `icons.svg`) exist and are readable
3. The output directory is writable

---

## See Also

- [build_functions.php](/php/build_functions) - Versions `<link>` and `<script>` tag URLs in HTML
- [build_paths.php](/php/build_paths) - Provides directory path constants
- [build_variables.php](/php/build_variables) - Tracks which files were updated
- [build_unified_assets.php](/php/build_unified_assets) - Creates the GENERATED CSS files this script processes
- [build_head_includes.php](/php/build_head_includes) - References the VERSIONED CSS in HTML includes
- [sync.sh](/backend/sync-sh) - Build orchestrator that invokes this script
