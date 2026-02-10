---
sidebar_position: 1
---

# build_unified_assets.php

**Path:** `build/build_unified_assets.php` | **Language:** PHP | **Lines:** ~130

Concatenates individual CSS and JS modules into unified bundles for production deployment.

## Overview

This script is the primary asset bundler for gwern.net's frontend code. It reads lists of individual CSS and JavaScript files and concatenates them into unified bundles that get loaded in the browser. This approach balances developer ergonomics (working with modular source files) with production performance (minimizing HTTP requests).

The script produces four main bundles: `head-GENERATED.css` and `head-GENERATED.js` (critical resources loaded synchronously in the document head), and `style-GENERATED.css` and `script-GENERATED.js` (deferred resources loaded after initial paint). It also generates a special `transclude-templates-GENERATED.js` file that embeds HTML templates as JavaScript template literals.

This script runs early in the build process (typically invoked by `sync.sh`) and must complete before any HTML generation that references these bundled assets.

## Key Functions/Variables

### Asset Lists

- **`$head_css`**: Array of CSS files loaded in `<head>` (initial paint styles: inlined-images, initial.css, special-occasions.css, fonts, reader-mode)
- **`$head_js`**: Array of JS files loaded in `<head>` (core utilities: utility.js, initial.js, layout.js, color.js, dark-mode-initial.js, etc.)
- **`$css`**: Array of main CSS files loaded after initial paint (inlined-images-GENERATED.css, fonts-VERSIONED.css, default.css, links.css)
- **`$js`**: Array of main JS modules loaded deferred (popups, annotations, transclude, extracts, typography, collapse, sidenotes, etc.)

### Template Processing

- **`$templates`**: Glob result from `{$template_dir}/*.tmpl` - all HTML template files
- Template files are read, escaped (backticks and backslashes), and embedded as JavaScript template literals in the `Transclude.templates` object

### Generated Files

- `head-GENERATED.css` - Concatenated critical CSS
- `head-GENERATED.js` - Concatenated critical JavaScript
- `style-GENERATED.css` - Concatenated main CSS
- `script-GENERATED.js` - Concatenated main JavaScript
- `transclude-templates-GENERATED.js` - Embedded HTML templates as JS object

## Input/Output

### Input Files

**CSS:**
- `/css/inlined-images-initial-GENERATED.css`
- `/css/initial.css`
- `/css/special-occasions.css`
- `/css/initial-fonts-VERSIONED.css`
- `/css/reader-mode-initial.css`
- `/css/inlined-images-GENERATED.css`
- `/css/fonts-VERSIONED.css`
- `/css/default.css`
- `/css/links.css`

**JavaScript:**
- `/js/utility.js`
- `/js/initial.js`
- `/js/layout.js`
- `/js/color.js`
- `/js/rewrite-initial.js`
- `/js/special-occasions.js`
- `/js/dark-mode-initial.js`
- `/js/reader-mode-initial.js`
- `/js/asset-versions-GENERATED.js`
- `/js/misc.js`
- `/js/popups.js`, `/js/popins.js`
- `/js/annotations.js`
- `/js/content.js`, `/js/transclude.js`
- `/js/extracts.js`, `/js/extracts-annotations.js`, `/js/extracts-content.js`, `/js/extracts-options.js`, `/js/extracts-load.js`
- `/js/typography.js`
- `/js/Hyphenopoly_Loader.js`
- `/js/rewrite.js`, `/js/collapse.js`, `/js/sidenotes.js`
- `/js/image-focus.js`
- `/js/dark-mode.js`, `/js/reader-mode.js`

**Templates:**
- `/template/include/*.tmpl` (all template files)

### Output Files

- `/css/head-GENERATED.css`
- `/js/head-GENERATED.js`
- `/css/style-GENERATED.css`
- `/js/script-GENERATED.js`
- `/js/transclude-templates-GENERATED.js`

All output file paths are appended to `$updated_files[]` for potential downstream tracking.

## Usage

Invoked during the build process, typically by `sync.sh`:

```bash
php /path/to/build/build_unified_assets.php
```

No command-line arguments required. The script:
1. Requires `build_paths.php`, `build_variables.php`, and `build_functions.php`
2. Reads and concatenates files according to the hardcoded arrays
3. Writes generated bundles to disk
4. Exits silently on success, or errors if files are missing

---

## See Also

- [sync.sh](/backend/sync-sh) - Main build orchestrator that invokes this script
- [pre-commit-hook.php](/php/pre-commit-hook) - Git hook that triggers asset rebuilds
- [build_asset_versions.php](/php/build_asset_versions) - Generates cache-busting version hashes for assets
- [build_head_includes.php](/php/build_head_includes) - Creates inlined HTML includes that reference these bundles
- [build_body_includes.php](/php/build_body_includes) - Creates body-end includes with versioned asset references
- [build_functions.php](/php/build_functions) - Utility functions for versioning and file operations
- [transclude.js](/frontend/transclude-js) - Consumes the generated `transclude-templates-GENERATED.js`
