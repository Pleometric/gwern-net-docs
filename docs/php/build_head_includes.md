---
sidebar_position: 3
---

# build_head_includes.php

**Path:** `build/build_head_includes.php` | **Language:** PHP | **Lines:** ~91

Generates an inlined HTML include file for the document `<head>` with critical CSS, external JS tags, and preload hints.

## Overview

This script creates `inlined-head.html`, a server-side include (SSI) file that contains critical resources needed for initial page render. It inlines CSS directly into `<style>` tags and adds versioned `<link>`/`<script>` tags for external resources (including `head.js`).

The generated include file is designed to be embedded in HTML pages via Apache SSI directives (`<!--#include virtual="/static/include/inlined-head.html"-->`). This approach allows the same optimized head content to be shared across all pages without duplicating code in templates.

The script handles two types of includes: inlined CSS (from `.css` files) and external resource references (preload hints, external stylesheets/scripts) with automatic cache-busting versioning.

## Key Functions/Variables

### Include Configuration

- **`$includes`**: Array defining resources to include in the head. Each entry is either:
  - `[ 'filename.css', 'attributes' ]` - CSS file to inline with optional HTML attributes
- `[ '<element ...>', null ]` - Literal HTML tag for external resources (including JS)

### Include List

```php
[
    [ 'light-mode-GENERATED.css', 'id="inlined-styles-colors"' ],
    [ 'dark-mode-GENERATED.css', 'id="inlined-styles-colors-dark" media="all and (prefers-color-scheme: dark)"' ],
    [ '<link rel="stylesheet" href="/static/css/head.css">' ],
    [ '<script src="/static/js/head.js"></script>' ],
    [ '<link rel="preload" href="/static/img/icon/icons.svg" as="image">' ]
]
```

### Processing Logic

- **Type detection**: Regex checks determine if an entry is CSS (`.css`), JS (`.js`), or external HTML (`src=`/`href=`)
- **File path resolution**: Constructs absolute filesystem paths based on type (CSS → `$css_dir`, JS → `$js_dir`, external → parse from tag)
- **Inlining**: For CSS files, reads content with `file_get_contents()` and wraps in `<style>` tags
- **Versioning**: For external resources, calls `VersionedAssetHref()` to append `?v={timestamp}` query parameters

### Helper Function Usage

- **`VersionedAssetHref($file_name, $file_extension)`**: Generates versioned URLs with modification timestamps for cache busting (defined in `build_functions.php`)

## Input/Output

### Input Files

**Inlined CSS:**
- `/css/light-mode-GENERATED.css`
- `/css/dark-mode-GENERATED.css`

**External CSS/JS (versioned):**
- `/css/head.css` (or `head-GENERATED.css`, `head-VERSIONED.css` - `VersionedAssetHref()` checks variants)
- `/js/head.js` (or `head-GENERATED.js`, `head-VERSIONED.js`)

**Preload hints:**
- `/img/icon/icons.svg` (file existence checked for versioning)

### Output Files

- `/include/inlined-head.html` - Complete HTML include with inlined CSS and versioned external references

Example output structure:
```html
<style id="inlined-styles-colors">
/* light-mode-GENERATED.css contents */
</style>
<style id="inlined-styles-colors-dark" media="all and (prefers-color-scheme: dark)">
/* dark-mode-GENERATED.css contents */
</style>
<link rel="stylesheet" href="/static/css/head.css?v=1704672000">
<script src="/static/js/head.js?v=1704668400"></script>
<link rel="preload" href="/static/img/icon/icons.svg?v=1704665800" as="image">
```

## Usage

Invoked during the build process after CSS/JS generation:

```bash
php /path/to/build/build_head_includes.php
```

No command-line arguments required. The script:
1. Requires `build_paths.php`, `build_variables.php`, and `build_functions.php`
2. Iterates through `$includes` array
3. Inlines CSS file contents or versions external references
4. Writes complete include file to `/include/inlined-head.html`

**Server configuration:** Pages must use the `.shtml` extension and enable SSI processing:

```apache
# .htaccess (Apache)
Options +Includes
AddType text/html .shtml
AddOutputFilter INCLUDES .shtml
```

**Template usage:**
```html
<head>
    <!--#include virtual="/static/include/inlined-head.html"-->
    <!-- other head elements -->
</head>
```

---

## See Also

- [build_body_includes.php](/php/build_body_includes) - Generates body-end includes for deferred resources
- [build_standalone_includes.php](/php/build_standalone_includes) - Generates includes for standalone pages without JS
- [build_unified_assets.php](/php/build_unified_assets) - Creates the bundled CSS/JS files referenced here
- [build_functions.php](/php/build_functions) - Provides `VersionedAssetHref()` for cache busting
- [build_paths.php](/php/build_paths) - Directory path constants for file resolution
- [sync.sh](/backend/sync-sh) - Orchestrates the build pipeline
- [pre-commit-hook.php](/php/pre-commit-hook) - Git hook that triggers include regeneration
