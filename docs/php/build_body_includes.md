---
sidebar_position: 4
---

# build_body_includes.php

**Path:** `build/build_body_includes.php` | **Language:** PHP | **Lines:** ~53

Generates versioned `<link>` and `<script>` tags for deferred CSS and JavaScript loaded at document end.

## Overview

This script creates `inlined-asset-links.html`, a server-side include (SSI) file containing `<link>` and `<script>` tags for the main CSS and JavaScript bundles. Unlike the head includes (which inline critical resources), this file references external assets that are loaded asynchronously after initial page render.

The script automatically appends cache-busting version parameters (`?v={timestamp}`) to asset URLs using the `VersionedAssetHref()` helper function. This ensures browsers fetch updated versions when files change while allowing aggressive long-term caching.

The CSS `<link>` tag uses the `media="print" onload="this.media='all'"` pattern to defer CSS loading without blocking render, and the JS `<script>` tag uses the `defer` attribute for non-blocking execution.

## Key Functions/Variables

### Include Configuration

- **`$includes`**: Array of HTML tags for deferred resources:
  ```php
  [
      '<link rel="stylesheet" href="/static/css/style.css" media="print" onload="this.media=`all`">',
      '<script src="/static/js/script.js" defer></script>'
  ]
  ```

### Processing Logic

- **URL extraction**: Regex `/(src|href)="\/static\/(.+?)(\.[^\.\/]+?)"/i` parses asset paths from HTML tags
- **Versioning**: Calls `VersionedAssetHref($m[2], $m[3])` to generate timestamped URLs
- **URL replacement**: Regex `/(src|href)="(.+?)"/` replaces original URLs with versioned ones

### Helper Function Usage

- **`VersionedAssetHref($file_name, $file_extension)`**: Checks for `-VERSIONED`, `-GENERATED`, or base file variants, returns quoted URL with `?v={timestamp}` parameter

## Input/Output

### Input Files

The script references (but does not directly read) these bundled assets:
- `/css/style.css` (or `style-GENERATED.css`, `style-VERSIONED.css` - checked by `VersionedAssetHref()`)
- `/js/script.js` (or `script-GENERATED.js`, `script-VERSIONED.js`)

**Note:** The actual files are created by `build_unified_assets.php`. This script only generates the HTML tags that reference them with versioned URLs.

### Output Files

- `/include/inlined-asset-links.html` - HTML include with versioned asset references

Example output:
```html
<link rel="stylesheet" href="/static/css/style.css?v=1704672000" media="print" onload="this.media=`all`">
<script src="/static/js/script.js?v=1704668400" defer></script>
```

## Usage

Invoked during the build process after asset bundling:

```bash
php /path/to/build/build_body_includes.php
```

No command-line arguments required. The script:
1. Requires `build_paths.php`, `build_variables.php`, and `build_functions.php`
2. Parses asset URLs from `$includes` array
3. Calls `VersionedAssetHref()` to generate timestamped URLs
4. Writes versioned HTML tags to `/include/inlined-asset-links.html`

**Server configuration:** Pages must use the `.shtml` extension and enable SSI processing:

```apache
# .htaccess (Apache)
Options +Includes
AddType text/html .shtml
AddOutputFilter INCLUDES .shtml
```

**Template usage:**
```html
<body>
    <!-- page content -->
    <!--#include virtual="/static/include/inlined-asset-links.html"-->
</body>
```

---

## See Also

- [build_unified_assets.php](/php/build_unified_assets) - Creates the `style.css` and `script.js` bundles referenced here
- [build_head_includes.php](/php/build_head_includes) - Generates head includes for critical inlined resources
- [build_standalone_includes.php](/php/build_standalone_includes) - Generates includes for standalone pages
- [build_asset_versions.php](/php/build_asset_versions) - Creates asset version manifest for frontend cache busting
- [build_functions.php](/php/build_functions) - Provides `VersionedAssetHref()` for cache-busting URL generation
- [sync.sh](/backend/sync-sh) - Orchestrates the build pipeline
- [pre-commit-hook.php](/php/pre-commit-hook) - Git hook that triggers include regeneration
