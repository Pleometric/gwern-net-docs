---
sidebar_position: 5
---

# build_standalone_includes.php

**Path:** `build/build_standalone_includes.php` | **Language:** PHP | **Lines:** ~88

Generates an inlined HTML include for standalone pages with minimal CSS and no JavaScript.

## Overview

This script creates `inlined-standalone.html`, a server-side include (SSI) file for standalone pages that don't require the full interactive JavaScript framework. These pages (typically error pages, simple static pages, or fallback views) need basic styling but not the popups, annotations, or dynamic features of the main site.

The include contains inlined light-mode CSS and external `<link>` tags for the head and main CSS bundles (with cache-busting versioning). Notably absent compared to `build_head_includes.php`: no JavaScript, no dark-mode CSS (since JS is required for the theme switcher), and no preload hints.

This approach allows standalone pages to render quickly with minimal resources while maintaining visual consistency with the main site.

## Key Functions/Variables

### Include Configuration

- **`$includes`**: Array defining resources for standalone pages:
  ```php
  [
      [ 'light-mode-GENERATED.css', 'id="inlined-styles-colors"' ],
      [ '<link rel="stylesheet" href="/static/css/head.css">' ],
      [ '<link rel="stylesheet" href="/static/css/style.css">' ]
  ]
  ```

### Processing Logic

- **Type detection**: Regex checks determine if an entry is CSS (`.css`), JS (`.js`), or external HTML (`src=`/`href=`)
- **File path resolution**: Constructs absolute filesystem paths based on type (CSS → `$css_dir`, JS → `$js_dir`, external → parse from tag)
- **Inlining**: For CSS files, reads content with `file_get_contents()` and wraps in `<style>` tags with optional attributes
- **Versioning**: For external resources, calls `VersionedAssetHref()` to append `?v={timestamp}` query parameters

### Differences from build_head_includes.php

- **No JavaScript**: No `<script>` tags or inlined JS
- **No dark mode CSS**: Only light mode colors (dark mode requires JS for switching)
- **No preload hints**: Minimal resource hints for faster initial load
- **Direct CSS links**: References CSS bundles directly instead of bundling everything inline

## Input/Output

### Input Files

**Inlined CSS:**
- `/css/light-mode-GENERATED.css`

**External CSS (versioned):**
- `/css/head.css` (or `head-GENERATED.css`, `head-VERSIONED.css` - checked by `VersionedAssetHref()`)
- `/css/style.css` (or `style-GENERATED.css`, `style-VERSIONED.css`)

### Output Files

- `/include/inlined-standalone.html` - HTML include with inlined light-mode CSS and versioned external CSS links

Example output structure:
```html
<style id="inlined-styles-colors">
/* light-mode-GENERATED.css contents */
</style>
<link rel="stylesheet" href="/static/css/head.css?v=1704672000">
<link rel="stylesheet" href="/static/css/style.css?v=1704668400">
```

## Usage

Invoked during the build process after CSS generation:

```bash
php /path/to/build/build_standalone_includes.php
```

No command-line arguments required. The script:
1. Requires `build_paths.php`, `build_variables.php`, and `build_functions.php`
2. Iterates through `$includes` array
3. Inlines CSS file contents or versions external references
4. Writes complete include file to `/include/inlined-standalone.html`

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
    <!--#include virtual="/static/include/inlined-standalone.html"-->
    <title>Error 404</title>
</head>
```

**Use cases:**
- Error pages (404, 500)
- Maintenance pages
- Minimal fallback views
- Static utility pages without interactive features

---

## See Also

- [build_head_includes.php](/php/build_head_includes) - Full head includes with JS and dark mode for interactive pages
- [build_body_includes.php](/php/build_body_includes) - Body-end includes for deferred JS (not used on standalone pages)
- [build_unified_assets.php](/php/build_unified_assets) - Creates the CSS bundles referenced here
- [build_functions.php](/php/build_functions) - Provides `VersionedAssetHref()` for cache busting
- [build_paths.php](/php/build_paths) - Directory path constants for file resolution
- [sync.sh](/backend/sync-sh) - Orchestrates the build pipeline
- [pre-commit-hook.php](/php/pre-commit-hook) - Git hook that triggers include regeneration
