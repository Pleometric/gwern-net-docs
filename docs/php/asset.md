---
sidebar_position: 7
---

# asset.php

**Path:** `asset.php` | **Language:** PHP | **Lines:** ~22

Development-only asset serving script with automatic rebuild on file changes.

## Overview

`asset.php` is a PHP script designed exclusively for development environments that serves static assets (JavaScript and CSS files) while automatically triggering the build system's pre-commit hook when Git detects changes. This creates a live-reload-like experience during development without requiring manual rebuild commands.

The script operates as a dynamic asset server that intercepts requests for `.js` and `.css` files, runs the pre-commit build hook if there are uncommitted changes, and then serves the requested file with the appropriate MIME type. This workflow enables developers to see their changes reflected immediately when refreshing the browser.

**CRITICAL WARNING**: This script is explicitly marked for development use only and should never be deployed to production. It executes Git commands and shell operations that could pose security risks in a production environment.

## Key Functions/Variables

### Core Variables

- **`$static_dir`**: Set to `__DIR__`, represents the base directory for asset resolution
- **`$file_name`**: Retrieved from the `f` query parameter (`$_GET['f']`), specifies which file to serve
- **`$content_type`**: MIME type for the response, defaults to `text/plain` with fallbacks for `.js` (application/javascript) and `.css` (text/css)

### Workflow Steps

1. **Content Type Detection**: Determines MIME type based on file extension using `str_ends_with()`
2. **Header Setting**: Sends appropriate Content-Type header with UTF-8 charset
3. **Build Trigger**:
   - Runs `git diff` to check for uncommitted changes
   - If changes exist, stages all files (`git add .`) and runs `/build/pre-commit.hook.php`
   - Logs output to `asset.log` using `tee -a`
   - Removes Git index lock file to prevent lock conflicts
4. **File Serving**: Uses `file_get_contents($file_name)` to read and output the requested file

### Shell Commands Executed

```php
`git diff`
`git add . ; ./build/pre-commit.hook.php | tee -a asset.log`
`rm .git/index.lock`
```

These backtick-wrapped commands execute shell operations directly, which is why this script is development-only.

## Usage

This script is typically invoked by the development web server (like PHP's built-in server) when assets are requested. The URL pattern would be:

```
http://localhost:8000/asset.php?f=static/js/extracts.js
http://localhost:8000/asset.php?f=static/css/default.css
```

### Development Server Setup

Likely used with Apache/Nginx rewrite rules or PHP's built-in server router configuration to intercept asset requests:

```bash
# Example: PHP built-in server
php -S localhost:8000 asset.php
```

### Build Integration

The script automatically triggers `/build/pre-commit.hook.php`, which likely performs:
- JavaScript/CSS minification
- Asset compilation
- Linting or validation checks
- Other pre-commit transformations

This ensures that served assets are always built with the latest changes, mimicking the production build pipeline.

### Log Output

Build operations are logged to `asset.log` in the script's directory, useful for debugging build failures during development.

## Security Considerations

**DO NOT USE IN PRODUCTION**:
- Executes arbitrary shell commands
- No input validation on `$file_name` (potential directory traversal vulnerability)
- No authentication or authorization
- Automatically stages Git changes
- Could expose sensitive repository information

---

## See Also

- [pre-commit-hook.php](/php/pre-commit-hook) - The build hook triggered by this script
- [build_unified_assets.php](/php/build_unified_assets) - Asset compilation system
- [build_asset_versions.php](/php/build_asset_versions) - Asset versioning for cache busting
- [build_functions.php](/php/build_functions) - Utility functions for versioning
- [sync.sh](/backend/sync-sh) - Production build orchestrator (contrast with dev workflow)
- [initial.js](/frontend/initial-js) - Example frontend module being served and rebuilt
