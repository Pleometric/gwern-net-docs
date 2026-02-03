---
sidebar_position: 1
---

# pre-commit.hook.php

**Path:** `build/pre-commit.hook.php` | **Language:** PHP | **Lines:** ~220

Git pre-commit hook that orchestrates the build pipeline for CSS, JavaScript, fonts, and icons.

## Overview

This script is the central orchestrator for gwern.net's asset build pipeline, triggered automatically before each git commit. It follows a dependency-aware build order, processing only **staged** changes since the last commit via `git diff-index --cached HEAD` (unless `--force` is specified). The script builds everything from raw source files to production-ready, versioned, and inlined assets.

The build proceeds in stages: first fonts and colors, then icons and versioned assets, then unified CSS/JS bundles, and finally the SSI-included HTML fragments that get injected into page templates. Each stage only runs if its source files have been modified, and successful builds automatically stage the generated files for commit.

At its core, `pre-commit.hook.php` is a declarative build system that maps source file patterns to build scripts, ensuring consistent ordering and dependency resolution across dozens of asset types.

## Key Functions/Variables

### Functions

- **`process_source_files($source_file_paths, $script_file_name)`**: Core build function that checks if any source files (or the build script itself) have changed using `git diff-index`, and if so, requires the build script and stages all updated files. Takes an array of source file paths and the name of a build script in `$build_dir`.

### Variables

- **`$force`**: Boolean flag set by `--force` CLI argument; when true, forces all build scripts to run regardless of git status.
- **`$build_dir`**: Directory containing all build scripts (from `build_paths.php`).
- **`$updated_files`**: Global array populated by build scripts; contains paths of newly generated files that need to be staged.
- **Font/CSS/Icon/Asset arrays**: Collections of source file paths organized by asset type (e.g., `$fonts_and_font_spec`, `$color_script_and_css_components`, `$icons`), constructed using glob patterns.

## Build Stages

The script processes assets in this order:

1. **Fonts** (`build_font_css.php`): Converts font spec + font files → `fonts-GENERATED.css`
2. **Versioned fonts** (`build_versioned_font_css.php`): Adds cache-busting versions to font CSS
3. **Color schemes** (`build_mode_css.php`): Builds light/dark mode CSS from color definitions
4. **Icon sprite** (`build_icon_sprite_file.php`): Combines individual SVGs → `icons.svg` sprite
5. **Asset versions** (`build_asset_versions.php`): Generates version database for JS-loaded assets
6. **Inlined images** (`build_inlined_images.php`): Converts small images to CSS variables
7. **Unified assets** (`build_unified_assets.php`): Concatenates JS/CSS into `head.js`, `script.js`, `head.css`, `style.css`
8. **Asset link versioning** (`version_asset_links.php`): Updates CSS references to use versioned asset URLs
9. **Head includes** (`build_head_includes.php`): Generates SSI fragment for `<head>` with inlined critical CSS
10. **Body includes** (`build_body_includes.php`): Generates SSI fragment for deferred CSS/JS loading
11. **Standalone includes** (`build_standalone_includes.php`): Generates SSI for standalone HTML files

## Input/Output

### Inputs

- **Font files**: `static/font/**/*.{otf,ttf}`, `static/font/font_spec.php`
- **CSS source files**: `static/css/*.css` (individual component stylesheets)
- **JavaScript source files**: `static/js/*.js` (individual modules)
- **Icon files**: `static/icon/*.svg`
- **Logo/dropcap assets**: `static/img/logo/**/*.{png,svg}`, `static/font/dropcap/**/*.{png,svg}`
- **Inlined images**: `static/img/{pattern,ornament}/**/*.*`
- **Templates**: `static/template/include/*.tmpl`
- **Build scripts**: All scripts in `build/` directory

### Outputs

All outputs are staged for commit via `git add`:

- **Generated CSS**: `fonts-GENERATED.css`, `head-GENERATED.css`, `style-GENERATED.css`, `light-mode-GENERATED.css`, `dark-mode-GENERATED.css`, `inlined-images-*.css`
- **Versioned CSS**: `fonts-VERSIONED.css`, `head-VERSIONED.css`, `style-VERSIONED.css`
- **Generated JS**: `head-GENERATED.js`, `script-GENERATED.js`, `asset-versions-GENERATED.js`
- **Icon sprite**: `static/icon/icons.svg`
- **SSI fragments**: `static/include/inlined-head.html`, `static/include/body-end.html`, `static/include/inlined-standalone.html`

## Usage

Installed as a git pre-commit hook:

```bash
# Automatic invocation (on git commit)
git commit -m "Update styles"

# Manual invocation with force rebuild
php build/pre-commit.hook.php --force
```

The hook is typically symlinked from `.git/hooks/pre-commit` to `build/pre-commit.hook.php`.

## See Also

- [sync.sh](/backend/sync-sh) - Main build orchestrator that calls this script
- [build_unified_assets.php](/php/build_unified_assets) - Unified CSS/JS concatenation
- [build_paths.php](/php/build_paths) - Directory structure and path constants
- [build_variables.php](/php/build_variables) - Shared build configuration
- [build_font_css.php](/php/build-font-css) - Font CSS generation
- [build_icon_sprite_file.php](/php/build-icon-sprite-file) - Icon sprite generation
