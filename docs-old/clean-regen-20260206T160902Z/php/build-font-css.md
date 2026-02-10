---
sidebar_position: 1
---

# build_font_css.php

**Path:** `build/build_font_css.php` | **Language:** PHP | **Lines:** ~225

Generates CSS `@font-face` declarations from a structured font specification file.

## Overview

This script is the primary font CSS generator for gwern.net. It reads a declarative font specification from `font_spec.php` and outputs two CSS files: a complete font stylesheet (`fonts-GENERATED.css`) and a subset containing only critical fonts for initial page load (`initial-fonts-GENERATED.css`).

The specification file uses a custom tab-delimited format that describes fonts with their weights, filenames, italic variants, and CSS properties like `font-display` and `unicode-range`. The script supports special `!inline` command blocks that mark certain font entries as critical for inlining into the initial CSS.

This is a foundational build step that runs early in the build process, as the generated CSS files are dependencies for versioning and inlining steps downstream.

## Key Functions/Variables

**`generate_entries($spec_block)`**
Parses a multi-line font specification block into an array of font entry objects. Each entry inherits base properties (name, base_path, format) and adds specific properties (weight, filename, italic variants).

**`construct_rule($entry, $italic = false)`**
Constructs a complete CSS `@font-face` rule from a font entry object. Handles italic path transformation via regex patterns specified in the spec file.

**`should_inline($candidate_entry, $italic = false)`**
Determines whether a font entry should be included in the initial CSS file by matching it against the global `$entries_to_inline` list populated by `!inline` command blocks.

**`process_command_block($command_block)`**
Processes special command blocks in the spec file (currently only `!inline`), which mark font entries that should be included in the critical initial CSS.

**`kv_tokenize($line)`**
Parses tab-delimited key-value pairs from spec file lines. Handles special cases like backtick-prefixed values and multi-part italic specifications.

**Global variables:**
- `$entries_to_inline`: Array of font entries marked for inclusion in initial CSS
- `$bare_fields`: Field names that can be inferred from position (`name`, `base_path`, `format`)
- `$formats`: Maps file extensions (`ttf`, `otf`) to CSS format strings (`truetype`, `opentype`)

## Input/Output

**Inputs:**
- `/static/font/font_spec.php` - Tab-delimited font specification file with metadata about each font family, weight, and variant

**Outputs:**
- `/css/fonts-GENERATED.css` - Complete CSS file with all font-face declarations
- `/css/initial-fonts-GENERATED.css` - Subset containing only fonts marked for initial page load

## Usage

Invoked directly by `sync.sh` during the CSS build phase:

```bash
php build/build_font_css.php
```

The script outputs progress to stdout and populates the `$updated_files` array for tracking by the build system.

---

## See Also

- [build_versioned_font_css.php](/php/build-versioned-font-css) - Adds cache-busting version parameters to the generated font CSS
- [font_spec.php](/php/font-spec) - Font specification database (tab-delimited DSL format)
- [build_unified_assets.php](/php/build_unified_assets) - Bundles the generated font CSS into main stylesheets
- [build_paths.php](/php/build_paths) - Directory path constants for font file locations
- [sync.sh](/backend/sync-sh) - Build orchestrator that invokes this script
- [initial.css](/css/initial) - References the generated font CSS for initial page load
