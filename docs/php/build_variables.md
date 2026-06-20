---
title: "build_variables.php"
description: "This minimal file defines a single global array that acts as a registry of files modified during the build pipeline."
sidebar_position: 2
---

# build_variables.php

This minimal file defines a single global array that acts as a registry of files modified during the build pipeline.

<div className="doc-meta">
  <div><strong>Path</strong><code>build/build_variables.php</code></div>
  <div><strong>Language</strong>PHP</div>
  <div><strong>Lines</strong>5</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/build_variables.php">build/build_variables.php</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing PHP asset generation, build hooks, template assembly, or maintenance scripts around build_variables.
</div>

## Overview

This minimal file defines a single global array that acts as a registry of files modified during the build pipeline. It provides shared state that multiple build scripts can append to, allowing the build system to track which generated or versioned files were created or updated during a build run.

The `$updated_files` array is initialized empty and populated by scripts like `version_asset_links.php` when they write output files. This allows downstream scripts or logging mechanisms to report what changed, which is useful for debugging, caching invalidation, and build verification.

While simple, this shared state mechanism is important for coordinating between multiple PHP scripts that run in sequence during the build process. Each script can append to this array to communicate what it modified.

## Key Variables

- `$updated_files` - Array that accumulates paths of files written/updated during build

## Input/Output

**Input:** None (initialized empty)

**Output:** Provides global `$updated_files` array for other scripts to populate

## Usage

Imported by build scripts that generate files:

```php
require_once(__DIR__ . '/build_variables.php');
global $updated_files;

// After writing a file:
$updated_files[] = $versioned_file_path;
```

Used by:
- `version_asset_links.php` - Appends versioned CSS file paths after generation
- Potentially other build scripts that generate files and want to report what they created

The array can be inspected at the end of a build to log or display which files were updated.

---

<details className="generated-section">
<summary>See Also</summary>

- [build_paths.php](/php/build_paths) - Directory path constants (often imported together)
- [build_functions.php](/php/build_functions) - Utility functions (third shared module)
- [build_unified_assets.php](/php/build_unified_assets) - Main bundler that tracks file updates
- [version_asset_links.php](/php/version_asset_links) - Appends to `$updated_files`
- [build_asset_versions.php](/php/build_asset_versions) - Another script that tracks updated files
- [sync.sh](/backend/sync-sh) - Build orchestrator that may inspect updated files
</details>
