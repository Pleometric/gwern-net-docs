---
sidebar_position: 2
---

# deconstruct_singlefile.php

**Path:** `build/deconstruct_singlefile.php` | **Language:** PHP | **Lines:** ~199

Extracts base64-encoded assets from single-file HTML archives, converting them to external files.

## Overview

This utility script converts "single-file" HTML archives (where all images, fonts, videos, and other assets are embedded as base64 data URIs) into normal HTML with external asset references. This is commonly used when archiving web pages with tools like SingleFile browser extension, which embeds everything into one .html file for portability.

The script scans the HTML for `data:` URIs, decodes the base64 data, determines the appropriate file extension from the MIME type, writes each asset to a separate file in an adjacent directory, and rewrites the HTML to reference the external files. It also validates extracted images using ImageMagick's `identify` command, reverting to base64 if the image is corrupted.

As a final step, the script adds `loading="lazy" decoding="async"` attributes to all `<img>` tags for better performance.

## Key Functions/Variables

### Functions

- **`file_force_contents($path, $contents)`**: Writes file contents to a path, creating intermediate directories if needed. Used to ensure asset output directories exist before writing extracted files.

### Variables

- **`$memory_limit`**: Configurable PHP memory limit (default 1024M); can be overridden via `-m` or `--memory-limit` flags to handle very large HTML files.
- **`$input_file_path`**: Path to the input HTML file containing base64-encoded assets.
- **`$asset_directory`**: Output directory for extracted assets, created adjacent to the input file with the same basename (e.g., `foo.html` → `foo/` directory).
- **`$asset_base_name`**: Base name of the input file without extension, used to name the asset directory.
- **`$asset_type_map`**: Large associative array mapping MIME types to file extensions (e.g., `'image/png' => 'png'`, `'video/mp4' => 'mp4'`). Covers dozens of image, audio, video, and font formats.
- **`$image_file_extensions`**: Array of image extensions (`bmp`, `gif`, `jpg`, `png`, etc.) that should be validated with ImageMagick.
- **`$asset_count`**: Counter for generating unique asset filenames (`-asset-1.png`, `-asset-2.jpg`, etc.).

### Regular Expressions

- **`/([\'"]?)data:([a-z0-9-+\.\/]+?);base64,([A-Za-z0-9+\/=]+)(\1)/`**: Matches base64 data URIs with optional quotes, capturing MIME type and encoded data.

## Input/Output

### Inputs

- **HTML file**: Single-file HTML archive with embedded base64 data URIs (typically from SingleFile or similar archiving tools)
- Arguments: `file_path` (required), `-m`/`--memory-limit` (optional)

### Outputs

- **Modified HTML file**: Original HTML file overwritten with external asset references
- **Asset directory**: Directory named `{basename}/` containing all extracted assets
- **Asset files**: Individual files named `{basename}-asset-{N}.{ext}` where N is a sequential counter

Example transformation:
```
foo.html → foo.html (modified) + foo/foo-asset-1.png + foo/foo-asset-2.jpg + ...
```

## Asset Extraction Process

1. **Parse data URIs**: Regex finds all `data:[type];base64,[data]` patterns
2. **Decode base64**: Convert base64 string to binary data
3. **Determine extension**: Look up MIME type in `$asset_type_map`, default to `.dat` if unknown
4. **Write asset file**: Save to `{dir}/{basename}-asset-{N}.{ext}`
5. **Validate images**: Run `identify` on image files; if it fails (corrupt image), delete the file and keep the base64 version
6. **Rewrite HTML**: Replace data URI with relative path like `{basename}/{basename}-asset-{N}.{ext}`

## Image Validation

The script uses ImageMagick's `identify` command to verify image integrity:

```bash
identify $asset_path 2>&1
```

If the output contains "error", the asset file is deleted and the original base64 data is preserved. This prevents broken images in the final HTML.

## Usage

```bash
# Basic usage with default 1024MB memory
php deconstruct_singlefile.php archive.html

# With custom memory limit
php deconstruct_singlefile.php -m 2048M large-archive.html
php deconstruct_singlefile.php --memory-limit 2048M large-archive.html
```

The script modifies the HTML file in-place and creates an adjacent directory for assets.

## See Also

- [LinkArchive.hs](/backend/link-archive-hs) - Main archiving system that produces SingleFile archives
- [linkArchive.sh](/shell/link-archive) - Shell script that invokes this PHP tool for large files
- [Config.LinkArchive](/backend/config-link-archive-hs) - Archive configuration and URL transforms
- [sync.sh](/backend/sync-sh) - Build orchestrator that may invoke this script
- [hakyll.hs](/backend/hakyll-hs) - Build system that coordinates archiving
