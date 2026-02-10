---
sidebar_position: 2
---

# deconstruct_singlefile.php

**Path:** `build/deconstruct_singlefile.php` | **Language:** PHP | **Lines:** ~600

Extracts base64-encoded assets from SingleFile HTML archives into separate files, with optional gwtar self-extracting archive creation.

## Overview

This utility script converts "single-file" HTML archives (where all images, fonts, videos, and other assets are embedded as base64 data URIs) into normal HTML with external asset references. It is commonly used when archiving web pages with tools like SingleFile browser extension, which embeds everything into one .html file for portability.

The script scans the HTML for `data:` URIs, decodes the base64 data, determines the appropriate file extension from the MIME type, writes each asset to a separate file in an adjacent directory, and rewrites the HTML to reference the external files. It also validates extracted images using ImageMagick's `identify` command, reverting to base64 if the image is corrupted.

**Major features:**
- **PNG→JPEG optimization**: Automatically converts PNGs to JPEG when PSNR quality loss is acceptable and file size reduction exceeds 30%
- **Image compression**: Integrates with `compressGIF`, `compressPNG`, `compressJPG` tools for lossless/lossy optimization
- **Gwtar archive creation**: Can package everything into a self-extracting `.gwtar.html` archive with SHA-256 integrity verification
- **PAR2 error correction**: Optional forward error correction (25% redundancy) for long-term archival resilience

As a final step, the script adds `loading="lazy" decoding="async"` attributes to all `<img>` tags for better performance.

---

## Command Line Options

```bash
php deconstruct_singlefile.php foo.html [options]
```

| Option | Default | Description |
|--------|---------|-------------|
| `--memory-limit=SIZE` | `1024M` | PHP memory limit for large files |
| `--backtrack-limit=N` | `5000000` | PCRE backtrack limit for regex on large files |
| `--jpg-quality=N%` | `80%` | JPEG quality when converting from PNG |
| `--optimize-images=1` | `true` | Run compression tools if available |
| `--save-original=1` | `true` | Keep original SingleFile with `.bak` extension |
| `--create-gwtar=0` | `false` | Create self-extracting `.gwtar.html` archive |
| `--add-fec-data=1` | `true` | Append PAR2 parity files (requires `--create-gwtar`) |
| `--keep-original=1` | `true` | Retain original `.html` file when creating gwtar |
| `--debug=0` | `false` | Print diagnostic messages |

Boolean arguments can be set to true by passing without a value: `--optimize-images`.

---

## Key Functions/Variables

### Functions

- **`file_force_contents($path, $contents)`**: Writes file contents to a path, creating intermediate directories if needed.

### Variables

- **`$gwtar_version_string`**: Current gwtar format version (`v1`)
- **`$asset_type_map`**: Large associative array mapping MIME types to file extensions (~60 mappings covering image, audio, video, and font formats)
- **`$image_file_extensions`**: Array of image extensions that get ImageMagick validation/optimization
- **`$assets`**: Manifest array tracking each asset's size, content-type, basename, and SHA-256 hash

### Regular Expressions

- **`/([\'"]?)data:([a-z0-9-+\.\/]+?);base64,([A-Za-z0-9+\/=]+)(\1)/`**: Matches base64 data URIs with optional quotes, capturing MIME type and encoded data

---

## Input/Output

### Inputs

- **HTML file**: SingleFile HTML archive with embedded base64 data URIs
- **Dependencies**: `php-cli` (with `pcre`, `json`), ImageMagick (`identify`, `convert`, `compare`), `tar`
- **Optional**: `par2create` (for FEC), Gwern.net utilities (`compressGIF`/`compressPNG`/`compressJPG`)
- **Required for gwtar**: `gwtar.js`, `gwtar_noscript.html` in script directory

### Outputs

**Standard mode:**
- **Modified HTML file**: Original HTML overwritten with external asset references
- **Asset directory**: `{basename}/` containing extracted assets
- **Asset files**: `{basename}-asset-{N}.{ext}` sequentially numbered

**Gwtar mode (`--create-gwtar=1`):**
- **Self-extracting archive**: `{basename}.gwtar.html` - opens as normal HTML page with embedded JavaScript decoder
- **Contents**: ustar tarball appended after HTML, with SHA-256 manifest
- **PAR2 parity**: Optional 25% redundancy for forward error correction

Example transformation:
```
foo.html → foo.html (modified) + foo/foo-asset-1.png + foo/foo-asset-2.jpg + ...
# or with gwtar:
foo.html → foo.gwtar.html (self-extracting)
```

---

## Processing Pipeline

### Asset Extraction

1. **Parse data URIs**: Regex finds all `data:[type];base64,[data]` patterns
2. **Decode base64**: Convert base64 string to binary data
3. **Determine extension**: Look up MIME type in `$asset_type_map`, default to `.dat`
4. **Write asset file**: Save to `{dir}/{basename}-asset-{N}.{ext}`
5. **Validate images**: Run `identify`; if corrupt, delete and keep base64
6. **Rewrite HTML**: Replace data URI with relative path

### PNG→JPEG Conversion

For each PNG asset:
1. Create test JPEG at 15% quality
2. Measure PSNR quality difference using ImageMagick `compare -metric PSNR`
3. Measure file size reduction percentage
4. If PSNR > 31dB AND size reduction > 30%: convert at `--jpg-quality`
5. Delete original PNG, update asset manifest

### Image Optimization

If `--optimize-images=1` and tools are available:
- GIFs → `compressGIF` (gifsicle optimization)
- PNGs → `compressPNG` (optipng/pngnq/advpng)
- JPEGs → `compressJPG` (jpegtran/mozjpeg)

### Gwtar Creation

1. Calculate HTML file size and SHA-256 hash
2. Sort assets (JavaScript first for faster loading)
3. Create ustar tarball of all assets
4. Embed `gwtar.js` decoder and `gwtar_noscript.html` warning
5. Generate JSON manifest with hashes
6. Append tarball after HTML marker
7. Optionally append PAR2 parity data

---

## Gwtar Format

The gwtar format (`--create-gwtar=1`) creates a self-extracting HTML archive for long-term preservation:

- **Browser-viewable**: Opens as normal HTML page with JavaScript-based extractor
- **Standard tooling**: Extractable with `tail -c +OFFSET | tar xf -`
- **Integrity verification**: SHA-256 hashes in manifest
- **Error correction**: Optional PAR2 parity for recovery from partial corruption

See [gwern.net/gwtar](https://gwern.net/gwtar) for detailed design rationale.

---

## Usage Examples

```bash
# Basic extraction with default settings
php deconstruct_singlefile.php archive.html

# With increased memory for large files
php deconstruct_singlefile.php --memory-limit=2048M large-archive.html

# Create gwtar archive with FEC
php deconstruct_singlefile.php --create-gwtar archive.html

# Disable image optimization for speed
php deconstruct_singlefile.php --optimize-images=0 archive.html

# Debug mode to see PNG conversion decisions
php deconstruct_singlefile.php --debug archive.html
```

---

## See Also

- [LinkArchive.hs](/backend/link-archive-hs) - Main archiving system that produces SingleFile archives
- [linkArchive.sh](/shell/link-archive) - Shell script that invokes this PHP tool for large files
- [Config.LinkArchive](/backend/config-link-archive-hs) - Archive configuration and URL transforms
- [sync.sh](/backend/sync-sh) - Build orchestrator that may invoke this script
- [hakyll.hs](/backend/hakyll-hs) - Build system that coordinates archiving
- [gwtar.js](/frontend/gwtar-js) - Client-side JavaScript decoder for gwtar archives
