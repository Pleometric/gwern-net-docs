---
sidebar_position: 2
---

# png.sh

**Path:** `build/png.sh` | **Language:** Bash | **Lines:** ~41

Aggressive PNG image optimization using lossy compression with quality gating.

---

## Overview

`png.sh` performs two-stage PNG optimization: lossy quantization followed by lossless compression. Unlike typical image optimization that only uses lossless techniques (optipng, pngcrush), this script uses `pngnq` to reduce color depth (neural-net quantization to 256 colors) before applying lossless optimization with `optipng`.

The script implements a quality gate: it only replaces the original file if the optimized version is at least 10% smaller. This prevents unnecessary churn from marginal gains and avoids cases where lossy quantization might actually increase file size due to compression artifacts.

Files and directories can be processed in parallel using GNU Parallel, making it efficient for batch optimization of entire image directories. This is typically run as part of the upload workflow or as a periodic maintenance task to reduce bandwidth and storage costs.

## Key Commands/Variables

**Core Function - `pngOne()`:**
```bash
pngOne() {
    orig_size=$(stat --printf="%s" "$1")     # Get original size in bytes
    pngnq -v -s1 "$temp_file"                # Lossy quantization (256 colors)
    optipng -o9 "$optimized_file"            # Lossless optimization (max level)
    diff=$(echo "scale=2; 100*(($orig_size - $new_size)/$orig_size)" | bc)
    if (( $(echo "$diff >= 10" | bc -l) )); then
        mv "$optimized_file" "$1"            # Replace only if 10%+ smaller
    fi
}
```

**Key Tools:**
- `pngnq` - Neural-net quantizer for lossy color reduction (256-color palette)
- `optipng -o9` - Lossless PNG optimizer (optimization level 9, slowest/best)
- `nice -n 19` - Run at lowest CPU priority to avoid blocking other work
- `parallel` - GNU Parallel for concurrent processing

**Quality Gate:**
- **Threshold**: 10% minimum size reduction required
- **Calculation**: `100 * ((orig_size - new_size) / orig_size)`
- **Rationale**: Avoid git churn for marginal gains, prevent size increases

**File Handling:**
- `temp_file=$(mktemp --suffix=.png)` - Avoid overwriting (pngnq bug on Ubuntu)
- `"${temp_file%.*}"-nq8.png` - pngnq output naming convention
- Cleanup: `rm -f "$temp_file" "$optimized_file"` after decision

## Usage

**Single file:**
```bash
./png.sh image.png
```

**Multiple files:**
```bash
./png.sh img1.png img2.png img3.png
# Processes in parallel via GNU Parallel
```

**Directory recursion:**
```bash
./png.sh ./images/
# Finds all *.png files in directory tree, processes via parallel
```

**Invocation patterns:**
- Single files: processed in background (`pngOne "$item" &`)
- Directories: `find "$item" -type f -name "*.png" | sort | parallel pngOne`

**Output:**
```
Optimized file of foo.png is not at least 10% smaller (45230 → 43891 ; reduction: 2.96%). Leaving the original file untouched.
```

**Integration with upload workflow:**
```bash
# Typically called from upload.sh or build pipeline
./png.sh ~/wiki/doc/images/
```

**Error handling:**
- `set -euo pipefail` - Fail fast on errors
- Non-PNG files: skipped with message "File X is not a PNG. Skipping."
- Missing optimized file: original preserved with warning

**Performance notes:**
- `nice -n 19` prevents CPU starvation of interactive processes
- GNU Parallel automatically manages CPU cores
- Lossy quantization can take seconds per image
- Optimization level 9 is very slow but maximizes savings

## See Also

- [Image.hs](/backend/image-hs) - Server-side image processing (dimensions, inversion)
- [upload.sh](/shell/upload) - File upload script that may call PNG optimization
- [sync.sh](/backend/sync-sh) - Build orchestrator that runs image optimization
- [should_image_have_outline.php](/php/should-image-have-outline) - Image outline detection
- [build_inlined_images.php](/php/build-inlined-images) - Inlines images into CSS
- [invertornot.py](/python/invertornot) - Image inversion classification
- [image-focus.js](/frontend/image-focus-js) - Client-side image lightbox
