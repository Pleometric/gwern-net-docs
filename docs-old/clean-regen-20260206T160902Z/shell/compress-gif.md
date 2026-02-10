# compressGIF

**Path:** `build/compressGIF` | **Language:** Bash | **Lines:** ~65

Lossless GIF compression script using gifsicle with intelligent file replacement.

---

## Overview

compressGIF optimizes GIF files using the `gifsicle` tool, but only replaces the original when the savings are worthwhile (>10% reduction). This threshold prevents unnecessary file churn—gifsicle always modifies file metadata even when no real optimization occurs, which would create spurious git changes.

The script processes files in parallel using GNU `parallel` for efficient batch optimization.

---

## Usage

```bash
# Compress specific files
./compressGIF image1.gif image2.gif

# Compress auto-detected images in current directory (jpg/jpeg/png/webp/avif)
./compressGIF

# Process from a list
find . -name "*.gif" | xargs ./compressGIF
```

**Arguments:**
- File paths to compress (optional)
- If no arguments, searches current directory for `*.jpg`, `*.jpeg`, `*.png`, `*.webp`, `*.avif` (note: does **not** auto-discover `.gif`)

---

## How It Works

### Optimization Process

```
Input GIF
    ↓
Create temp file
    ↓
gifsicle --colors=256 --optimize=3
    ↓
Compare sizes
    ↓
┌─────────────────────────┐
│ Size reduction >= 10%?  │
└─────────────────────────┘
    ↓ Yes           ↓ No
Replace original    Delete temp
    ↓               (keep original)
Done
```

### The 10% Threshold

The script calculates:
```bash
size_delta = original_size - optimized_size
min_reduction = original_size * 0.1
```

Only if `size_delta >= min_reduction` does it replace the original file.

**Why this matters:**
- gifsicle always changes metadata, even with no real optimization
- Without the threshold, every GIF would show as modified in git
- The 10% threshold ensures only meaningful optimizations are kept

---

## Key Functions

### optimize_gif()

Core optimization function applied to each file:

```bash
optimize_gif() {
    local gif="$1"

    # Skip if file doesn't exist
    if [ ! -f "$gif" ]; then return; fi

    # Create temp file and optimize
    temp_gif="$(mktemp /tmp/XXXXXX.gif)"
    gifsicle --colors=256 --optimize=3 "$gif" > "$temp_gif"

    # Compare sizes
    original_size="$(stat --printf="%s" "$gif")"
    optimized_size="$(stat --printf="%s" "$temp_gif")"

    # Replace only if >10% savings
    size_delta="$((original_size - optimized_size))"
    min_reduction="$(echo "scale=0; $original_size * 0.1 / 1" | bc)"

    if [ "$size_delta" -ge "$min_reduction" ]; then
        mv "$temp_gif" "$gif"
        echo "Optimized $gif"
    else
        rm "$temp_gif"
    fi
}
```

### get_image_files()

Auto-discovers image files when no arguments provided (jpg/jpeg/png/webp/avif only):

```bash
find . -maxdepth 1 -type f \
    \( -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.png" \
       -o -iname "*.webp" -o -iname "*.avif" \) \
    | sort --version-sort
```

---

## Gifsicle Options

| Option | Purpose |
|--------|---------|
| `--colors=256` | Reduce to 256 colors (GIF maximum) |
| `--optimize=3` | Maximum optimization level |

The `-O3` optimization includes:
- Cross-frame optimization
- LZW compression tuning
- Transparency optimization

---

## Dependencies

- **gifsicle**: GIF manipulation tool
- **parallel**: GNU parallel for batch processing
- **bc**: Calculator for percentage math
- **stat**: File size information

Install on Debian/Ubuntu:
```bash
apt-get install gifsicle parallel bc
```

---

## Helper Functions (Inlined)

The script inlines common helpers for portability:

```bash
red()   { echo -e "\e[41m$@\e[0m"; }   # Red background text
green() { echo -e "\e[32m$@\e[0m"; }   # Green text
```

---

## Error Handling

- Exits immediately on error (`set -e`)
- Checks for required commands before processing
- Gracefully handles non-existent files in `optimize_gif()`
- Reports when no arguments are available

---

## Known Issue

The script comment references a gifsicle behavior:

> NOTE: this also avoids the issue where `gifsicle` *always* changes the file metadata by clobbering the original, even when no real change was made (which is a WONTFIX by the maintainer: https://github.com/kohler/gifsicle/issues/201)

This is why the threshold-based replacement is essential—without it, every GIF would appear modified in version control.

---

## See Also

- [compressPNG](/shell/compress-png) - Similar script for PNG compression
- [sync.sh](/backend/sync-sh) - Main build script that may call image optimizers
- [Image.hs](/backend/image-hs) - Haskell image processing utilities
