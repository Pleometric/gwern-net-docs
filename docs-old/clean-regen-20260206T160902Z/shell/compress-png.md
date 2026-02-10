# compressPNG

**Path:** `build/compressPNG` | **Language:** Bash | **Lines:** ~79

Lossy and lossless PNG compression using pngnq and optipng with intelligent replacement.

---

## Overview

compressPNG optimizes PNG files through a two-stage process: first lossy color quantization with `pngnq`, then lossless compression with `optipng`. Like `compressGIF`, it only replaces the original when savings exceed 10%, avoiding unnecessary file churn.

The script processes files in parallel and handles both individual files and directories.

---

## Usage

```bash
# Compress specific files
./compressPNG image1.png image2.png

# Compress all image files in current directory
./compressPNG

# Process entire directory recursively
./compressPNG /path/to/images/

# Mixed files and directories
./compressPNG file.png directory/
```

**Arguments:**
- File paths and/or directory paths (optional)
- If no arguments, searches current directory for image files

---

## How It Works

### Optimization Pipeline

```
Input PNG
    ↓
Copy to temp file
    ↓
pngnq -v -s1 (lossy quantization → *-nq8.png)
    ↓
optipng -o9 (lossless compression)
    ↓
Compare sizes
    ↓
┌─────────────────────────┐
│ Size reduction >= 10%?  │
└─────────────────────────┘
    ↓ Yes           ↓ No
Replace original    Keep original
    ↓               (delete temp)
Done
```

### Two-Stage Compression

1. **pngnq** (lossy): Reduces PNG to 8-bit palette (256 colors max)
   - `-v`: Verbose output
   - `-s1`: Sampling rate for color selection
   - Creates file with `-nq8.png` suffix

2. **optipng** (lossless): Maximum compression of the quantized image
   - `-o9`: Highest optimization level (slowest but smallest)

---

## Key Functions

### pngOne()

Core optimization function for individual files:

```bash
pngOne() {
    if [[ "$1" =~ .*\.png ]]; then
        orig_size=$(stat --printf="%s" "$1")
        temp_file=$(mktemp --suffix=.png)
        cp "$1" "$temp_file"

        # WARNING: pngnq on Ubuntu is buggy, will delete on overwrite
        nice --adjustment=19 pngnq -v -s1 "$temp_file"
        optimized_file="${temp_file%.*}"-nq8.png

        if [ -f "$optimized_file" ]; then
            nice --adjustment=19 optipng -o9 "$optimized_file"
            new_size=$(stat --printf="%s" "$optimized_file")

            # Calculate percentage difference
            diff=$(echo "scale=2; 100*(($orig_size - $new_size)/$orig_size)" | bc)

            # Replace only if 10%+ smaller
            if (( $(echo "$diff >= 10" | bc --mathlib) )); then
                mv "$optimized_file" "$1"
            else
                green "Not at least 10% smaller ($orig_size → $new_size). Keeping original."
            fi
        fi

        rm --force -- "$temp_file" "$optimized_file"
    else
        green "File $1 is not a PNG. Skipping."
    fi
}
```

### Processing Logic

```bash
for ITEM in "${args[@]}"; do
    if [ -f "$ITEM" ]; then
        # Single file: process in background
        pngOne "$ITEM" &
    elif [ -d "$ITEM" ]; then
        # Directory: find all PNGs, process in parallel
        find "$ITEM" -type f -name "*.png" | sort | parallel --jobs "$N" -- pngOne
    fi
done
```

---

## Configuration

| Variable | Default | Purpose |
|----------|---------|---------|
| `N` | 14 | Max parallel jobs for directory processing |

---

## Dependencies

- **pngnq**: PNG color quantizer (lossy)
- **optipng**: PNG optimizer (lossless)
- **parallel**: GNU parallel for batch processing
- **bc**: Calculator for percentage math

Install on Debian/Ubuntu:
```bash
apt-get install pngnq optipng parallel
```

---

## Important Notes

### pngnq Bug Warning

```bash
# WARNING: can't overwrite on Ubuntu as pngnq is buggy
# and will simply delete the file!
```

The script works around this by:
1. Copying to a temp file first
2. Running pngnq on the temp file (creates new `-nq8.png`)
3. Moving result back to original if optimization is worthwhile

### Nice Priority

Both compression tools run with lowest CPU priority:
```bash
nice --adjustment=19 pngnq ...
nice --adjustment=19 optipng ...
```

This prevents the compression from starving other processes.

### File Type Check

Only processes `.png` files, skips others with a message:
```bash
if [[ "$1" =~ .*\.png ]]; then
    # process
else
    green "File $1 is not a PNG. Skipping."
fi
```

---

## Comparison with compressGIF

| Feature | compressPNG | compressGIF |
|---------|-------------|-------------|
| Compression | Lossy + Lossless | Lossless only |
| Tools | pngnq + optipng | gifsicle |
| Threshold | 10% | 10% |
| Directory support | Yes (recursive) | No (flat only) |
| Parallelism | Background + parallel | parallel only |

---

## See Also

- [compressGIF](/shell/compress-gif) - Similar script for GIF compression
- [sync.sh](/backend/sync-sh) - Main build script that may call image optimizers
- [Image.hs](/backend/image-hs) - Haskell image processing utilities
