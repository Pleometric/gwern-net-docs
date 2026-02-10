---
sidebar_position: 4
---

# upload.sh

**Path:** `build/upload.sh` | **Language:** Bash | **Lines:** ~279

Comprehensive file upload manager with automatic optimization, naming, and deployment.

---

## Overview

`upload.sh` is the primary interface for adding files to gwern.net, handling everything from file naming and format conversion to optimization, metadata extraction, and deployment. It serves as a quality gate ensuring all uploaded content follows site conventions: lowercase filenames, standardized extensions, globally unique names, and optimized formats.

The script distinguishes between two upload modes: **temporary uploads** (one argument) go to `/doc/www/misc/`, while **permanent uploads** (two arguments) are placed in topic-specific directories like `/doc/statistics/decision/`. Files undergo format-specific processing: PDFs are OCR'd and compressed, images are metadata-stripped and checked for optimization opportunities, and text files are reformatted for readability. (No automatic deletion is scheduled in the script.)

A key feature is the large-file strategy: files over 200MB are uploaded normally but added to `.gitignore` to avoid bloating the Git repository, providing the benefits of Git-based deployment without the overhead of Git-LFS or Git-annex. The script integrates with the broader gwern.net infrastructure by calling external tools (compressPdf, crossref, cloudflare-expire) and opening uploaded files in a browser for verification.

## Key Commands/Variables

**Main Function - `_upload()`:**

**Filename normalization:**
```bash
FILENAME="$(echo "$1" | tr '[:upper:]' '[:lower:]' | sed -e 's/\.jpeg$/.jpg/')"
# Always lowercase, standardize .jpeg → .jpg
```

**Extension validation:**
```bash
ALLOWED_EXTENSIONS=$(find ~/wiki/ -type f -printf '%f\n' | awk -F. 'NF>1 {print $NF}' | sort -iu)
# Reject any extension never used before (prevents typos/malformed files)
```

**Global uniqueness check:**
```bash
rename_file() {
  for ((i=2; i<=100; i++)); do
    new_filename="${base_name}-${i}.${extension}"
    # Loop until unique: 2023-liu.pdf → 2023-liu-2.pdf → 2023-liu-3.pdf ...
  done
}
```

**Format conversions:**
- `.md` → `.txt` (avoid compiling random Markdown snippets)
- `.jpeg` → `.jpg` (enforce consistency)
- `.webp` → `.png` (avoid exotic formats)

**PDF handling:**
```bash
compressPdf "$TARGET" || true  # JBIG2 compression, OCR, PDF/A conversion
METADATA=$(crossref "$TARGET") && echo "$METADATA" &  # Extract DOI metadata
```

**Large file strategy:**
```bash
SIZE_THRESHOLD=200000000  # 200MB
if [[ "$FILESIZE" -gt "$SIZE_THRESHOLD" ]]; then
  echo "$TARGET" >> ./.gitignore  # Skip git versioning
  bold "Added large file to .gitignore (size: ...)"
fi
```

**Image optimization:**
```bash
exiftool -overwrite_original -All="" "$FILENAME"  # Strip metadata from temp images
png2JPGQualityCheck ~/wiki/"$TARGET"  # Suggest PNG→JPG if appropriate
```

**Deployment:**
```bash
rsync --chmod='a+r' --mkpath -q "$TARGET" gwern@176.9.41.242:"/home/gwern/gwern.net/$TARGET_DIR/"
cloudflare-expire "$TARGET_DIR/$(basename "$FILE")" > /dev/null  # Clear CDN cache
"$BROWSER" "$URL" 2> /dev/null &  # Open for verification
```

## Usage

**Temporary upload (no automatic expiration in script):**
```bash
./upload.sh screenshot.png
# → https://gwern.net/doc/www/misc/2026-01-07-screenshot.png
# Automatically prefixed with today's date
```

**Permanent upload with topic directory:**
```bash
./upload.sh 2023-smith.pdf statistics
# → /doc/statistics/2023-smith.pdf
# → https://gwern.net/doc/statistics/2023-smith.pdf
```

**Automatic filename rewriting:**
```bash
./upload.sh smith2023.pdf reinforcement-learning
# Automatically renamed: smith2023.pdf → 2023-smith.pdf
# → /doc/reinforcement-learning/2023-smith.pdf
```

**Tag-based directory guessing:**
```bash
./upload.sh paper.pdf "deep learning"
# Calls: ./static/build/guessTag "deep learning"
# → Might resolve to /doc/ai/ or /doc/neural-network/
```

**Multiple file upload:**
```bash
./upload.sh *.pdf statistics/decision
# Last argument (if not a file) = directory
# All files uploaded to that directory
```

**Large file handling:**
```bash
./upload.sh large-dataset.pkl embeddings
# File size: 450MB
# → Uploaded to /doc/embeddings/
# → Added to .gitignore automatically
# → Not tracked in git history
```

**Workflow example:**
```bash
# Download a paper
wget https://arxiv.org/pdf/2301.12345.pdf -O transformer-paper.pdf

# Upload to appropriate topic directory
./upload.sh transformer-paper.pdf ai

# Script automatically:
# 1. Renames to match year-author.pdf pattern
# 2. Runs OCR if needed
# 3. Compresses with JBIG2
# 4. Extracts metadata via CrossRef
# 5. Deploys to server
# 6. Opens in browser for verification
```

**Environment variables:**
- `BROWSER` - Prefers Firefox if running, else Chromium/Chrome/Brave

**Exit codes:**
- `1` - File missing/empty
- `2` - Missing required dependencies
- `3` - Unsupported file extension
- `4` - Could not change to wiki directory (temp upload path)
- `5` - Failed to find unique filename after 100 attempts
- `6` - Rename failure after uniqueness attempt
- `8` - Target directory invalid and guess failed
- `9` - Could not change to wiki directory (permanent upload path)
- `10` - File already exists at exact target path

## Key Dependencies

**Required external tools:**
- `firefox`/`chromium` - Browser for verification
- `exiftool` - Metadata stripping
- `rsync` - Server upload
- `curl` - URL validation
- `git` - Version control
- `compressPdf` - PDF optimization wrapper
- `cloudflare-expire` - CDN cache invalidation
- `png2JPGQualityCheck` - Image format recommendation
- `convert` (ImageMagick) - WebP conversion
- `locate` - Filename search
- `crossref` - DOI metadata extraction
- `guessTag` - Tag-to-directory mapper

**Sourced utilities:**
```bash
. ~/wiki/static/build/bash.sh  # Common functions (red, bold, etc.)
```

---

## See Also

- [sync.sh](/backend/sync-sh) - Build orchestrator that may trigger bulk uploads
- [gwsed.sh](/shell/gwsed) - Site-wide string replacement for updating URLs
- [download-title.sh](/shell/download-title) - Title extraction for uploaded files
- [embed.sh](/shell/embed) - Embedding generation for uploaded documents
- [LinkMetadata.hs](/backend/link-metadata-hs) - Stores metadata for uploaded papers
- [Annotation.hs](/backend/annotation-hs) - Annotation system that references uploaded files
- [clean-pdf.py](/python/clean-pdf) - PDF text cleaning for uploaded documents
