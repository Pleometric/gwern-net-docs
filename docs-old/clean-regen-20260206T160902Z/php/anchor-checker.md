---
sidebar_position: 6
---

# anchor-checker.php

**Path:** `build/anchor-checker.php` | **Language:** PHP | **Lines:** ~173

Validates local fragment identifiers in HTML files to detect broken internal links.

## Overview

This quality assurance tool scans HTML files to find broken local anchors (fragment identifiers like `#section-name`). It parses each HTML file with PHP's DOMDocument, collects all valid anchor targets (elements with `id` or `name` attributes), then checks every `<a href="...">` and `<area href="...">` link to ensure local fragment references point to existing targets.

The script is designed for build-time validation, returning a non-zero exit code when problems are found. This allows it to be integrated into CI/CD pipelines to prevent deployment of pages with broken internal navigation. It's particularly valuable for large sites like gwern.net where anchors are used extensively for section links, footnotes, sidenotes, and cross-references within documents.

The checker only validates anchors local to each document (those starting with `#` and no filename prefix). Cross-document anchors like `other-page.html#section` are intentionally ignored to avoid false positives, as they require filesystem crawling or a site map.

## Key Functions

### `main(array $files): never`

Entry point that orchestrates validation across multiple files.

**Behavior:**
1. Validates file list is non-empty
2. Checks each file for readability
3. Calls `check_file()` for each valid file
4. Collects files with no links (potential errors)
5. Prints bad anchors to stderr in `filename<TAB>anchor` format
6. Exits with code 0 (success) or 1 (errors found)

**Exit codes:**
- `0` - All anchors valid
- `1` - Bad anchors found, or all files had no links
- `2` - File read error
- `3` - HTML parse error

### `check_file(string $file): CheckResult`

Reads and parses a single HTML file.

**Behavior:**
1. Reads file contents
2. Strips `<wbr>` tags (HTML5 elements that trip up parser)
3. Parses with `DOMDocument::loadHTML()`, suppressing warnings
4. Returns `CheckResult` object

**Special handling:**
- Empty files return `CheckResult([], 0)` (valid but no content)
- Parse errors exit with code 3

### `check_document(DOMDocument $dom): CheckResult`

Performs anchor validation on parsed DOM.

**Algorithm:**
1. **Collect anchor targets** using XPath:
   - All elements with `id` attribute → `#id` valid
   - All `name` attributes on `<a>`, `<map>`, `<area>`, `<img>` → `#name` valid
   - Special targets: `#` (top of page) and `#top` always valid
2. **Find all links**:
   - XPath query: `//a/@href | //area/@href`
3. **Check each link**:
   - Skip non-local links (not starting with `#`)
   - URL-decode the fragment (handles `%20`, etc.)
   - Check if fragment exists in target set
   - Collect missing fragments as bad anchors
4. Return unique bad anchors and total href count

**Data structures:**
- `$id_set` - Associative array used as a set of valid `#fragment` strings
- `$bad_anchors` - List of fragment identifiers with no matching target

### `CheckResult` Class

Simple value object returned by checking functions:
- `$bad_anchors` (array) - List of invalid fragment identifiers found
- `$href_count` (int) - Total number of `<a href>` and `<area href>` elements

## Input/Output

**Input:**
- One or more HTML file paths as command-line arguments
- Files must be readable and contain valid HTML

**Output:**
- **Stderr**: Error messages in format `filename<TAB>#fragment`
- **Exit code**: 0 for success, 1 for validation errors

**Example output:**
```
dir/page.html	#missing-section
dir/page.html	#undefined-footnote
dir/other.html	#nonexistent-anchor
```

## Usage

Run from command line:

```bash
# Check single file:
php anchor-checker.php output/page.html

# Check multiple files:
php anchor-checker.php output/*.html

# Integrate into build:
find ./output -name "*.html" -print0 | xargs -0 php build/anchor-checker.php
if [ $? -ne 0 ]; then
    echo "Anchor validation failed!"
    exit 1
fi
```

Typical integration in `sync.sh` or Makefile:

```bash
# After HTML generation:
hakyll build

# Validate anchors before deployment:
php build/anchor-checker.php $(find _site -name "*.html") 2> anchor-errors.txt
if [ -s anchor-errors.txt ]; then
    echo "Found broken anchors:"
    cat anchor-errors.txt
    exit 1
fi
```

### Requirements

- PHP 8.1+ (uses readonly properties, constructor property promotion)
- PHP CLI SAPI: `sudo apt install php-cli`
- DOM extension (usually built-in)

### Edge Cases Handled

- **HTML5 elements**: Strips `<wbr>` tags that cause parse errors
- **Empty files**: Considered valid (no anchors to break)
- **No links in file**: Warning if ALL files lack links, otherwise just noted
- **URL encoding**: Decodes `%20` and similar before checking
- **Duplicate bad anchors**: De-duplicated in output via `array_unique()`
- **Non-standard name attributes**: Accepts `name` on various elements for compatibility

## See Also

- [hakyll.hs](/backend/hakyll-hs) - Site generator that produces HTML files to validate
- [sync.sh](/backend/sync-sh) - Build orchestrator that invokes anchor checking
- [markdown-lint.sh](/shell/markdown-lint) - Markdown linting that complements anchor checking
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation system that creates internal links
- [LinkBacklink.hs](/backend/link-backlink-hs) - Backlink system creating anchor targets
