---
sidebar_position: 6
---

# htmlAttributesExtract.py

**Path:** `build/htmlAttributesExtract.py` | **Language:** Python | **Lines:** ~83

BeautifulSoup-based HTML parser that extracts all CSS classes, data-attributes, and IDs for whitelist validation.

---

## Overview

`htmlAttributesExtract.py` is a build-time quality assurance tool that extracts every CSS class, data-attribute key, and HTML ID from compiled HTML files. The output is designed to be piped through grep with a whitelist of known/expected values, catching typos, unused classes, and forgotten cleanup from development.

The tool serves a dual purpose:
1. **Error detection**: Catches typos like `class="collpase"` instead of `class="collapse"`
2. **Documentation**: The whitelist itself becomes living documentation of gwern.net's HTML/CSS architecture

The script is called in `sync.sh` during the build process. It checks both classes and data-attribute *keys* (not values, which vary too much). IDs are prefixed with `id:` to eliminate ambiguity when checking against class/data-attribute whitelists (an ID matching a class name is often an error).

## Key Functions

- **File validation**: Checks existence, readability, non-empty before processing
- **BeautifulSoup parsing**: Single-pass iteration over all HTML elements
- **Attribute extraction**:
  - CSS classes: Joined with spaces (as they appear in HTML)
  - Data-attributes: Keys only (e.g., `data-link-icon`, not its value)
  - IDs: Prefixed with `id:` for unambiguous grepping

## Command Line Usage

```bash
# Single file
python htmlAttributesExtract.py index.html
# Output:
# TOC
# abstract smallcaps-not dropcap-not
# data-filesize-bytes
# data-link-icon
# id:footnote-1
# id:introduction

# Multiple files
python htmlAttributesExtract.py *.html

# Build pipeline usage (in sync.sh)
find ./_site -name "*.html" | \
  xargs python htmlAttributesExtract.py | \
  sort -u | \
  grep -F -v -f .build/html-class-whitelist.txt

# If grep finds anything, those are unknown/unexpected attributes
```

**Output format:**
- One item per line
- CSS classes: Space-separated as they appear in HTML
- Data-attributes: Just the key (e.g., `data-popup-target`)
- IDs: Prefixed with `id:` (e.g., `id:main-content`)
- Sorted output for easy diffing

**Error handling:**
- Missing files: Exits with error message
- Unreadable files: Exits with permission error
- Empty files: Exits with empty file error
- Non-file arguments: Exits with "not a regular file" error

---

## See Also

- [sync.sh](/backend/sync-sh) - Build orchestrator that calls this script
- [rewrite.js](/frontend/rewrite-js) - Frontend code that applies many of these classes dynamically
- [collapse-checker.py](/python/collapse-checker) - Another HTML validation tool
- [daterange-checker.py](/python/daterange-checker) - Validates date-range markup
- [initial.css](/css/initial) - Stylesheet defining validated classes
- [default.css](/css/default) - Main stylesheet defining validated classes
