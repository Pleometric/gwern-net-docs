---
title: "htmlAttributesExtract.py"
description: "htmlAttributesExtract.py is a build-time quality assurance tool that extracts every CSS class, data-attribute key, and HTML ID from compiled HTML files."
sidebar_position: 6
---

# htmlAttributesExtract.py

htmlAttributesExtract.py is a build-time quality assurance tool that extracts every CSS class, data-attribute key, and HTML ID from compiled HTML files.

<div className="doc-meta">
  <div><strong>Path</strong><code>build/htmlAttributesExtract.py</code></div>
  <div><strong>Language</strong>Python</div>
  <div><strong>Lines</strong>83</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/htmlAttributesExtract.py">build/htmlAttributesExtract.py</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing Python helper scripts for metadata cleanup, text processing, PDFs, dates, or generated content around htmlAttributesExtract.
</div>

## Overview

`htmlAttributesExtract.py` is a build-time quality assurance tool that extracts every CSS class, data-attribute key, and HTML ID from compiled HTML files. The output is designed to be piped through grep with a whitelist of known/expected values, catching typos, unused classes, and forgotten cleanup from development.

The tool serves a dual purpose:
1. **Error detection**: Catches typos like `class="collpase"` instead of `class="collapse"`
2. **Documentation**: The whitelist itself becomes living documentation of gwern.net's HTML/CSS architecture

The script is called in `sync.sh` during the build process. It checks both classes and data-attribute *keys* (not values, which vary too much). ID extraction code exists (with `id:` prefixes) but is currently commented out, so IDs are not emitted.

## Key Functions

- **File validation**: Checks existence, readability, non-empty before processing
- **BeautifulSoup parsing**: Single-pass iteration over all HTML elements
- **Attribute extraction**:
  - CSS classes: Joined with spaces (as they appear in HTML)
  - Data-attributes: Keys only (e.g., `data-link-icon`, not its value)
  - IDs: Collected but output currently disabled (commented out in source)

## Command Line Usage

```bash
# Single file
python htmlAttributesExtract.py index.html
# Output:
# TOC
# abstract smallcaps-not dropcap-not
# data-filesize-bytes
# data-link-icon

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
- Sorted output for easy diffing

**Note:** ID extraction code exists but is currently disabled (commented out in source). When enabled, IDs would be prefixed with `id:` for unambiguous grepping.

**Error handling:**
- Missing files: Exits with error message
- Unreadable files: Exits with permission error
- Empty files: Exits with empty file error
- Non-file arguments: Exits with "not a regular file" error

---

<details className="generated-section">
<summary>See Also</summary>

- [sync.sh](/backend/sync-sh) - Build orchestrator that calls this script
- [rewrite.js](/frontend/rewrite-js) - Frontend code that applies many of these classes dynamically
- [collapse-checker.py](/python/collapse-checker) - Another HTML validation tool
- [daterange-checker.py](/python/daterange-checker) - Validates date-range markup
- [initial.css](/css/initial) - Stylesheet defining validated classes
- [default.css](/css/default) - Main stylesheet defining validated classes
</details>
