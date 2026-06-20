---
title: "collapse-checker.py"
description: "Gwern.net uses \"collapses\" (disclosure widgets similar to ) to manage page complexity and allow readers to opt into levels of detail."
sidebar_position: 4
---

# collapse-checker.py

Gwern.net uses "collapses" (disclosure widgets similar to ) to manage page complexity and allow readers to opt into levels of detail.

<div className="doc-meta">
  <div><strong>Path</strong><code>build/collapse-checker.py</code></div>
  <div><strong>Language</strong>Python</div>
  <div><strong>Lines</strong>133</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/collapse-checker.py">build/collapse-checker.py</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing Python helper scripts for metadata cleanup, text processing, PDFs, dates, or generated content around collapse-checker.
</div>

## Overview

Gwern.net uses "collapses" (disclosure widgets similar to `<details>`) to manage page complexity and allow readers to opt into levels of detail. Content inside a `.collapse` element is initially hidden and can be revealed on hover or click. This is particularly useful for handling transclusions, which are lazy-loaded only when the collapse is expanded.

However, collapses can be overused. Collapsing small items like 2-3 item lists creates visual clutter and cognitive burden without meaningful benefit. This script uses BeautifulSoup and heuristics to parse Gwern.net-style HTML, identify collapsed blocks, estimate their "content volume," and warn if they're too small to justify collapse.

The script is used during the site build process to check all essays and annotations, printing filenames of pages with undersized collapses for manual review and fixing.

## Key Functions

- **`read_and_parse_html(filename)`**: Reads HTML file and returns BeautifulSoup object
- **`has_excluded_class(element, exclude_classes)`**: Checks if element or any parent has an excluded class (backlinks, similars, etc.)
- **`check_for_incorrect_collapse_usage(soup, filename)`**: Main validation logic that calculates content volume and prints warnings
- **Content volume heuristic**: Counts paragraphs, list items, figures (weighted 3×), images (weighted 2×), and direct children
- **Threshold**: Warns if content volume ≤ 6 units

**Excluded elements** (always allowed to be collapsed):
- Headings: `h1` through `h6`
- Sections and spans
- Code blocks: `code`, `pre`
- Figures and images
- Links

**Excluded classes** (special contexts where small collapses are acceptable):
- `.backlinks-append`
- `.similars-append`
- `.link-bibliography-append`
- `.aux-links-transclude-file`

## Command Line Usage

```bash
# Check single file
python collapse-checker.py /path/to/file.html

# Check multiple files
python collapse-checker.py file1.html file2.html file3.html

# Typical build usage (check all annotations)
find ./metadata/annotation/ -name "*.html" -exec python collapse-checker.py {} \;

# URL-encoded filenames are decoded for easier reference
python collapse-checker.py './metadata/annotation/https%3A%2F%2Fexample.com%2Fpage.html'
# Output: ./metadata/annotation/https://example.com/page.html
```

**Output:**
- Prints decoded filename for each file with problematic collapses
- No output means the file passed validation
- Can be used in CI/CD to fail builds with undersized collapses

---

<details className="generated-section">
<summary>See Also</summary>

- [collapse.js](/frontend/collapse-js) - Frontend JavaScript that handles collapse interactions
- [daterange-checker.py](/python/daterange-checker) - Similar LLM-based validation tool
- [htmlAttributesExtract.py](/python/htmlAttributesExtract) - HTML attribute extraction for validation
- [sync.sh](/backend/sync-sh) - Build orchestrator that runs validation checks
- [Annotation.hs](/backend/annotation-hs) - Backend annotation system generating collapse content
- [transclude.js](/frontend/transclude-js) - Transclusion system with lazy-loaded collapses
</details>
