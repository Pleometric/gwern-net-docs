---
sidebar_position: 4
---

# collapse-checker.py

**Path:** `build/collapse-checker.py` | **Language:** Python | **Lines:** ~133

Detects HTML blocks with `collapse` class that are too small and should not be collapsed.

---

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

## See Also

- [collapse.js](/frontend/collapse-js) - Frontend JavaScript that handles collapse interactions
- [daterange-checker.py](/python/daterange-checker) - Similar LLM-based validation tool
- [htmlAttributesExtract.py](/python/htmlAttributesExtract) - HTML attribute extraction for validation
- [sync.sh](/backend/sync-sh) - Build orchestrator that runs validation checks
- [Annotation.hs](/backend/annotation-hs) - Backend annotation system generating collapse content
- [transclude.js](/frontend/transclude-js) - Transclusion system with lazy-loaded collapses
