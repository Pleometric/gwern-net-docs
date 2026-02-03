---
sidebar_position: 3
---

# gwsed.sh

**Path:** `build/gwsed.sh` | **Language:** Bash | **Lines:** ~75

Site-wide string replacement tool for maintaining and updating URLs across the gwern.net codebase.

---

## Overview

This script performs fixed-string (literal, non-regex) search-and-replace operations across the core gwern.net corpus: Markdown files, Haskell source code, GTX annotations, and HTML templates. JavaScript/CSS are not part of the replacement target set; they are only searched in the post-replacement `gw` check.

The script includes numerous safety checks and special-case handling: it validates input to prevent dangerous rewrites, automatically upgrades HTTP→HTTPS for entire domains (not just individual URLs), handles special anchor cases (like adding affiliation tags), and excludes problematic files like auto-generated code and temporary files.

After performing replacements, it runs a case-insensitive search to show any remaining variants, helping catch edge cases that might have been missed.

## Key Commands/Variables

- **`EXCLUDE`**: Base blacklist of files to never modify (temporary files, vendored code)
- **`EXCLUDE_SEARCH`**: Additional exclusions for search (auto-generated files, metadata)
- **`EXCLUDE_SEARCH_AND_REPLACE`**: Full exclusion list for replacements (specific Haskell/include files)
- **`stringReplace`**: Compiled Haskell utility for fast parallel string replacement
- **`gwhttp`**: Helper function for HTTP→HTTPS domain upgrades (defined in `bash.sh`)
- **`gw()`**: Post-replacement search function to find remaining variants
- **`FILES`**: List of files containing the search string

## Usage

```bash
./gwsed.sh <old-string> <new-string>
# or special case for W3C validator output:
./gwsed.sh <old-url> redirected to <new-url>
```

**Arguments:**
- `old-string`: Exact string to find (literal, not regex)
- `new-string`: Replacement string

**Safety checks:**
- Requires exactly 2 unique arguments
- Both strings must be single-line (no newlines)
- Blocks dangerous rewrites like `http://https://` → `https://`
- Exits with no action if no matches found

**Special cases:**

1. **HTTP→HTTPS domain upgrade**: If transforming `http://example.com/page` → `https://example.com/page`, automatically calls `gwhttp` to upgrade all links from that domain
   ```bash
   $ gwsed.sh "http://example.com/foo" "https://example.com/foo"
   # Upgrades ALL http://example.com/* links, not just /foo
   ```

2. **Affiliation anchor handling**: When adding affiliation tags (e.g., `#deepmind`), automatically deduplicates doubled anchors
   ```bash
   $ gwsed.sh "/doc/foo.pdf" "/doc/foo.pdf#deepmind"
   # Automatically fixes #deepmind#deepmind → #deepmind
   ```

3. **W3C validator redirects**: Handles validator output format
   ```bash
   $ gwsed.sh "http://old.com/page" redirected to "https://new.com/page"
   ```

**Examples:**
```bash
# Fix typo across entire site
$ gwsed.sh "recieve" "receive"

# Update moved URL
$ gwsed.sh "http://old-domain.com/article" "https://new-domain.com/article"

# Add affiliation to PDF
$ gwsed.sh "/doc/ai/2024-smith.pdf" "/doc/ai/2024-smith.pdf#anthropic"
```

**Exit codes:**
- `2`: Wrong number of arguments or multi-line input detected
- `3`: Unsafe rewrite pattern detected

**Dependencies:**
- `stringReplace`: Custom Haskell tool for parallel string replacement
- `grep`, `find`, `xargs`: Standard Unix utilities
- `cut`, `sort`, `wc`: Text processing tools

---

## See Also

- [sync.sh](/backend/sync-sh) - Build system that may trigger rewrites
- [StringReplace.hs](/backend/string-replace-hs) - Haskell utility for parallel string replacement
- [LinkArchive.hs](/backend/link-archive-hs) - Link localization system
- [upload.sh](/shell/upload) - File upload that may require URL updates
- [download-title.sh](/shell/download-title) - Title extraction for URL updates
- [bash.sh](/backend/bash-sh) - Common shell functions including `gwhttp` helper
