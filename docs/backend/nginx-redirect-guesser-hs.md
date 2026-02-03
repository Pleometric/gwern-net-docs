
# nginxredirectguesser.hs

**Path:** `build/nginxredirectguesser.hs` | **Language:** Haskell | **Lines:** ~160

> Semi-automated nginx redirect rule generator for fixing 404 errors

---

## Overview

This utility assists in maintaining gwern.net's large nginx redirect configuration (~30k rules). When 404 errors are logged, this tool processes the broken URLs and suggests appropriate redirect rules by finding the closest matching existing redirects or files.

The approach is pragmatic rather than sophisticated: instead of machine learning or complex pattern matching, it uses Levenshtein distance to find similar URLs in the existing redirect corpus. About half of the generated rules are usable directly; the rest require manual review. This reflects the reality that URL errors are varied and ad-hoc—past attempts at clever regexes often backfired.

The tool reads broken URLs from stdin, consults the existing redirect maps (`static/nginx/redirect/move.conf`, `static/nginx/redirect/broken.conf`) and the site's actual file tree, then emits new redirect rules sorted by confidence (failures first, then by edit distance).

---

## Public API

### `main :: IO ()`

Entry point. Reads broken URLs from stdin, loads existing redirect rules and file database, computes best matches, and prints suggested nginx rules to stdout.

**Usage:**
```bash
xclip -o | sort -u | runghc -istatic/build/ static/build/nginxredirectguesser.hs
```

**Calls:** `listFilesRecursivelyWithBasename`, `diffAndRank`, `mkPattern`, `escapeRegex`

---

## Internal Architecture

### Data Flow

```
stdin (broken URLs)
        ↓
   strip + dedupe
        ↓
┌───────┴────────┐
│  Load files    │  Load existing redirects
│  from C.root   │  from redirect map files
└───────┬────────┘
        ↓
   diffAndRank per URL
        ↓
   bestMatch filtering (distance ≤ 2)
        ↓
   mkRule formatting
        ↓
   dedupe + filter existing
        ↓
stdout (nginx rules)
```

### Key Data Structures

**File database:** `[(FilePath, FilePath)]` — pairs of (basename, full-site-path) for exact-match lookups.

**Redirect corpus:** `[(String, String)]` — pairs of (source-pattern, destination) parsed from existing nginx configs. Patterns are cleaned by removing regex metacharacters for fuzzy matching.

**Match result:** `(Int, String, String)` — tuple of (edit-distance, matched-source, destination).

### Matching Algorithm

1. **Exact basename match:** If the broken URL's filename exists in the site, return it immediately with distance 0
2. **Fuzzy match:** Calculate Levenshtein distance between the broken URL and all existing redirect sources
3. **Threshold:** Only accept matches with distance ≤ `minDistance` (2)

---

## Key Patterns

### Regex Escaping for nginx

The `escapeRegex` function handles PCRE metacharacters, producing patterns like:

```
/doc/foo.pdf  →  "~^/doc/foo\.pdf.*$"
/page?q=x     →  "~^/page\?q=x$"
```

PDF URLs get special treatment: the `.pdf` extension is followed by `.*$` to match query strings and fragments.

### Corpus Cleaning for Fuzzy Matching

When comparing URLs, regex metacharacters are stripped from the existing redirect sources:

```haskell
filter (`notElem` ("~^.*?+[]\"" :: String)) source
```

This allows meaningful edit-distance comparisons despite the regex syntax in the original rules.

### Output Deduplication

Results are deduplicated in order (first occurrence wins) and filtered against existing rules to avoid generating redundant entries:

```haskell
nubOrd . filter (`S.notMember` existingLines) $ generated
```

---

## Configuration

### Constants

| Name | Value | Purpose |
|------|-------|---------|
| `minDistance` | 2 | Maximum Levenshtein distance for a match to be considered valid |

### File Paths

- `nginx/redirect/move.conf` — Primary redirect rules
- `nginx/redirect/broken.conf` — Rules for clearly broken/malicious URLs
- `C.root` — Site root directory for file enumeration

---

## Integration Points

### Inputs

- **stdin:** Newline-separated list of broken URLs (typically from error logs)
- **Existing configs:** Reads both nginx redirect configuration files
- **File system:** Enumerates all files under site root (excluding `.git`, `_cache`, `_site`)

### Outputs

- **stdout:** nginx redirect rules in the format:
  ```
  "~^/broken/url$" "/correct/destination";
  "~^/unknown/url$" "";
  ```

### Dependencies

- `Config.Misc` — For `C.root` (site root path)
- `Utils` — For `replaceChecked`, `replaceMany`
- `Text.EditDistance` — Levenshtein distance calculation (external package)

---

## Output Format

Rules are sorted with unmatched URLs first (destination `""`), then by edit distance, then alphabetically:

```nginx
# No match found — needs manual investigation
"~^/completely/unknown/path$" "";

# Close match found — suggested redirect
"~^/doc/typo-in-filename\.pdf.*$" "/doc/correct-filename.pdf";
```

The empty destination (`""`) signals to the reviewer that this URL needs manual attention or should be added to the "broken" list.

---

## See Also

- [404-guesser.js](/frontend/404-guesser-js) - Client-side counterpart using same Levenshtein algorithm
- [Config.Misc](/backend/config-misc-hs) - Provides site root path constant
- [Utils.hs](/backend/utils-hs) - Provides `replaceChecked` and `replaceMany` helpers
- [sync.sh](/backend/sync-sh) - Build orchestrator that may invoke this during maintenance
- [rename.hs](/backend/rename-hs) - Related tool that generates redirect rules during page renames
- [gwern.net.conf](/nginx/gwern-net-conf) - Nginx configuration file where redirects are stored
