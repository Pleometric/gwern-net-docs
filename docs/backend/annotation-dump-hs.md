
# annotation-dump.hs

**Path:** `build/annotation-dump.hs` | **Language:** Haskell | **Lines:** ~69

> CLI utility for querying and grepping the annotation database

---

## Overview

`annotation-dump.hs` is a command-line utility for dumping and querying gwern.net's annotation database. It reads the four annotation GTX files (`me.gtx`, `full.gtx`, `half.gtx`, `auto.gtx`), merges them, and outputs single-line formatted entries suitable for grep-based searching. The tool serves two primary use cases: (1) dumping the entire annotation database for inspection, and (2) looking up specific URLs or finding all annotations that reference those URLs as substrings.

The tool formats each annotation on a single line with color-coded fields (source label, authors, year, URL, tags, title, abstract) separated by semicolons. This format enables pipeline-based workflows where you can extract links from a markdown file, pass them through `annotation-dump.hs`, and quickly identify which links lack annotations or tags.

A key feature is its handling of "empty" annotations: when queried for a URL with no metadata, it outputs the bare URL with `[]` to indicate missing tags, enabling workflows that identify untagged links for manual curation.

---

## Public API / CLI Usage

### Basic Usage

**Dump entire database:**
```bash
./annotation-dump.hs
```

Outputs all annotations in single-line format, sorted by path/date.

**Query specific URLs:**
```bash
echo "/doc/ai/2020-smith.pdf" | ./annotation-dump.hs
# Or with multiple URLs:
cat urls.txt | ./annotation-dump.hs
```

Returns:
1. Direct matches (the annotation for each queried URL)
2. Substring matches (any annotation containing the queried URL)
3. Empty markers (queried URLs with no annotation, shown as `URL []`)

### Output Format

Each line contains semicolon-separated fields:

```
<label>; <authors_cite>; <year>; <cite_key>; <colored_url>; <tags>; "<title>"; (<authors_truncated>); <date>; <abstract>; <generated_url>
```

Where:
- **label**: Source file (`m`=me.gtx, `f`=full.gtx, `h`=half.gtx, `a`=auto.gtx)
- **authors_cite**: Citation format (e.g., "Smith et al")
- **year**: First 4 chars of date
- **cite_key**: Citation key (usually author-year)
- **colored_url**: Green ANSI-colored URL
- **tags**: Array of tag strings
- **title**: Purple ANSI-colored quoted title
- **authors_truncated**: Author list (truncated with … if needed)
- **abstract**: Single-line abstract (newlines/extra spaces collapsed)
- **generated_url**: Optional canonical URL (green, if differs from path)

### Integration Patterns

**Find untagged links:**
```bash
./annotation-dump.hs | grep " \[\]"
```

**Check which links in a file need annotations:**
```bash
./link-extracter.hs doc.md | ./annotation-dump.hs | grep " \[\]"
```

**Search for specific topic:**
```bash
./annotation-dump.hs | grep -i "machine learning"
```

---

## Internal Architecture

### Data Flow

1. **Load GTX files**: Reads four GTX databases using `readGTXSlow`
2. **Merge databases**: Creates `incompleteDB` (raw union) and `finalDB` (blacklist-filtered, labeled)
3. **Sort and format**: Sorts by path/date, converts to single-line format
4. **Process input**:
   - If stdin empty → dump all formatted annotations
   - If stdin has URLs → lookup each + filter for substring matches

### Key Data Structures

```haskell
type Path = String
type MetadataItem = (Title, CiteKey, Date, ...)
type MetadataList = [(Path, MetadataItem)]
type Metadata = Map Path MetadataItem

-- Labeled metadata includes source file indicator
(Path, (MetadataItem, String))  -- String is "m"/"f"/"h"/"a"
```

### Database Hierarchy

Files merged with precedence (first wins):
1. `me.gtx` - Hand-curated definitions (highest priority)
2. `full.gtx` - Complete manually-written annotations
3. `half.gtx` - Tagged but not fully polished
4. `auto.gtx` - Auto-generated cached metadata (lowest priority)

### Blacklist Filter

The `blacklist` function excludes:
- Empty titles (`title==""`)
- Wikipedia pages (`en.wikipedia.org`)
- Index pages (`/doc/.../index`)

This prevents noise from auto-generated stub entries.

---

## Key Patterns

### Dual Database Strategy

The tool maintains two versions of the merged database:
- **incompleteDB**: Raw merge without filtering, used for lookups
- **finalDB**: Blacklist-filtered, used for full dumps

This ensures that direct URL queries return results even for blacklisted items, while full dumps exclude them.

### Empty Annotation Marker

When a queried URL has no metadata, the tool outputs:
```
/path/to/file []
```

The `[]` marker is intentionally chosen to match the tags field format, enabling consistent grep patterns for "untagged links" regardless of whether they have auto-titles.

### ANSI Color Coding

Output uses terminal colors for readability:
- **Green** (`\x1b[32m`): URLs and generated URLs
- **Purple** (`\x1b[35m`): Titles
- **Reset** (`\x1b[0m`): Ends color spans

### Substring Matching

When URLs are provided via stdin, the tool performs two types of lookups:
1. **Direct lookup**: `M.lookup url db` for exact match
2. **Substring filter**: `anyInfix stdin finalSingleLine` for cross-references

This enables discovering annotations that reference a URL (e.g., finding all annotations that cite a paper).

### Citation Formatting

Uses `authorsToCite` for smart author formatting:
- Single author: "Smith"
- Two authors: "Smith & Jones"
- Three+ authors: "Smith et al"

Truncates author lists with ellipsis if too long.

---

## Integration Points

### Dependencies

**Modules:**
- `GTX`: `readGTXSlow` for parsing GTX files
- `LinkID`: `authorsToCite`, `generateURL` for citation/URL generation
- `LinkMetadata`: `sortItemPathDate` for sorting
- `Tags`: `validateTagsSyntax` for tag validation
- `Metadata.Author`: `authorsTruncateString` for author truncation

**Data files:**
- `metadata/me.gtx`
- `metadata/full.gtx`
- `metadata/half.gtx`
- `metadata/auto.gtx`

### Common Workflows

**With link-extracter.hs:**
```bash
./link-extracter.hs doc.md | ./annotation-dump.hs
```

**With grep for quality control:**
```bash
./annotation-dump.hs | grep -E '(^\[\]|TODO|FIXME)'
```

**Audit specific source file:**
```bash
./annotation-dump.hs | grep "^m;" # Only manually-curated entries
```

---

## See Also

- [LinkMetadata.hs](/backend/link-metadata-hs) - Manages the annotation database
- [LinkMetadataTypes.hs](/backend/link-metadata-types-hs) - Core type definitions (MetadataItem, Path)
- [GTX.hs](/backend/gtx-hs) - GTX file format parser
- [Annotation.hs](/backend/annotation-hs) - Generates auto.gtx entries
- [Metadata/Author.hs](/backend/metadata-author-hs) - Author formatting utilities
- [Tags.hs](/backend/tags-hs) - Tag validation

---
