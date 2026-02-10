
# Columns.hs

**Path:** `build/Columns.hs` | **Language:** Haskell | **Lines:** ~99

> Detects "skinny tall" lists suitable for CSS multi-column rendering

---

## Overview

Columns.hs is a command-line lint tool that scans Markdown and HTML files for lists that would benefit from multi-column CSS layout. On gwern.net, lists wrapped in `<div class="columns">` are rendered in two columns, which works well for long lists of short items (like link lists or bibliographies) but looks awkward for short lists or lists with long items.

The module defines a "skinny tall" list as one where all items are under 75 characters wide—narrow enough to fit side-by-side—meaning multi-column layout would be beneficial. The tool parses files into Pandoc AST, extracts all ordered and bullet lists, measures the longest item in each, and reports lists that fall under the threshold.

Files that already contain `<div class="columns"` are skipped entirely, on the assumption they've been manually reviewed and all appropriate lists have already been wrapped.

---

## Public API

### `main :: IO ()`

Entry point. Processes command-line arguments and scans each file for candidate lists.

**Called by:** sync.sh (via `runghc`)
**Calls:** `printLists`, `getLongLists`

### `listLength :: Block -> Int`

Returns the length of the longest item in a list block (measured as rendered plain text). For non-list blocks, returns `maxBound` so they're filtered out.

**Called by:** `listsTooLong`, GenerateSimilar.hs
**Calls:** `listLengthLongest`, `listItemLength`

### `listsTooLong :: Int -> [Block] -> [Block]`

Filters a list of blocks to return only lists whose longest item is shorter than the threshold.

**Called by:** `getLongLists`
**Calls:** `extractLists`, `listLength`

---

## Internal Architecture

### Data Flow

```
File → Pandoc Parser → AST → extractLists → filter by listLength → simplified output
```

1. **Parse**: File content is read and parsed as HTML or Markdown (determined by `.html` suffix) using Pandoc with full extension set
2. **Extract**: `queryWith` traverses the AST to collect all `BulletList` and `OrderedList` blocks
3. **Measure**: For each list, compute the maximum item length by:
   - For each list item (a `[Block]`), find the longest sub-block
   - For each sub-block, render it to plain text via `simplified` and measure character length
4. **Filter**: Keep only lists where `listLength < threshold`
5. **Report**: Print lists that pass (i.e., are skinny enough for columns)

### Key Types

```haskell
-- List items are nested: [Block](Block) where outer list is items, inner is paragraphs within an item
listLengthLongest :: [Block](Block) -> Int
listItemLength    :: [Block] -> Int
listSubItemLength :: Block -> Int
```

---

## Key Patterns

### Plain Text Measurement via `simplified`

Rather than computing character widths from raw AST (which would require handling links, emphasis, etc.), the module renders each block to plain text using Pandoc's `writePlain` and measures that:

```haskell
listSubItemLength :: Block -> Int
listSubItemLength = T.length . simplified
```

This gives accurate "visual width" that accounts for link text vs URL length, stripped formatting, etc.

### Early Exit for Pre-Annotated Files

Files containing `<div class="columns"` anywhere are assumed to be manually curated:

```haskell
let preexisting = T.isInfixOf "<div class=\"columns" input
unless preexisting $ ...
```

This avoids re-reporting lists in files where the author has already made deliberate choices.

---

## Configuration

| Setting | Location | Default | Purpose |
|---------|----------|---------|---------|
| `listLengthMaxN` | `Config/Misc.hs:111` | 75 | Maximum item width (characters) for a list to be considered "skinny" |

The threshold of 75 characters is roughly half the width of a gwern.net content line, ensuring side-by-side items fit comfortably.

---

## Integration Points

### Build System

Called from `sync.sh` during content validation:

```bash
(find . -name "*.md"; find ./metadata/annotation/ -maxdepth 1 -name "*.html") | \
    parallel --max-args=500 runghc -istatic/build/ ./static/build/Columns.hs --print-filenames
```

The `--print-filenames` flag enables filename prefixes in output for batch processing.

### GenerateSimilar.hs

Imports `listLength` to decide whether to wrap "See Also" link lists in columns:

```haskell
import Columns as CL (listLength)
...
if CL.listLength (BulletList similarItems) > 60 || length matchesPruned < 4
   then html
   else "<div class=\"columns\">\n" `T.append` html `T.append` "\n</div>"
```

Here the logic is inverted: wrap in columns only if items are short enough AND there are at least 4 items.

### CSS Support

The `columns` class is handled in `default.css` using CSS3 multi-column layout:

```css
.columns { column-count: 2; ... }
```

---

## See Also

- [hakyll.hs](/backend/hakyll-hs) - Main build pipeline that integrates column detection
- [Typography.hs](/backend/typography-hs) - Related Pandoc AST transformations
- [sync.sh](/backend/sync-sh) - Build orchestration that calls Columns.hs
- [layout.js](/frontend/layout-js) - Client-side block layout system
- [GenerateSimilar.hs](/backend/generate-similar-hs) - Uses `listLength` for "See Also" sections
- [Utils.hs](/backend/utils-hs) - `simplified` function for plain text rendering
