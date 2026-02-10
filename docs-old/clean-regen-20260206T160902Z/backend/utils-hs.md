
# Utils.hs

**Path:** `build/Utils.hs` | **Language:** Haskell | **Lines:** ~774

> Shared utility functions for file I/O, string manipulation, Pandoc AST transformations, and URL handling

---

## Overview

Utils.hs is the central utility module for the gwern.net Haskell backend, providing approximately 150 helper functions used throughout the build system. It consolidates common operations that don't belong to any specific domain module, serving as a shared dependency for Typography.hs, Annotation.hs, LinkMetadata.hs, and other backend components.

The module covers five major areas: (1) file I/O with atomic writes and change detection, (2) string manipulation including regex-based search-replace and fixed-string operations, (3) Pandoc AST utilities for converting between HTML/Markdown/plaintext and manipulating inline elements, (4) URL parsing and validation with gwern.net style enforcement, and (5) statistical and date calculation helpers.

A key design decision is the heavy use of `Text` alongside `String`, with most functions having both variants (e.g., `replace`/`replaceT`, `delete`/`deleteT`). The module also emphasizes safety through checked variants like `replaceChecked` and comprehensive error messages that include context for debugging.

---

## Public API

### File I/O

#### `writeUpdatedFile(template, target, contentsNew) -> IO ()`

Writes content to a file only if changed, using atomic rename via temp file. Creates parent directories as needed.

**Called by:** Most build modules when writing output files
**Calls:** `doesFileExist`, `createDirectoryIfMissing`, `emptySystemTempFile`, `renameFile`

#### `safeGetFileSize(path) -> IO Integer`

Returns file size, or 0 on error (no exception thrown).

**Called by:** `getDirectoryContentsSizeRecursive`
**Calls:** `getFileSize` (wrapped in try/catch)

#### `getDirectoryContentsSizeRecursive(dirPath) -> IO Integer`

Recursively calculates total size of all files in a directory tree.

**Called by:** Build statistics gathering
**Calls:** `safeGetFileSize`, `listDirectory`, `doesFileExist`, `doesDirectoryExist`

#### `getMostRecentlyModifiedDir(dir) -> IO String`

Returns Unix timestamp of most recently modified file in a directory.

**Called by:** Cache invalidation logic
**Calls:** `listDirectory`, `getModificationTime`

---

### String Manipulation

#### `replace(before, after, str) -> str`

Fixed-string replacement. Errors if `before == after`.

```haskell
replace "foo" "bar" "foo baz foo"  -- "bar baz bar"
```

**Called by:** Nearly all modules
**Calls:** `split`, `intercalate`

#### `replaceChecked(before, after, str) -> str`

Strict replacement that errors if: any argument is null, arguments aren't unique, or replacement didn't happen.

**Called by:** Code paths where replacement must succeed
**Calls:** `replace`

#### `sed(before, after, str) -> str`

Regex-based search and replace using POSIX extended regex syntax. Catches exceptions and provides debugging context.

```haskell
sed "^<p>(.*)</p>$" "\\1" "<p>hello</p>"  -- "hello"
```

**Called by:** `toHTML`, HTML cleanup functions
**Calls:** `mkRegex`, `subRegex`

#### `sedMany(regexps) -> (String -> String)`

Apply multiple regex replacements in sequence.

**Called by:** Batch text cleanup
**Calls:** `sed`, `isUniqueList`

#### `delete(target) -> (String -> String)`

Delete substring from string (specialized `replace x ""`).

**Called by:** Content cleanup
**Calls:** `replace`

#### `deleteMany(targets) -> (String -> String)`

Delete multiple substrings.

**Called by:** `inlinesToText`, content cleanup
**Calls:** `delete`, `isUniqueList`

#### `deleteMixed(target, str) -> String`

Smart deletion: if target starts with space, remove as suffix; if ends with space, remove as prefix; otherwise delete anywhere.

```haskell
deleteMixed " - Site Name" "Title - Site Name"  -- "Title"
deleteMixed "Site: " "Site: Title"              -- "Title"
```

**Called by:** Title cleanup in annotation scrapers
**Calls:** `delete`

#### `trim(str) -> String`

Strip leading/trailing whitespace and hyphens.

**Called by:** `simplifiedString`, various cleanups
**Calls:** `dropWhile`, `reverse`

---

### Pandoc Utilities

#### `toPandoc(html) -> Pandoc`

Parse HTML string to Pandoc AST.

**Called by:** `simplifiedHtmlToString`, content processing
**Calls:** `readHtml`

#### `toHTML(inline) -> String`

Render a single Pandoc Inline to HTML string, stripping `<p>` wrapper.

```haskell
toHTML $ Span nullAttr [Str "foo"]  -- "<span>foo</span>"
toHTML $ Str "foo"                   -- "foo"
```

**Called by:** HTML generation
**Calls:** `writeHtml5String`, `sed`

#### `toMarkdown(html) -> String`

Convert HTML to Markdown.

**Called by:** Content conversion
**Calls:** `readHtml`, `writeMarkdown`

#### `simplified(block) -> Text`

Render a Pandoc Block to plain text (no formatting).

**Called by:** Title/description extraction
**Calls:** `simplifiedDoc`

#### `simplifiedDoc(pandoc) -> Text`

Render entire Pandoc document to plain text with very wide column width (prevents unwanted line breaks).

**Called by:** `simplified`, `simplifiedHtmlToString`
**Calls:** `writePlain`

#### `inlinesToText(inlines) -> Text`

Extract plain text from Pandoc inlines, recursively unwrapping formatting.

```haskell
inlinesToText [Emph [Str "hello"], Str " ", Strong [Str "world"]]
-- "hello world"
```

**Called by:** Link text extraction, title handling
**Calls:** Recursive pattern matching on Inline types

#### `parseRawAllClean(pandoc) -> Pandoc`

Parse RawBlock HTML into proper Pandoc AST and clean up empty Divs/Spans.

**Called by:** Content normalization
**Calls:** `parseRawBlock`, `cleanUpSpans`, `cleanUpDivsEmpty`

#### `addClass(class, inline) -> Inline`

Add CSS class to Link, Span, Image, or Code inline.

**Called by:** Link annotation, styling
**Calls:** Pattern matching on Inline

#### `removeClass(class, inline) -> Inline`

Remove CSS class from inline element.

**Called by:** Cleanup passes
**Calls:** `filter`

#### `hasClass(class, inline) -> Bool`

Check if inline element has a CSS class.

**Called by:** Conditional processing
**Calls:** `elem`

#### `addKey / removeKey / hasKey`

Manipulate key-value attributes on inline elements.

---

### URL Handling

#### `host(url) -> Text`

Extract hostname from URL. Enforces gwern.net style: root domains must have trailing slash.

```haskell
host "https://example.com/path"  -- "example.com"
host "https://example.com"       -- prints warning, returns "example.com"
```

**Called by:** Link categorization, domain checks
**Calls:** `parseURIReference`, `escapeUnicode`

#### `isURL(url) -> Bool`

Check if string is valid HTTP/HTTPS URL.

**Called by:** Link validation
**Calls:** `parseURI`, `escapeUnicode`

#### `isURLAny(url) -> Bool`

Check any URL (local paths, mailto:, irc:, etc.).

**Called by:** General link validation
**Calls:** `isURL`, `isURILocalT`

#### `isLocal(text) -> Bool`

Check if URL is local (starts with `/`).

**Called by:** Link categorization
**Calls:** `T.head`

#### `hasExtension(ext, path) -> Bool`

Check file extension of URL/path.

**Called by:** File type detection
**Calls:** `extension`

#### `escapeUnicode(text) -> Text`

Percent-encode Unicode characters for URI compatibility.

**Called by:** `host`, `isURLT`
**Calls:** `escapeURIString`

---

### Statistics & Dates

#### `calculatePercentilesFromWholeNumbers(sizes) -> [Int]`

Calculate percentile rank (0-100) for each positive integer in a list.

**Called by:** Size-based styling/ranking
**Calls:** `sort`, `Map.fromList`

#### `calculateDateSpan(start, end) -> Int`

Calculate days between two dates (supports YYYY, YYYY-MM, YYYY-MM-DD formats).

```haskell
calculateDateSpan "1939-09-01" "1945-05-08"  -- 2077
```

**Called by:** Duration calculations
**Calls:** `parseDate`, `diffDays`

#### `formatDaysInLargestUnit(days) -> String`

Format day count as days/months/years.

```haskell
formatDaysInLargestUnit 365  -- "1y"
formatDaysInLargestUnit 45   -- "1m"
formatDaysInLargestUnit 7    -- "7d"
```

**Called by:** Human-readable duration display
**Calls:** Arithmetic

#### `formatIntWithCommas(n) -> String`

Format integer with thousand separators.

```haskell
formatIntWithCommas 1234567  -- "1,234,567"
```

**Called by:** Statistics display
**Calls:** `intercalate`, `chunksOf`

---

### Utility Helpers

#### `fixedPoint(f, x) -> x`

Repeatedly apply function until output stabilizes. Detects cycles and infinite loops (5000 iteration limit).

**Called by:** Normalization loops
**Calls:** Recursive with Set-based cycle detection

#### `frequency(list) -> [(Int, a)]`

Count occurrences, return sorted by frequency ascending.

```haskell
frequency "hello"  -- [(1,'e'),(1,'h'),(2,'l'),(1,'o')]
```

**Called by:** Analysis/statistics
**Calls:** `group`, `sort`

#### `repeated(list) -> [a]`

Find elements appearing more than once.

```haskell
repeated "foo bar"  -- "o"
```

**Called by:** Duplicate detection
**Calls:** `M.filter`, `M.fromListWith`

#### `ensure(location, description, predicate, list) -> list`

Assert predicate holds for all list elements; fatal error with context if not. Forces evaluation via `deepseq`.

**Called by:** Validation passes
**Calls:** `deepseq`, `error`

#### `truncateString(maxLen, str) -> String`

Truncate string at word boundary, append "…".

```haskell
truncateString 20 "This is a long string"  -- "This is a long…"
```

**Called by:** Title display, column fitting
**Calls:** `elemIndices`, `take`

---

## Internal Architecture

### Data Flow Patterns

```
HTML Input → toPandoc → Pandoc AST → walk/topDown transforms → toHTML/toMarkdown → Output
                              ↑
                    parseRawAllClean (normalize Raw* blocks)
                              ↓
                    cleanUpSpans/cleanUpDivsEmpty (remove cruft)
```

### Key Data Structures

**Pandoc Attr:** `(id, [classes], [(key,value)])` - the attribute tuple used throughout
```haskell
nullAttr = ("", [], [])
```

**WriterOptions:** `safeHtmlWriterOptions` sets column width to 9999 to prevent unwanted line breaks in tables/grids.

### String vs Text

Most functions come in pairs:
- `replace` / `replaceT`
- `delete` / `deleteT`
- `deleteMany` / `deleteManyT`
- `anyInfix` / `anyInfixT`
- `kvLookup` / `kvLookupT`

The `T` suffix indicates `Data.Text` versions.

---

## Key Patterns

### Atomic File Writes

```haskell
writeUpdatedFile template target contentsNew = do
  existsOld <- doesFileExist target
  if not existsOld then do
    createDirectoryIfMissing True (takeDirectory target)
    TIO.writeFile target contentsNew
  else do
    contentsOld <- TIO.readFile target
    if contentsNew /= contentsOld then do
      tempPath <- emptySystemTempFile ("hakyll-"++template)
      TIO.writeFile tempPath contentsNew
      renameFile tempPath target  -- atomic on POSIX
    else return ()
```

Only writes when content changed. Uses temp file + rename for atomicity, preventing partial writes.

### Safe Regex with Debug Context

```haskell
sed before after s = unsafePerformIO $ do
  catch action handleExceptions
    where
      handleExceptions e = return $
        "Error occurred. Exception: " ++ show e ++
        "; arguments were: '" ++ before ++ "' : '" ++ after ++ "' : '" ++ s ++ "'"
```

Regex operations catch exceptions and include full context (pattern, replacement, input) in error messages.

### Fixed-Point with Cycle Detection

```haskell
fixedPoint' n seen f i
  | i `S.member` seen = error $ "Cycle detected! Last result: " ++ show i
  | otherwise = let i' = f i
                in if i' == i then i
                   else fixedPoint' (n-1) (S.insert i seen) f i'
```

Tracks all previous values in a Set to detect cycles, with 5000-iteration safety limit.

### Pandoc Inline Class Manipulation

```haskell
addClass clss x@(Link (i, clsses, ks) s (url, tt)) =
  if clss `elem` clsses
  then x
  else Link (i, clss:clsses, ks) s (url, tt)
```

Uniform handling across Link, Span, Image, Code with idempotent add (no duplicates).

---

## Configuration

### Writer Options

`safeHtmlWriterOptions` - Used for HTML output:
- `writerColumns = 9999` - Prevents line-breaking in grid tables
- `writerExtensions = enableExtension Ext_shortcut_reference_links pandocExtensions` - Cleaner link syntax

### Regex Library

Uses `regex-compat-tdfa` (not `regex-compat`) for Unicode support. This is critical—wrong package causes silent failures with non-ASCII text.

### Fixed-Point Limits

- Default iteration limit: 5000
- Can be tuned per-call if number of rewrite rules is known

---

## Integration Points

### Terminal Output

```haskell
printGreen :: String -> IO ()  -- Normal progress (green text)
printRed   :: String -> IO ()  -- Errors/warnings (red background)
```

Used throughout build system for status messages. Red messages truncated at 2048 chars.

### Shared Predicates

Several predicates are used across modules:
- `isInflationURL` / `isInflationLink` - Detect `$`/`₿` prefixed prices
- `isLocal` - Local vs external URL
- `hasExtension` - File type checking

### Pandoc Ecosystem

Depends on:
- `Text.Pandoc` - Core AST types and readers/writers
- `Text.Pandoc.Walk` - AST traversal (`walk`, `topDown`)

Provides utilities consumed by Typography.hs, LinkMetadata.hs, and content processing.

### External Tools

`inlineMath2Text` shells out to `static/build/latex2unicode.py` for LaTeX→Unicode conversion.

---

## See Also

- [Hakyll.hs](/backend/hakyll-hs) - Site generator that imports Utils as a core dependency
- [Query.hs](/backend/query-hs) - Uses Pandoc AST utilities for link extraction
- [Typography.hs](/backend/typography-hs) - Major consumer of string manipulation and Pandoc utilities
- [Annotation.hs](/backend/annotation-hs) - Uses string manipulation and URL handling
- [LinkMetadata.hs](/backend/link-metadata-hs) - Uses file I/O, URL validation, and text processing
- [Unique.hs](/backend/unique-hs) - Companion validation utilities for configuration lists
- [sync.sh](/backend/sync-sh) - Build orchestrator that coordinates modules using Utils
