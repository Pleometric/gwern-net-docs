
# GTX.hs

**Path:** `build/GTX.hs` | **Language:** Haskell | **Lines:** ~234

> Custom plain-text format for storing link annotations

---

## Overview

GTX ("Gwern Text") is a custom document format designed for storing and editing link annotations on gwern.net. It replaces earlier solutions that used Haskell `Read`/`Show` tuple-formatted data files and YAML, which both had significant usability problems.

The format is optimized for three properties: **human-writability** (no quoting, escaping, or indentation sensitivity), **git-diffability** (changes show cleanly in version control), and **append-friendliness** (new entries can be trivially added to the end of a file). It's used to store 20,000+ annotations across four GTX files: `metadata/me.gtx` (personal pages), `metadata/full.gtx` (hand-annotated external links), `metadata/half.gtx` (tagged but not fully annotated), and `metadata/auto.gtx` (auto-generated annotations).

The design philosophy is aggressive simplicity. GTX avoids named fields (position-based instead), quoting/escaping (only the abstract can contain arbitrary text, and it's always last), indentation semantics (fields are single lines), and complex nesting (flat records). The trade-off is zero flexibility - the format is hardcoded to exactly seven fields in exactly one order for exactly one use case.

---

## The GTX File Format

### Record Structure

A GTX file is UTF-8 text with the `.gtx` extension. Records are separated by `---\n` delimiters. Each record has 7 fixed fields (plus the abstract which can span multiple lines):

| Line | Field | Format | Required |
|------|-------|--------|----------|
| 0 | Separator | `---` | Yes |
| 1 | URL | Valid URI, no whitespace | **Yes** (only mandatory field) |
| 2 | Title | UTF-8 HTML string | No |
| 3 | Authors | Comma-separated names | No |
| 4 | Date | `YYYY[-MM[-DD]]` or natural language | No |
| 5 | Date Created | `YYYY-MM-DD` (when annotation was added) | Auto-filled if empty |
| 6 | Key-Values | Haskell list `[("key","val")]` or naked DOI/ID | No |
| 7 | Tags | Space-separated directory names (must match `doc/*`) | Semi-mandatory |
| 8+ | Abstract | HTML (until next `---`) | No |

### Example GTX File

```
---
https://jackcook.com/2024/02/23/mamba.html

2024-11-15

ai/nn/rnn

---
http://www.demarcken.org/carl/papers/ITA-software-travel-complexity/ITA-software-travel-complexity.pdf
Computational Complexity of Air Travel Planning
Carl de Marcken
2003
2024-06-10

cs/computable

---
https://arxiv.org/abs/1003.0358#schmidhuber
Deep Big Simple Neural Nets Excel on Handwritten Digit Recognition
Dan Claudiu Ciresan, Ueli Meier, Luca Maria Gambardella, Jürgen Schmidhuber
2010-03-01
2023-04-22
10.1162/NECO_a_00052
ai/nn/fully-connected ai/scaling/hardware
<p>Good old on-line back-propagation for plain multi-layer perceptrons yields
a very low 0.35% error rate on the famous <a href="https://en.wikipedia.org/wiki/MNIST_database">MNIST</a>
handwritten digits benchmark.</p>
<p>All we need to achieve this best result so far are many hidden layers,
many neurons per layer, numerous deformed training images, and graphics cards.</p>
```

### Key-Value Line Shortcuts

For convenience when hand-writing entries, the key-value line supports multiple input formats:

```
# Empty (becomes [])
(blank line)

# Naked DOI (must contain '/')
10.1162/NECO_a_00052

# Naked ID (hash or structured like "smith-2020")
_aB3cD4eF
smith-et-al-2020

# DOI + ID on same line (max 2 parts)
10.1162/NECO_a_00052 smith-et-al-2020

# Full Haskell list (canonical output format)
[("doi","10.1162/NECO_a_00052"),("id","smith-et-al-2020")]
```

DOIs are distinguished from IDs by containing `/`. All forms are normalized to the Haskell list format on write.

### Permitted Key Names

Keys in the key-value list are whitelisted (from `Config.Misc.gtxKeyValueKeyNames`):

| Key | Purpose |
|-----|---------|
| `doi` | Digital Object Identifier |
| `id` | Human-readable citation ID (e.g., "smith-et-al-2020") |
| `title`, `description` | Alternative text |
| `created`, `modified` | Timestamps |
| `status`, `importance`, `confidence` | Metadata quality indicators |
| `css-extension`, `invert` | Display options |
| `backlink`, `placeholder`, `index` | Link behavior flags |
| `thumbnail`, `thumbnail-text` | Preview images |

---

## Public API

### `readGTX :: FilePath -> IO MetadataList`

Fast reader that parses a GTX file into a list of `(Path, MetadataItem)` tuples. Checks if the file exists (trying both relative and absolute paths). No validation or normalization.

```haskell
readGTX :: FilePath -> IO MetadataList
readGTX f = do
  content <- TIO.readFile f'
  return $ parseGTX content
```

**Called by:** `readGTXFast`, `readGTXSlow`, `rewriteLinkMetadata`
**Calls:** `parseGTX`, `TIO.readFile`

### `readGTXFast :: FilePath -> IO MetadataList`

Alias for `readGTX`. Used during normal builds where speed matters and data is trusted.

**Called by:** `LinkMetadata.readLinkMetadata`, `rescrapeGTX`

### `readGTXSlow :: FilePath -> IO MetadataList`

Full parsing with extensive postprocessing:
- Canonicalizes author names via `cleanAuthors`/`authorsCanonicalize`
- Parses fuzzy dates ("September 1st, 1981" → "1981-09-01") via `guessDateFromString`
- Expands tag shortcuts to full paths via `guessTagFromShort`
- Strips Unicode whitespace from URLs (e.g., `⁄` → `/`)
- Converts curly double quotes to single quotes in titles
- Validates entries aren't malformed (rejects empty URLs like `"---"`)

```haskell
readGTXSlow path = do
  allTags <- listTagsAll
  results <- fmap (map (postprocessing allTags)) $ readGTX path
  results' <- mapM fixDate results
  let badEntries = filter (\(p,_) -> p `elem` ["---", "---.md", ""]) results'
  if null badEntries then return results' else error ...
```

**Called by:** `LinkMetadata.readLinkMetadataSlow`, `readLinkMetadataAndCheck`, `walkAndUpdateLinkMetadataGTX`
**Calls:** `readGTX`, `listTagsAll`, `guessDateFromString`, `authorsCanonicalize`

### `writeGTX :: FilePath -> MetadataList -> IO ()`

Writes an entire MetadataList to a GTX file, replacing its contents. Uses a global lock to prevent concurrent writes. If date-created is empty, fills in today's date.

```haskell
writeGTX :: FilePath -> MetadataList -> IO ()
writeGTX f ml = do
  today <- C.todayDayString
  let lists = concatMap (untupleize today) ml
  void $ GL.lock $ writeUpdatedFile "gtx" f $ T.unlines lists
```

**Called by:** `rewriteLinkMetadata`, `walkAndUpdateLinkMetadataGTX`, `rescrapeItem`
**Calls:** `untupleize`, `writeUpdatedFile`, `GL.lock`

### `appendLinkMetadata :: Path -> MetadataItem -> IO ()`

Appends a single annotation to `metadata/auto.gtx`. Used when new links are discovered during compilation. Uses yesterday's date if running after midnight (heuristic: compilation may have started the previous day).

```haskell
appendLinkMetadata :: Path -> MetadataItem -> IO ()
appendLinkMetadata l i = do
  overnight <- C.lateNight
  today <- if overnight then C.yesterdayDayString else C.todayDayString
  let newGTX = T.unlines $ untupleize today (l, i)
  void $ GL.lock $ TIO.appendFile "metadata/auto.gtx" newGTX
```

**Called by:** `LinkMetadata.annotateLink`
**Calls:** `untupleize`, `GL.lock`

### `rewriteLinkMetadata :: MetadataList -> MetadataList -> Path -> IO ()`

Cleans `auto.gtx` by removing entries that have been "promoted" to `half.gtx` or `full.gtx`. Deduplicates and sorts via Map construction.

```haskell
rewriteLinkMetadata half full gtx = do
  old <- readGTXFast gtx
  let betterURLs = nubOrd (map fst half ++ map fst full)
  let old' = filter (\(p,_) -> p `notElem` betterURLs) old
  let new = M.fromList old'  -- Map deduplicates/sorts
  writeGTX gtx (M.toList new)
```

**Called by:** `LinkMetadata.readLinkMetadataAndCheck`
**Calls:** `readGTXFast`, `writeGTX`

### `untupleize :: String -> (Path, MetadataItem) -> [T.Text]`

Converts a MetadataItem back to GTX lines. Exported for use by `LinkMetadata.annotateLink` logging.

**Called by:** `writeGTX`, `appendLinkMetadata`, `LinkMetadata.annotateLink`

---

## Internal Architecture

### MetadataItem Type

Defined in `LinkMetadataTypes.hs`:

```haskell
type MetadataItem = (String, String, String, String, [(String,String)], [String], String)
--                  Title   Author   Date   DateCreated  KeyValues     Tags     Abstract

type MetadataList = [(Path, MetadataItem)]
type Path = String  -- URL or local path like "/essay-name"
```

### Parse Pipeline

```
GTX File Text
    ↓ T.drop 4 (skip initial "---\n")
    ↓ T.splitOn "\n---\n"
List of Record Texts
    ↓ map T.lines
List of Line Lists  [url, title, author, date, dc, kv, tags, abstract...](url, title, author, date, dc, kv, tags, abstract...)
    ↓ map tupleize
MetadataList [(Path, MetadataItem)]
```

### Key Internal Functions

**`parseGTX :: T.Text -> MetadataList`** - Splits on `\n---\n`, then tupleizes each chunk.

**`tupleize :: [T.Text] -> (Path, MetadataItem)`** - Converts 7+ lines to tuple. Abstract is all remaining lines joined.

**`doiOrIDorKV :: [T.Text] -> String -> [(String,String)]`** - Parses key-value line:

```haskell
doiOrIDorKV mi s
  | s == ""       = []
  | s == "[]"     = []
  | head s == '[' = read s  -- full Haskell list
  | otherwise     =
      let parts = words s
          dois = filter ('/' `elem`) parts
          ids = filter isValidID parts
      in [("doi", d) | d <- dois] ++ [("id", i) | i <- ids]
```

---

## Key Patterns

### Why No Escaping?

The abstract field is always last and extends to the next `---\n`. This means abstracts can contain newlines, quotes, colons, brackets—anything except the literal `\n---\n` sequence.

### Global File Lock

```haskell
import System.GlobalLock as GL (lock)

void $ GL.lock $ writeUpdatedFile "gtx" f content
void $ GL.lock $ TIO.appendFile "metadata/auto.gtx" newGTX
```

Multiple build processes might append annotations simultaneously. The `global-lock` package provides cross-process file locking.

### Late Night Date Handling

```haskell
overnight <- C.lateNight
today <- if overnight then C.yesterdayDayString else C.todayDayString
```

If a build runs after midnight, new annotations are backdated to the previous day. This keeps "date created" meaningful for annotations discovered during long overnight builds.

### Trailing Newline Problem

The source comments note a subtle issue: if the final `auto.gtx` entry lacks a tag and your editor strips trailing blank lines, the abstract gets truncated. Workaround: ensure the last entry always has a tag, so there's exactly one trailing newline in the abstract field.

---

## Configuration

### Date Validation

Dates must be ISO 8601 format or parseable by `guessDateFromString`:
- `2024`, `2024-03`, `2024-03-15` → valid
- `March 15, 2024` → parsed to `2024-03-15`

Invalid dates that can't be parsed cause `readGTXSlow` to error rather than silently return empty string.

### Tag Validation

In `readGTXSlow`, tags are validated against `doc/*` directories via `listTagsAll`. Short tags like `ai` are expanded to full paths via `guessTagFromShort`.

### ID Validation

IDs must pass `LinkID.isValidID`:
- 9-character base64 hashes starting with `_` (e.g., `_aB3cD4eF`)
- Structured IDs: letters (including Unicode), digits, hyphens (e.g., `smith-et-al-2020`)
- No leading/trailing hyphens, no starting with `_` unless hash

---

## Integration Points

### GTX Database Files

```
metadata/
├── me.gtx      # Personal annotations (Gwern's own pages)
├── full.gtx    # Hand-written complete annotations
├── half.gtx    # Tagged but not fully written
└── auto.gtx    # Machine-generated (lowest priority)
```

Priority during merge: `me` > `full` > `half` > `auto` (left-biased Map.union)

### Module Dependencies

- **LinkMetadataTypes.hs** - `MetadataItem`, `MetadataList`, `Path` types
- **Tags.hs** - `listTagsAll`, `guessTagFromShort`, tag validation
- **Metadata.Author.hs** - `authorsCanonicalize`, `cleanAuthors`
- **Metadata.Date.hs** - `guessDateFromString`, `guessDateFromLocalSchema`, `isDate`
- **LinkID.hs** - `isValidID` for ID field validation
- **Config.Misc.hs** - `gtxKeyValueKeyNames` whitelist

### Consumers

- **LinkMetadata.hs** - Main consumer; reads all GTX files, validates, merges
- **annotation-dump.hs** - CLI tool for dumping annotations
- **changeTag.hs** - CLI tool for renaming tags across GTX files
- **Paragraph.hs** - Reads GTX for paragraph-level processing
- **GenerateSimilar.hs** - Reads GTX for embedding generation

---

## Error Handling

GTX parsing errors are fatal (calls `error`):

| Error | Cause |
|-------|-------|
| Empty list in tupleize | Record has no lines |
| Missing mandatory entries | Record has < 7 lines |
| Empty KV parsing | `[""]` or `[("","")]` in key-values |
| Malformed KV | Starts with `[` but doesn't end with `]` |
| Too many parts | More than 2 naked DOI/ID values |
| Invalid ID | ID doesn't pass `isValidID` |
| Invalid path | URL is `"---"`, `""`, or `"---.md"` |

The philosophy is fail-fast rather than silently corrupt data.

---

## Why Not YAML?

The module header documents YAML's problems:

1. **Indentation sensitivity** - Easy to break, hard to debug
2. **Quote inconsistency** - Writer alternates single/double unpredictably
3. **Colon breakage** - Adding `:` to text could break parsing
4. **Line wrapping** - Forcibly wraps at short lengths, breaks search
5. **Date typing** - `2000` parses as integer, must quote `"2000"`
6. **Field omission** - Easy to forget unlabeled fields
7. **Markdown incompatibility** - Would need escaping

GTX solves these with fixed field order, no quotes, no indentation, simple `---` delimiter.

---

## See Also

- [LinkMetadata.hs](/backend/link-metadata-hs) - Main consumer; merges and validates GTX files
- [LinkMetadataTypes.hs](/backend/link-metadata-types-hs) - Type definitions (MetadataItem, Path)
- [Annotation.hs](/backend/annotation-hs) - Generates MetadataItems written to auto.gtx
- [Metadata/Author.hs](/backend/metadata-author-hs) - Author canonicalization called during slow read
- [Metadata/Date.hs](/backend/metadata-date-hs) - Date parsing called during slow read
- [annotation-dump.hs](/backend/annotation-dump-hs) - CLI for querying GTX databases
