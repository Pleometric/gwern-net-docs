
# link-suggester.hs

**Path:** `build/link-suggester.hs` | **Language:** Haskell | **Lines:** ~114

> Generates an Emacs-compatible link suggestion database from site-wide anchor/URL pairs

---

## Overview

`link-suggester.hs` extracts anchor text and URLs from all Markdown/HTML files on gwern.net and produces an Elisp file that Emacs can load for semi-automated link insertion. When writing annotations or essays, this enables quick conversion of plain text into properly linked references by matching known anchor text to their canonical URLs.

The tool implements a multi-stage filtering pipeline: URLs must appear at least 4 times across the corpus (configurable via `hitsMinimum`), anchor text must not appear associated with multiple URLs (ambiguity filter), must not be dictionary words, and must pass pattern-based blacklists. This aggressive filtering ensures only high-confidence, unambiguous suggestions reach the final database.

The output format is native Elisp that sets `markdown-rewrites` to an association list of `(anchor-text URL)` pairs, sorted by anchor length (longest first) for greedy matching. A secondary debug file captures which entries failed each filtering stage for diagnostics.

---

## Public API

### `main :: IO ()`

Entry point. Reads filenames from stdin, parses all files in parallel, builds the suggestion database through filtering stages, writes the Elisp output to the first CLI argument, and writes debug output to `metadata/linkSuggestions-deleted.hs`.

**Called by:** Command line via `find . -name "*.md" -or -name "*.html" | link-suggester.hs output.el`

**Calls:** `parseURLs`, filtering pipeline, `writeUpdatedFile`

---

### `parseURLs :: FilePath -> IO [(T.Text, [T.Text])]`

Parses a single file and extracts all URL → anchor text mappings. Normalizes `https://gwern.net/` prefixes to `/` for local URLs.

**Called by:** `main` (via `Par.mapM`)

**Calls:** `Query.parseMarkdownOrHTML`, `Query.extractURLsAndAnchorTooltips`

---

## Internal Architecture

### Processing Pipeline

```
Input files (stdin)
        ↓
[Parse all files in parallel] → [(URL, [AnchorText])]
        ↓
[Build Map: URL → [Anchors]]
        ↓
[Filter: blacklisted URLs] → filterURLs
        ↓
[Filter: hitsMinimum ≥ 4] → keep URLs with enough occurrences
        ↓
[Clean anchors] → strip whitespace/punctuation
        ↓
[Filter: duplicate anchors] → remove anchors pointing to multiple URLs
        ↓
[Filter: dictionary words] → /usr/share/dict/words exclusion
        ↓
[Filter: bad anchor patterns] → regex + blacklist
        ↓
[Union with whitelist] → restore known-good pairs
        ↓
[Invert: Map URL [Anchor] → [(Anchor, URL)]]
        ↓
[Sort by anchor length descending]
        ↓
[Merge case variants] → prefer capitalized forms
        ↓
[Format as Elisp]
        ↓
output file (first CLI argument)
```

### Key Data Structures

**Suggestion Map:** `M.Map T.Text [T.Text]` — URL to list of associated anchor texts

**Reversed DB:** `[(T.Text, T.Text)]` — (anchor, URL) pairs for Emacs consumption

---

## Key Patterns

### Multi-Stage Filtering with Audit Trail

Each filtering stage preserves the removed entries in a `dbFailed*` variable:

```haskell
let dbMinimumLess = M.filter (\texts -> length texts >= C.hitsMinimum) db
let dbFailedMinimum = ("Did not pass hitsMinimum filter",
                       db `M.difference` dbMinimumLess)
```

This produces `metadata/linkSuggestions-deleted.hs` containing all rejected entries grouped by rejection reason—invaluable for debugging false negatives.

### Case-Sensitive Deduplication

When multiple case variants of an anchor exist (e.g., "GPT-3" and "gpt-3"), the merger preserves the capitalized form:

```haskell
mergeAssocList xs = M.elems mergedMap
  where
    mergedMap = M.fromListWith choose
                [ (T.toLower k, (k, v)) | (k, v) <- xs ]
    choose p1@(k1, _) p2@(k2, _) =
      case (isAllLower k1, isAllLower k2) of
        (True, False)  -> p2  -- prefer capitalized
        (False, True)  -> p1
        _              -> p1
```

### Length-Sorted Output

Suggestions are sorted by anchor length descending so longer, more specific matches are tried first during Emacs search-and-replace:

```haskell
sortBy (\(t1,_) (t2,_) ->
  if T.length t1 > T.length t2 then LT else ...)
```

This ensures "reinforcement learning" matches before "learning".

---

## Configuration

All configuration lives in `Config/LinkSuggester.hs`:

| Setting | Value | Purpose |
|---------|-------|---------|
| `hitsMinimum` | 4 | Minimum URL occurrences to consider |
| `anchorLengthMaximum` | 80 | Maximum anchor text length |
| `filterURLs` | function | URL blacklist (dropbox, newsletters, metadata paths) |
| `filterAnchors` | function | Anchor blacklist (regexes for page numbers, figures, etc.) |
| `badAnchorStrings` | ~400 items | Explicit anchor blacklist |
| `whiteListDB` | ~250 entries | Override pairs that pass filters regardless |

### URL Filtering Rules

- Paths starting with `$`, `₿`, `#`, `/static/img/`, `/metadata/`, `/newsletter/20`
- Domain blacklist: `dropbox.com`, `harney.com`
- Doc index pages: `/doc/*/index`

### Anchor Filtering Rules

- Length > 80 characters
- Contains `$`, `%`, `[`, `]`
- Starts with `(`, `.`, `"Wikipedia link about "`
- Matches patterns: page numbers (`p.123`), figure references (`Figure S1`), chapter numbers
- In explicit blacklist (~400 common words/phrases like "for example", "ADHD", "see also")

---

## Integration Points

### Input

- **stdin:** Newline-separated file paths (from `find`)
- **File system:** Reads `/usr/share/dict/words` for dictionary filtering

### Output

**`linkSuggestions.el`** — Elisp file for Emacs:

```elisp
(setq markdown-rewrites '(
                 ("reinforcement learning" "https://en.wikipedia.org/wiki/Reinforcement_learning")
                 ("GPT-3" "/gpt-3")
                 ...
                 )
      )
```

**`metadata/linkSuggestions-deleted.hs`** — Debug file with rejected entries

### Emacs Integration

The generated file is loaded via `load-file` in Emacs. The `markdown-rewrites` variable is then used by custom Emacs functions to offer link suggestions during Markdown editing—when the cursor is on text matching a known anchor, Emacs can insert the appropriate Markdown link syntax.

### Dependencies

- `Query.hs` — `extractURLsAndAnchorTooltips`, `parseMarkdownOrHTML`
- `Utils.hs` — `writeUpdatedFile`, `printGreen`, `repeated`
- `Config/LinkSuggester.hs` — All configuration
- `monad-parallel` — Parallel file processing

---

## See Also

- [Config.LinkSuggester](/backend/config-link-suggester-hs) - Filtering rules and whitelists for suggestions
- [link-extractor.hs](/backend/link-extractor-hs) - Simpler URL extraction tool
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database consulted for suggestions
- [LinkID.hs](/backend/link-id-hs) - Citation ID generation for deduplication
- [Query.hs](/backend/query-hs) - URL extraction utilities
- [Interwiki.hs](/backend/interwiki-hs) - Interwiki link expansion
