
# Tags.hs

**Path:** `build/Tags.hs` + `build/Config/Tags.hs` | **Language:** Haskell | **Lines:** ~1,200+ (209 + ~993)

> Hierarchical tag system: validation, normalization, rendering, and fuzzy matching for the `/doc/` directory taxonomy.

---

## Overview

Tags.hs manages gwern.net's taxonomy system where tags are **directory paths** under `/doc/`. A tag like `ai/nn/transformer/gpt` corresponds to the actual directory `/doc/ai/nn/transformer/gpt/` on disk. This conflation of tags and filesystem structure enables annotations to be "tagged" by placing files in directories, while also supporting cross-tagging through the annotation database.

The module handles four main concerns: **tag validation** (ensuring tags are lowercase alphanumeric with hyphens/slashes/dots), **tag normalization** (expanding short forms like "gpt4" → "ai/nn/transformer/gpt/4"), **tag rendering** (converting tags to clickable Pandoc links with abbreviated display names), and **tag inference** (guessing tags from file paths and URLs).

Configuration lives in `Config/Tags.hs`, which contains ~1000 lines of mapping tables for short→long tag expansions, display abbreviations, URL-based tag inference rules, and the test suite. This separation keeps the logic clean while allowing extensive customization of the tag vocabulary.

---

## Public API

### `tagsToLinksSpan :: [T.Text] -> Inline`

Converts a list of tag names to a Pandoc `Span` containing comma-separated links to tag index pages.

```haskell
tagsToLinksSpan ["economics", "psychology/writing"]
-- → Span ("",[link-tags"],[]) [Link ... "economics" ..., Str ", ", Link ... "psychology/writing" ...]
-- HTML: <span class="link-tags"><a href="/doc/economics/index">economics</a>, ...</span>
```

**Called by:** LinkMetadata.hs (annotation rendering)
**Calls:** `tagsToLinks`, `abbreviateTag`

---

### `tagsToLinksDiv :: [T.Text] -> Block`

Block-level variant of `tagsToLinksSpan` for contexts requiring a `Div` rather than `Span`.

**Called by:** hakyll.hs (page-level tag displays)

---

### `guessTagFromShort :: [String] -> [String] -> String -> String`

The fuzzy tag matcher. Given a short/ambiguous tag input, returns the canonical long form. Uses a cascade of matching strategies:

1. Exact match in tag list
2. Lookup in `tagsShort2Long` petnames ("gpt4" → "ai/nn/transformer/gpt/4")
3. Suffix/infix/partial path-segment matches (including against `tagsShort2Long` values)
4. Suffix/prefix/infix matches against the full tag list
5. Rewrite `.`→`/` and `-`→`/`, then retry matches
6. Levenshtein edit distance (max 3) for typo correction

```haskell
guessTagFromShort [] allTags "gpt4"        -- → "ai/nn/transformer/gpt/4"
guessTagFromShort [] allTags "sr1"         -- → "darknet-market/silk-road/1"
guessTagFromShort [] allTags "economicss"  -- → "economics" (typo fix)
```

**Called by:** GTX.hs, changeTag.hs, guessTag.hs
**Calls:** `C.tagsShort2Long`, `C.shortTagBlacklist`, `findClosestTagByDistance`

---

### `listTagsAll :: IO [String]`

Scans `/doc/` for all valid tag directories (those containing `index.md`). Validates syntax and filters blacklisted paths.

**Called by:** GTX.hs, LinkMetadata.hs, changeTag.hs, guessTag.hs
**Calls:** `getDirFiltered`, `validateTagSyntax`, `C.tagListBlacklist`

---

### `listTagDirectories :: Bool -> [FilePath] -> IO [FilePath]`

Converts tag directory paths like "doc/foo/index.md" to absolute paths "/doc/foo/index", verifying existence. The Bool controls whether to recurse into subdirectories.

**Called by:** generateDirectory.hs
**Calls:** `getSubdirsRecursive`

---

### `listTagDirectoriesAll :: [FilePath] -> IO [FilePath]`

Convenience wrapper for `listTagDirectories True` (full recursion).

**Called by:** generateDirectory.hs, Annotation/Gwernnet.hs

---

### `validateTagSyntax :: String -> Bool`

Checks that a tag contains only lowercase letters, digits, hyphens, slashes, and dots.

```haskell
validateTagSyntax "ai/nn/transformer"  -- True
validateTagSyntax "AI/NN"              -- False (uppercase)
validateTagSyntax "ai nn"              -- False (space)
```

**Called by:** `listTagsAll`, annotation-dump.hs

---

### `tag2TagsWithDefault :: String -> String -> [String]`

Parses a space-separated tag string, lowercases tags, and adds a default tag inferred from the file path if applicable.

```haskell
tag2TagsWithDefault "/doc/ai/2021-paper.pdf" "economics"
-- → ["ai", "economics"]  (adds "ai" from path)
```

**Called by:** GTX.hs

---

### `uniqTags :: [String] -> [String]`

Deduplicates tags and removes general tags when a more specific child exists.

```haskell
uniqTags ["ai", "ai/nn/transformer/gpt", "economics"]
-- → ["ai/nn/transformer/gpt", "economics"]  (removes redundant "ai")
```

**Called by:** GTX.hs

---

### `pages2Tags :: String -> [String] -> [String]`

Adds URL-inferred tags to existing tags using the `urlTagDB` rules.

**Called by:** GTX.hs

---

### `abbreviateTag :: T.Text -> T.Text`

Shortens a tag for display using the `tagsLong2Short` table and regex rewrites.

```haskell
abbreviateTag "reinforcement-learning/model/alphago"  -- → "AlphaGo"
abbreviateTag "ai/nn/transformer/gpt/4"               -- → "GPT-4"
abbreviateTag "psychology/cognitive-bias/sunk-cost"   -- → "sunk cost bias"
```

**Called by:** `tagsToLinks`, generateDirectory.hs, Annotation/Gwernnet.hs

---

### `testTags :: IO ()`

Runs the tag system test suite, verifying all short→long mappings and checking for cycles in rewrite rules.

**Called by:** Test.hs

---

## Internal Architecture

### Tag Resolution Flow

```
User Input (e.g. "gpt4")
         │
         ▼
┌─────────────────────────┐
│  guessTagFromShort      │
│  ┌────────────────────┐ │
│  │ Exact match?       │─┼──Yes──► Return as-is
│  └────────────────────┘ │
│           │ No          │
│           ▼             │
│  ┌────────────────────┐ │
│  │ In shortTagBlacklist│─┼──Yes──► Error (ambiguous)
│  └────────────────────┘ │
│           │ No          │
│           ▼             │
│  ┌────────────────────┐ │
│  │ Lookup tagsShort2Long│─┼──Found──► Return mapped value
│  └────────────────────┘ │
│           │ Not found   │
│           ▼             │
│  ┌────────────────────┐ │
│  │ Suffix/Infix/Path  │─┼──Found──► Return first match
│  │ segment (short tags)│ │
│  └────────────────────┘ │
│           │ Not found   │
│           ▼             │
│  ┌────────────────────┐ │
│  │ Suffix/Prefix/Infix│─┼──Found──► Return first match
│  │ (full tag list)    │ │
│  └────────────────────┘ │
│           │ Not found   │
│           ▼             │
│  ┌────────────────────┐ │
│  │ Rewrite ./- -> /   │─┼──Retry matches
│  └────────────────────┘ │
│           │ Still none  │
│           ▼             │
│  ┌────────────────────┐ │
│  │ Levenshtein (≤3)   │─┼──Found──► Return closest
│  └────────────────────┘ │
│           │ Not found   │
│           ▼             │
│     Return original     │
└─────────────────────────┘
```

### Key Data Structures (in Config/Tags.hs)

**`tagsShort2Long :: [(String, String)]`** (~300 entries)
Maps short forms and common typos to canonical tags:
```haskell
("gpt-4", "ai/nn/transformer/gpt/4")
("sr1", "darknet-market/silk-road/1")
("evoluton", "evolution")  -- typo
```

**`tagsLong2Short :: [(String, String)]`** (~350 entries)
Display abbreviations (priority ordered, first match wins):
```haskell
("ai/nn/transformer/gpt/4", "GPT-4")
("darknet-market/silk-road/1", "SR1 DNM")
("psychology/cognitive-bias", "cognitive bias")
```

**`urlTagDB :: [(String -> Bool, String)]`**
URL pattern → tag inference:
```haskell
(("https://publicdomainreview.org/" `isPrefixOf`), "history/public-domain-review")
(("r-project.org" `isInfixOf`), "cs/r")
```

**`shortTagBlacklist :: [String]`**
Ambiguous short forms that error rather than guess:
```haskell
["a", "an", "the", "error", "done", ...]
```

---

## Key Patterns

### Fixed-Point Tag Normalization

Tag guessing uses `fixedPoint` to repeatedly apply rewrites until stable:

```haskell
guessTagFromShort _ _ "" = ""
guessTagFromShort raw l s = fixedPoint (f l) (replace "=" "-" s)
```

This handles chained rewrites like `"silkroad2"` → `"silk-road/2"` → `"darknet-market/silk-road/2"`.

### Parent Tag Suppression

When a specific tag exists, its parent is redundant:

```haskell
uniqTags tags = filter (\t -> not (any ((t++"/") `isPrefixOf`) tags)) tags
```

So `["ai", "ai/nn/transformer"]` becomes `["ai/nn/transformer"]`.

### Tag Size Monitoring

Constants enforce annotation organization discipline:

```haskell
tagMax = 100      -- Max items per tag before splitting recommended
tagPairMax = 11   -- Max co-occurring tag pairs
```

`tagCount` and `tagPairsCount` aggregate statistics from the annotation database.

---

## Configuration

All configuration lives in `Config/Tags.hs`:

| Config | Purpose |
|--------|---------|
| `tagTypoMaxDistance = 3` | Levenshtein threshold for typo correction |
| `tagGuessBlacklist` | Paths where directory ≠ tag (archives, mirrors) |
| `tagListBlacklist` | Directories to exclude from tag enumeration |
| `urlTagDB` | URL patterns for automatic tag inference |
| `tagsShort2Long` | Short→canonical tag mappings |
| `tagsLong2Short` | Display abbreviations |
| `wholeTagRewritesRegexes` | Regex-based display transforms (e.g., "cs/" → "CS/") |
| `shortTagBlacklist` | Forbidden ambiguous short forms |
| `shortTagTestSuite` | Test cases for tag guessing |

---

## Integration Points

### With generateDirectory.hs

`generateDirectory` uses Tags to:
1. Enumerate all tag directories via `listTagDirectoriesAll`
2. Render tag links with abbreviations via `abbreviateTag`
3. Build `/doc/*/index` pages linking files and cross-tagged annotations

### With GTX.hs (Annotation Processing)

When creating/updating annotations:
1. `listTagsAll` provides valid tag vocabulary
2. `guessTagFromShort` normalizes user-input tags
3. `tag2TagsWithDefault` adds path-inferred tags
4. `uniqTags` removes redundant parent tags
5. `pages2Tags` adds URL-inferred tags

### With LinkMetadata.hs

`tagsToLinksSpan` renders tags in annotation popups/displays.

### Events & Shared State

- **Input:** Reads `/doc/` filesystem structure
- **Output:** Pandoc AST elements (Span, Div, Link)
- **Side effects:** None (pure functions except `listTagsAll` I/O)

---

## See Also

- [Config.Tags](/backend/config-tags-hs) - Tag system configuration (aliases, hierarchy mappings, display formatting)
- [guessTag.hs](/backend/guess-tag-hs) - CLI tool for testing tag expansion
- [changeTag.hs](/backend/change-tag-hs) - CLI tool for bulk tag add/remove operations
- [GTX.hs](/backend/gtx-hs) - Annotation data format that stores tags
- [LinkMetadata.hs](/backend/link-metadata-hs) - Uses tag rendering for annotations
- [Hakyll.hs](/backend/hakyll-hs) - Site generator integrating tag page generation
- [generateDirectory.hs](/backend/generate-directory-hs) - Builds tag index pages
