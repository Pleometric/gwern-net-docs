
# Config.Tags

**Path:** `build/Config/Tags.hs` | **Language:** Haskell | **Lines:** ~993

> Tag system configuration: aliases, hierarchy mappings, and display formatting

---

## Overview

Config.Tags is a pure configuration module that defines the gwern.net tag taxonomy. It contains no business logic—just data tables that map between canonical tag paths, display names, URL shortcuts, and typo corrections.

The module serves three primary purposes: (1) normalizing user-entered tags to canonical forms (e.g., "gpt-4" → "ai/nn/transformer/gpt/4"), (2) converting internal hierarchical paths to human-readable display names (e.g., "anime/eva" → `<em>NGE</em>`), and (3) guessing tags from URLs when scraping external links. The extensive alias tables (~200 entries in `tagsShort2LongRewrites` alone) reflect years of accumulated typo corrections and naming conventions.

A notable design decision is the use of hierarchical slash-separated paths as canonical tag identifiers. This enables both precise categorization (e.g., "ai/nn/transformer/gpt/4/poetry") and inheritance-based browsing. The display layer then collapses these verbose paths into concise labels.

---

## Public API

### `tagTypoMaxDistance :: Int`

Maximum Levenshtein edit distance for fuzzy tag matching. Set to 3.

**Called by:** Tags.guessTagFromShort (tag inference logic)
**Calls:** N/A (constant)

### `tagGuessBlacklist :: String -> Bool`

Predicate that returns `True` for paths that should not have tags auto-inferred from their directory structure. Covers project archives and mirrors.

**Called by:** Tag guessing logic in Tags module
**Calls:** `Utils.anyPrefix`

### `tagListBlacklist :: [String]`

List of directory names to exclude from tag lists. Prevents archive directories from appearing as tags.

**Called by:** Tag listing/rendering
**Calls:** N/A (constant)

### `urlTagDB :: [(String -> Bool, String)]`

Lookup table mapping URL patterns to tags. Enables automatic tagging of links to known sites (e.g., "https://publicdomainreview.org/" → "history/public-domain-review").

**Called by:** Annotation scraping pipeline
**Calls:** `isPrefixOf`, `isInfixOf`, `Utils.anyInfix`

### `wholeTagRewritesRegexes :: [(String, String)]`

Regex-based tag transformations for display. Handles capitalization (cs → CS, ai → AI), semantic rewrites (genetics/selection → evolution), and HTML formatting (anime/eva → `<em>NGE</em>`).

**Called by:** Tag rendering
**Calls:** N/A (constant)

### `tagsShort2Long :: [(String, String)]`

Master alias table mapping short/alternate forms to canonical paths. Combines manual rewrites with auto-generated inverses from `tagsLong2Short`.

**Called by:** Tag normalization, typo correction
**Calls:** `tagsShort2LongRewrites`, `tagsLong2Short`

### `tagsLong2Short :: [(String, String)]`

Display name mappings from canonical paths to human-readable labels. First match wins (more specific paths listed first via `reverse`).

**Called by:** Tag rendering, HTML generation
**Calls:** N/A (constant)

### `shortTagBlacklist :: [String]`

Common English words and HTML fragments that should never be interpreted as tags.

**Called by:** `Tags.guessTagFromShort`
**Calls:** N/A (constant)

### `shortTagTestSuite :: [(String, String)]`

Test cases for tag resolution. Used by the build system's test suite to verify alias mappings work correctly.

**Called by:** Test harness
**Calls:** N/A (constant)

---

## Internal Architecture

### Data Flow

```
User input (annotation, URL, directory path)
         │
         ▼
┌─────────────────────────┐
│ tagGuessBlacklist       │ ─── Skip if path matches blacklist
└─────────────────────────┘
         │
         ▼
┌─────────────────────────┐
│ tagsShort2Long          │ ─── Normalize aliases/typos to canonical form
└─────────────────────────┘
         │
         ▼
┌─────────────────────────┐
│ wholeTagRewritesRegexes │ ─── Apply display transformations
└─────────────────────────┘
         │
         ▼
┌─────────────────────────┐
│ tagsLong2Short          │ ─── Convert to human-readable label
└─────────────────────────┘
```

### Alias Table Structure

`tagsShort2LongRewrites` handles several categories:

1. **Typo corrections**: "gpt4" → "ai/nn/transformer/gpt/4"
2. **Synonyms**: "dogs" → "dog", "psychedelics" → "psychedelic"
3. **Abbreviation expansion**: "mr" → "genetics/heritable/correlation/mendelian-randomization"
4. **Legacy path migration**: "ai/clip" → "ai/nn/transformer/clip"
5. **Common misspellings**: Extensive anencephaly variants (~50 entries)

### URL Tag Database

`urlTagDB` uses three matching strategies:

- **Prefix matches**: Domain-specific (e.g., "https://publicdomainreview.org/")
- **Infix matches**: Technology sites (e.g., "r-project.org" anywhere in URL)
- **Special cases**: Multi-domain patterns via predicates (e.g., EVA wiki sites)

---

## Key Patterns

**Hierarchical tag paths**: Tags use slash-separated paths that mirror both the site's directory structure and conceptual taxonomy. "ai/nn/transformer/gpt/4/poetry" encodes: domain (AI) → architecture (neural network) → family (transformer) → model (GPT) → version (4) → content type (poetry).

**First-match priority**: `tagsLong2Short` is reversed so more specific paths match before their parents. Without this, "ai/nn/transformer/gpt/4" would match the generic "GPT" entry before "GPT-4".

**Bidirectional generation**: `tagsShort2Long` auto-generates reverse mappings from display names that don't contain special characters (spaces, HTML tags, parentheses), reducing maintenance burden.

**Aggressive typo tolerance**: The 3-character edit distance combined with ~200 explicit typo mappings means common errors like "gpt4" or "transfomer" resolve correctly.

---

## Configuration

All configuration is compile-time via the literal tables in this module. To add:

- **New tag alias**: Add entry to `tagsShort2LongRewrites`
- **New display name**: Add entry to `tagsLong2Short` (specific paths first)
- **New URL auto-tag**: Add to `urlTagDB` prefix/infix/special sections
- **Block a tag**: Add to `tagGuessBlacklist` or `tagListBlacklist`

---

## Integration Points

### Consumers

- **[link-metadata-hs](link-metadata-hs)**: Uses `urlTagDB` for automatic tag inference from URLs
- **[annotation-hs](annotation-hs)**: Applies tag normalization when processing metadata
- **Tags.hs**: Primary consumer of all exports for tag resolution logic
- **HTML templates**: Use `tagsLong2Short` for rendering tag links

### Related Modules

- **Utils.hs**: Provides `anyPrefix` and `anyInfix` helpers
- **Tags.hs**: Contains business logic that uses these config tables

---

## See Also

- [Tags.hs](/backend/tags-hs) - Core tag manipulation logic consuming this configuration
- [guessTag.hs](/backend/guess-tag-hs) - CLI tool using `tagsShort2Long` for tag expansion
- [changeTag.hs](/backend/change-tag-hs) - CLI tool for tag operations using these mappings
- [Annotation.hs](/backend/annotation-hs) - Annotation processing that uses tag inference
- [LinkMetadata.hs](/backend/link-metadata-hs) - Metadata management with tag support
- [Hakyll.hs](/backend/hakyll-hs) - Site generator that renders tag links
- [Utils.hs](/backend/utils-hs) - Provides `anyPrefix` and `anyInfix` helpers
