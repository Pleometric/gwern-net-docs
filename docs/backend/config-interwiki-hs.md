
# Config.Interwiki

**Path:** `build/Config/Interwiki.hs` | **Language:** Haskell | **Lines:** ~6,000

> Wikipedia redirect bypass database and interwiki link test cases

---

## Overview

Config.Interwiki is a massive data module containing over 5,500 Wikipedia URL redirect mappings and test cases for the interwiki link system. It exists to solve two problems: Wikipedia redirects slow down page loads and create inconsistent link targets, and interwiki link transformations need comprehensive testing.

The redirect database maps Wikipedia article URLs to their canonical targets, bypassing Wikipedia's redirect system entirely. This improves performance (no redirect hop), ensures consistency for link suggestion deduplication, and eliminates false positives in link checker runs. The database also handles disambiguation by mapping ambiguous terms to their intended articles (e.g., "Depression" → "Depression_(mood)", "Newton" → "Isaac_Newton").

The module is pure configuration data with no logic—all processing happens in [interwiki-hs](interwiki-hs). This separation keeps the 6,000-line data file from cluttering the transformation logic.

---

## Public API

### `testCases :: [(Inline, Inline)]`

Test case pairs for verifying interwiki link transformations.

Each tuple contains (input link, expected output link) as Pandoc `Inline` elements. The test suite in [interwiki-hs](interwiki-hs) runs these to verify transformations work correctly.

**Called by:** `Interwiki.interwikiTestSuite`
**Calls:** Pandoc constructors (`Link`, `Str`, `Emph`, `Strong`)

### `quoteOverrides :: [T.Text]`

Article titles where trailing possessive (`'s`) should NOT be stripped.

Default behavior strips `'s` from link text to derive article names (e.g., "George Washington's" → "George Washington"). These overrides preserve the possessive because it's part of the official title.

**Examples:** `["Antoine's", "Bloomingdale's", "Collier's", "Denny's"]`

**Called by:** `Interwiki.wpURLRewrites`

### `redirectDB :: [(T.Text, T.Text)]`

Massive mapping of Wikipedia URLs to their canonical targets.

Each tuple is `(source_article, canonical_article)`. Both are either article names (prefixed with `https://en.wikipedia.org/wiki/` internally) or full URLs for non-Wikipedia targets (e.g., Internet Archive for deleted articles).

**Called by:** `Interwiki.wpURLRedirectRewrites`, `Interwiki.interwikiTestSuite`, `Interwiki.interwikiCycleTestSuite`

---

## Internal Architecture

### Data Structure

The file is essentially one giant list literal with 5,500+ entries:

```haskell
redirectDB :: [(T.Text, T.Text)]
redirectDB = let wp u = if "http" `T.isPrefixOf` u then u
                        else T.append "https://en.wikipedia.org/wiki/" u in
             map (\(a,b) -> (wp a, wp b)) $ [
    ("WP:RS", "Wikipedia:Reliable_sources")
  , ("9/11", "September_11_attacks")
  , ("ADHD", "Attention_deficit_hyperactivity_disorder")
  -- ... 5,500 more entries
  ] ++ -- disambiguation overrides:
  [ ("Depression", "Depression_(mood)")
  , ("Newton", "Isaac_Newton")
  -- ... 400 more
  ]
```

### Entry Categories

The redirects fall into distinct categories:

1. **Redirect bypasses**: Skip Wikipedia's redirect mechanism
   - `("9/11", "September_11_attacks")`
   - `("AIDS", "HIV/AIDS")`

2. **URL encoding fixes**: Normalize URL-encoded characters
   - `("%C3%80_la_recherche_du_temps_perdu", "In_Search_of_Lost_Time")`

3. **Disambiguation overrides**: Force specific meaning for ambiguous terms
   - `("Depression", "Depression_(mood)")` — not the economic kind
   - `("Newton", "Isaac_Newton")` — not the unit or city

4. **Deleted article recoveries**: Point to Internet Archive snapshots
   - `("Mnet", "https://web.archive.org/web/...")`

5. **Naming convention fixes**: Handle capitalization and formatting
   - `("Covid-19", "COVID-19_pandemic")`
   - `("MacOS", "macOS")`

---

## Key Patterns

### The `wp` Helper

The internal `wp` function provides syntactic sugar for entries:

```haskell
let wp u = if "http" `T.isPrefixOf` u then u
           else T.append "https://en.wikipedia.org/wiki/" u
```

This allows writing just article names (`"ADHD"`) instead of full URLs, while still permitting full URLs for external targets (Internet Archive, other Wikimedia projects).

### Disambiguation Section

The database explicitly separates redirect bypasses from disambiguation overrides with a list concatenation:

```haskell
  ] ++ -- disambiguation overrides:
  [ ("Depression", "Depression_(mood)")
```

This makes the intent clear: bypasses fix Wikipedia's redirect graph, while disambiguation overrides encode editorial decisions about default meanings.

### Test Case Structure

Test cases use Pandoc AST constructors directly:

```haskell
testCases = [
  (Link nullAttr [Str "Pondicherry"] ("!Wikipedia",""),
   Link ("", ["link-live"], []) [Str "Pondicherry"]
        ("https://en.wikipedia.org/wiki/Pondicherry", ""))
]
```

This tests the full transformation: shortcut expansion, class assignment, URL construction.

---

## Configuration

This module IS the configuration. Key tuning points:

| Data | Purpose | When to Modify |
|------|---------|----------------|
| `redirectDB` | URL mappings | Wikipedia renames an article, or new disambiguation needed |
| `quoteOverrides` | Possessive exceptions | New article title ends in `'s` |
| `testCases` | Transformation tests | New interwiki prefix or edge case discovered |

### Adding a Redirect

```haskell
-- In redirectDB list:
, ("Old_Article_Name", "New_Canonical_Name")

-- For disambiguation:
, ("Ambiguous_Term", "Specific_Article_(disambiguation)")

-- For deleted articles:
, ("Deleted_Article", "https://web.archive.org/web/TIMESTAMP/...")
```

### Validation

The [interwiki-hs](interwiki-hs) module runs validation:

1. **Key uniqueness**: No duplicate source URLs
2. **Cycle detection**: No A→B→A redirect chains
3. **URL validity**: All targets produce valid URLs

---

## Integration Points

### Consumers

| Module | Usage |
|--------|-------|
| [interwiki-hs](interwiki-hs) | Imports all three exports, applies redirects during link transformation |
| Test suite | Validates test cases produce expected output |

### Data Flow

```
Markdown: [foo](!W)
    ↓
Interwiki.convertInterwikiLinksInline
    ↓
Lookup "foo" in redirectDB → canonical URL
    ↓
HTML: <a href="https://en.wikipedia.org/wiki/Canonical_Article">foo</a>
```

### Shared State

None. Pure data module with no mutable state.

---

## See Also

- [Interwiki.hs](/backend/interwiki-hs) - The transformation logic that consumes this data
- [LinkAuto.hs](/backend/link-auto-hs) - Generates interwiki links that trigger redirects
- [Config.LinkAuto](/backend/config-link-auto-hs) - Auto-linking patterns that produce !W links
- [LinkMetadata.hs](/backend/link-metadata-hs) - Uses Wikipedia URLs for annotation lookup
- [Typography.hs](/backend/typography-hs) - Also performs Pandoc AST transformations
- [hakyll.hs](/backend/hakyll-hs) - Build system that orchestrates link processing
