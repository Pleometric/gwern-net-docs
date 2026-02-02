
# duplicatequotesitefinder.hs

**Path:** `build/duplicatequotesitefinder.hs` | **Language:** Haskell | **Lines:** ~47

> Near-duplicate detection for quote and site-of-the-day databases using Levenshtein edit distance

---

## Overview

This is a standalone lint tool that scans the "quote of the day" and "site of the day" databases for near-duplicate entries. It uses Levenshtein edit distance to identify entries that are suspiciously similar, helping maintainers catch accidental duplicates before they appear on the live site.

The tool reads the same Haskell-serialized tuple databases (`metadata/quotes.hs` and `metadata/sites.hs`) used by [XOfTheDay](x-of-the-day-hs) and computes pairwise edit distances between all entries. Any pairs below configurable thresholds are reported for manual review. This is a quality assurance tool, not part of the main build pipeline—it's meant to be run periodically or before adding new entries.

The implementation is intentionally simple: a runghc script with no optimization. Since the databases are small (hundreds to low thousands of entries), the O(n²) comparison is acceptable for occasional use.

---

## Public API

This is a standalone executable with no library API.

### `main :: IO ()`

Entry point. Reads both databases, computes near-duplicate pairs, and prints matches to stdout.

**Called by:** Manual invocation (`runghc duplicatequotesitefinder.hs`)
**Calls:** `readData`, `sortData`, `rankData`

---

## Internal Architecture

### Data Types

```haskell
type Site = (String, String, Bool)
-- (content, attribution/name, has-been-used)
-- Same structure as Quote (both are "Snippets" in XOfTheDay terminology)
```

### Core Functions

#### `readData :: Read a => FilePath -> IO [a]`

Reads a Haskell-serialized database file using `read`. Returns empty list on parse failure (graceful degradation).

#### `sortData :: [Site] -> [Site]`

Sorts entries alphabetically by their content field. This is primarily for deterministic output ordering.

#### `rankData :: Int -> [Site] -> [(Site, [(Site, Int)])]`

The core duplicate detection logic. For each entry:
1. Computes Levenshtein distance to every other entry
2. Filters to only those within `maxDist` threshold
3. Sorts matches by distance (closest first)
4. Returns tuples of `(entry, [(similar_entry, distance), ...])`

### Control Flow

```
main
 ├── Read sites.hs database
 ├── Sort sites alphabetically
 ├── Compute all pairwise distances ≤ maxDistanceSite (3)
 ├── Filter to entries with ≥1 match
 ├── Print site duplicates
 │
 ├── Read quotes.hs database
 ├── Sort quotes alphabetically
 ├── Filter quotes shorter than minQuoteLength (26 chars)
 ├── Compute all pairwise distances ≤ maxDistanceQuote (18)
 ├── Filter to entries with ≥1 match
 └── Print quote duplicates
```

---

## Key Patterns

### Brute-Force Pairwise Comparison

The implementation uses a simple list comprehension for O(n²) comparison:

```haskell
[(other, levenshteinDistance defaultEditCosts d o)
 | other@(o, _, _) <- dta, datum /= other]
```

No optimization (like blocking, shingling, or locality-sensitive hashing) is used because the databases are small enough that brute force is acceptable. This keeps the code simple and correct.

### Asymmetric Thresholds

Sites use a much tighter threshold (3 edits) than quotes (18 edits). This reflects:
- **Sites**: Short URLs where even small differences matter (3 chars can distinguish `foo.com` from `bar.com`)
- **Quotes**: Longer text where minor wording variations might indicate true duplicates (typos, reformatting)

### Minimum Length Filter for Quotes

Short quotes are excluded (`length q > 26`) to avoid false positives. Very short strings will naturally have small edit distances even when semantically distinct.

---

## Configuration

All configuration is via compile-time constants:

| Constant | Value | Purpose |
|----------|-------|---------|
| `maxDistanceQuote` | 18 | Max Levenshtein distance for quote matches |
| `maxDistanceSite` | 3 | Max Levenshtein distance for site matches |
| `minQuoteLength` | 26 | Minimum quote length to consider |

Database paths come from `Config.XOfTheDay`:
- `quoteDBPath` = `metadata/quotes.hs`
- `siteDBPath` = `metadata/sites.hs`

---

## Integration Points

### Dependencies

- **Config.XOfTheDay**: Provides database paths
- **Text.EditDistance**: External package for Levenshtein computation (`defaultEditCosts` = standard substitution/insertion/deletion costs of 1)

### Shared State

Reads (but does not modify) the same databases as [XOfTheDay](x-of-the-day-hs). The tuple format is:

```haskell
(String, String, Bool)
-- (content, attribution, has-been-used)
```

### Output Format

Prints Haskell `show` output for each entry with matches:

```haskell
(("original quote", "attribution", False),
 [("similar quote", "attribution", True), 8),
  ("another similar", "attr", False), 12)])
```

Human review is required to determine if matches are true duplicates.

---

## See Also

- [XOfTheDay.hs](/backend/x-of-the-day-hs) - The system that uses these databases
- [Utils.hs](/backend/utils-hs) - Shared utility functions
- [sync.sh](/backend/sync-sh) - Build orchestrator (may invoke this as a lint step)
- [Test.hs](/backend/test-hs) - Test suite that validates quote/site databases
- [Unique.hs](/backend/unique-hs) - Uniqueness checking utilities
