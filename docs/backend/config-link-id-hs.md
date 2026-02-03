
# Config.LinkID

**Path:** `build/Config/LinkID.hs` | **Language:** Haskell | **Lines:** ~29

> Configuration for citation ID generation overrides and affiliation anchor lists

---

## Overview

Config.LinkID provides two static configuration lists that support the citation ID generation system in [link-id-hs](link-id-hs). Citation IDs are human-readable identifiers like `smith-2020` or `jones-et-al-2021` used for backlinks, anchor targets, and the `/ref/` URL system.

The module addresses two specific needs: (1) a list of known organizational affiliations used to detect duplicate annotation URLs that differ only by affiliation hash fragments, and (2) manual overrides for URLs where automatic ID generation produces collisions or suboptimal results.

This is a pure data module with no logic—just lists that are consumed by LinkID.hs and LinkMetadata.hs during the build process.

---

## Public API

### `affiliationAnchors :: [String]`

A list of ~50 organization identifiers (AI labs, tech companies, research institutions) used as URL hash fragments to tag papers by author affiliation.

```haskell
affiliationAnchors = ["ai21", "adobe", "alibaba", "allen", "amazon", "anthropic", ...]
```

**Called by:**
- `LinkMetadata.findDuplicatesURLsByAffiliation` — detects papers annotated multiple times with different affiliation tags
- `Test.hs` — validates uniqueness

**Purpose:** When a paper like `/doc/ai/2020-foo.pdf` has multiple annotations (`#deepmind`, `#google`, `#openai`), this list helps identify such duplicates for cleanup. The system looks for URLs containing `#org` or `org=org` patterns.

---

### `linkIDOverrides :: [(String, T.Text)]`

Manual mapping from URLs to citation IDs for cases where automatic generation fails.

```haskell
linkIDOverrides = [
  ("/gpt-2-music", "gwern-presser-2019-music")
]
```

**Called by:**
- `LinkID.generateID` — checked first before attempting automatic ID generation
- `Test.hs` — validates uniqueness of keys and values, checks ID format rules

**Purpose:** Some URLs don't have sufficient metadata for good automatic IDs, or automatic generation produces collisions. This list provides escape-hatch overrides. The example shown disambiguates a co-authored page that would otherwise collide with other GPT-2 content.

---

## Internal Architecture

The module is purely declarative—two exported constant lists with no computation.

### Data Structures

**affiliationAnchors:** Flat list of lowercase organization slugs. Must be unique (enforced by tests). Covers major AI labs and tech companies circa 2020-2024.

**linkIDOverrides:** Association list of `(URL, ID)` pairs where:
- Keys are relative URLs (paths) starting with `/`
- Values are citation-style IDs starting with letters, containing only alphanumerics and hyphens
- No periods allowed in values (CSS/JS compatibility)
- Values must not be URLs themselves (prevents accidental swaps)

---

## Key Patterns

### Affiliation-Based Duplicate Detection

The affiliation list powers a specific quality-control check: finding papers that appear multiple times in the metadata database with different organizational tags. For example, a Google Brain / DeepMind collaboration might be annotated at both:
- `/doc/ai/2020-foo.pdf#google`
- `/doc/ai/2020-foo.pdf#deepmind`

This is usually unintentional duplication that should be consolidated.

### Override Precedence

The override lookup in `generateID` happens before any other ID generation logic:

```haskell
generateID md url author date
  | any (\(u,_) -> u == url) C.linkIDOverrides =
      fromJust $ lookup url C.linkIDOverrides
  | otherwise = ... -- automatic generation
```

This makes overrides absolute—no fallback or combination with automatic generation.

---

## Configuration

Both lists are edited directly in source. Adding new entries:

**For affiliationAnchors:**
```haskell
affiliationAnchors = [...existing..., "neworg"]
```

**For linkIDOverrides:**
```haskell
linkIDOverrides = [
  ...existing...
  , ("/problematic-url", "desired-citation-id")
]
```

### Validation Rules (enforced by Test.hs)

| List | Constraint |
|------|------------|
| affiliationAnchors | Unique entries |
| linkIDOverrides | Unique keys, unique values |
| linkIDOverrides | Keys must be valid URIs |
| linkIDOverrides | Values must NOT be URLs |
| linkIDOverrides | Values must start with `[a-zA-Z]` |
| linkIDOverrides | Values must not contain `.` |

---

## Integration Points

### Consumers

| Module | Usage |
|--------|-------|
| [link-id-hs](link-id-hs) | Checks overrides before generating IDs |
| [link-metadata-hs](link-metadata-hs) | Uses affiliation list for duplicate detection |
| Test.hs | Validates both lists for correctness |

### Shared State

None—pure configuration data with no runtime state.

### Build Integration

The lists are compiled into the Hakyll build executable. Changes require rebuild but no external file updates.

---

## See Also

- [LinkID.hs](/backend/link-id-hs) - ID generation logic that consumes these configs
- [LinkMetadata.hs](/backend/link-metadata-hs) - Metadata database that uses affiliation detection
- [Annotation.hs](/backend/annotation-hs) - Broader annotation system context
- [hakyll.hs](/backend/hakyll-hs) - Build system that generates ID databases
- [link-suggester.hs](/backend/link-suggester-hs) - Uses citation IDs for deduplication
- [link-extractor.hs](/backend/link-extractor-hs) - Extracts URLs that receive IDs
