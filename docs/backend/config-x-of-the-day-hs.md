
# Config.XOfTheDay

**Path:** `build/Config/XOfTheDay.hs` | **Language:** Haskell | **Lines:** ~499

> Configuration constants for the "X of the Day" feature: paths, thresholds, and domain exclusions

---

## Overview

This module centralizes all configuration for gwern.net's "X of the Day" features—Quote of the Day, Site of the Day, and Annotation of the Day. Each feature displays a rotating selection of curated content on the homepage, drawn from metadata databases.

The module defines three categories of configuration: (1) file paths to the source databases and output HTML files, (2) quality thresholds that filter which entries qualify for rotation, and (3) a comprehensive domain blacklist preventing certain sites from appearing in the Site of the Day feature.

The blacklist is notably large (~460 domains) and excludes common platforms (GitHub, Wikipedia, Twitter/X), major publications (NYT, Guardian, Nature), academic repositories (arXiv, JSTOR), and gwern.net itself—the rationale being these are either too common to be interesting discoveries or would create self-referential recommendations.

---

## Public API

### `quoteDBPath :: FilePath`
Path to the quote database: `metadata/quotes.hs`

### `quotePath :: FilePath`
Output path for today's quote: `metadata/today-quote.html`

### `siteDBPath :: FilePath`
Path to the site database: `metadata/sites.hs`

### `sitePath :: FilePath`
Output path for today's site: `metadata/today-site.html`

### `annotDayDB :: String`
Path to the annotation database: `metadata/annotations.hs`

### `annotPath :: String`
Output path for today's annotation: `metadata/today-annotation.html`

### `minAnnotationAbstractLength :: Int`
Minimum character length for annotation abstracts to qualify: **2000**

Comment notes this threshold yielded 3,313 qualifying entries (vs 10,046 at >500 chars).

### `siteLinkMin :: Int`
Minimum number of links required for a site to qualify: **3**

### `siteBlackList :: [T.Text]`
List of domain names excluded from Site of the Day recommendations.

---

## Internal Architecture

The module contains no logic—it's purely declarative constants. The blacklist is a simple list literal of `T.Text` domain strings, designed for O(n) membership checks (or conversion to a `Set` by consumers).

### Database File Format

The `.hs` extension on database files suggests they use Haskell's `Read`/`Show` serialization rather than JSON or YAML. This allows type-safe deserialization directly into Haskell data structures.

### Output Files

The `today-*.html` files are pre-rendered HTML fragments included in page templates, regenerated during each build cycle.

---

## Key Patterns

**Threshold Tuning Comments**: The module includes inline documentation about threshold selection rationale:

```haskell
-- at >500, yielded 10,046 on 2023-03-08; >2,000 yielded a more reasonable 3,313
```

This captures the empirical reasoning for configuration choices, useful for future adjustments.

**Domain Blacklist Organization**: Domains are listed alphabetically within chunks of ~5 per line, making diffs readable and merges less conflicting.

---

## Configuration

All values are compile-time constants. To modify:

| Setting | Current Value | Effect |
|---------|---------------|--------|
| `minAnnotationAbstractLength` | 2000 | Raises bar: fewer but meatier annotations |
| `siteLinkMin` | 3 | Ensures sites have meaningful presence in annotations |
| `siteBlackList` | ~460 domains | Prevents generic/common sites from appearing |

---

## Integration Points

**Consumers**: The build system modules that generate X of the Day content import these constants:

- Selection logic uses thresholds to filter candidates
- HTML generators write to the configured output paths
- Recommendation engine checks blacklist before selecting sites

**Related metadata files**:
- `metadata/quotes.hs` - Quote database
- `metadata/sites.hs` - Site recommendation database
- `metadata/annotations.hs` - Full annotation database

---

## See Also

- [XOfTheDay.hs](/backend/x-of-the-day-hs) - Main module consuming this configuration
- [Hakyll.hs](/backend/hakyll-hs) - Site generator that invokes XOTD generation
- [Annotation.hs](/backend/annotation-hs) - Populates the annotation database this module references
- [Config.Misc](/backend/config-misc-hs) - Related configuration module with global constants
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database used for annotation-of-the-day
- [Blog.hs](/backend/blog-hs) - Related daily content system for blog posts
- [sync.sh](/backend/sync-sh) - Build orchestrator coordinating content generation
