
# XOfTheDay.hs

**Path:** `build/XOfTheDay.hs` | **Language:** Haskell | **Lines:** ~153

> Daily rotating featured content system: quotes, sites, and annotations

---

## Overview

XOfTheDay implements a "quote of the day" style system extended to three content types: **quotes**, **sites** (external links), and **annotations**. Each builds on the same core pattern: maintain a database of items, track which have been "used," pick one per build, mark it used, and write an HTML snippet file for transclusion.

The module solves the duplicate problem inherent in random selection. With simple random sampling from 366 quotes, a daily visitor has 50% odds of seeing a duplicate after only 23 visits (birthday problem). Instead, XOfTheDay cycles through items exhaustively—each is shown exactly once before the cycle resets—providing guaranteed O(n) coverage rather than probabilistic O(n log n).

Design priorities: simplicity (plain Haskell `read`/`show` databases), determinism (smallest-first selection for quotes, longest-first for annotations), and minimal external dependencies (just file I/O and Pandoc for HTML generation).

---

## Public API

### `qotd :: IO ()`

Generate quote-of-the-day. Reads quote database, selects smallest unused quote, writes HTML epigraph snippet to `metadata/today-quote.html`, updates database.

**Called by:** Build system (sync.sh, hakyll.hs)
**Calls:** `generateSnippetAndWriteTTDB`, `quoted`

### `sotd :: IO ()`

Generate site-of-the-day. Same pattern as `qotd` but outputs a hyperlink snippet to `metadata/today-site.html`.

**Called by:** Build system
**Calls:** `generateSnippetAndWriteTTDB`, `sited`

### `aotd :: Metadata -> IO ()`

Generate annotation-of-the-day. Unlike quotes/sites, requires the full annotation metadata as input. Selects the longest unshown annotation by abstract length, writes include snippet to `metadata/today-annotation.html`.

```haskell
aotd :: Metadata -> IO ()
aotd md = do am <- readArchiveMetadata
             generateAnnotationOfTheDay md C.annotDayDB C.annotPath (annotated am)
```

**Called by:** Build system (must pass Metadata)
**Calls:** `generateAnnotationOfTheDay`, `annotated`, `readArchiveMetadata`

### `sitePrioritize :: IO [T.Text]`

Utility to find candidate sites for the site-of-the-day database. Scans backlinks database, counts domain frequencies, filters by minimum threshold (`siteLinkMin`), excludes blacklisted domains, returns domains sorted by link count descending.

**Called by:** Manual curation workflow
**Calls:** `readBacklinksDB`, `readTTDB`

---

## Internal Architecture

### Database Types

```haskell
type TTDB = [Snippet]
type Snippet = (String, String, Bool)  -- (content, attribution, used?)

type AotD = [String]  -- list of URLs already shown
```

Quotes and sites share the `Snippet` tuple format. The third element is a "used" flag that cycles: when all items are marked `True`, they reset to `False` and the cycle begins again.

Annotations use a simpler append-only list of URLs (`AotD`). The selection algorithm filters against this list rather than toggling flags.

### Selection Algorithm (Quotes/Sites)

```
1. Read database as Set
2. Filter to unused items (status = False)
3. If all used, negate entire set (reset cycle)
4. Sort by content length, take smallest (quote) or arbitrary (site)
5. Format as HTML, write to snippet file
6. Toggle selected item's flag, write database
```

The "smallest quote first" heuristic provides variation—long epigraphs appear later in cycles—and makes cycles feel fresh rather than front-loaded.

### Selection Algorithm (Annotations)

```
1. Filter Metadata to:
   - Abstract length > minAnnotationAbstractLength (2000 chars)
   - Not by "Gwern Branwen" (no self-promotion)
   - Not already in shown-list
   - Not a /blog/ or /index page
2. Sort by abstract length ascending
3. Take last (longest) annotation
4. Append URL to shown-list, write database
```

Longest-first ensures the most substantive annotations get featured. Unlike quotes/sites, there's no reset—once shown, an annotation stays in the "used" list permanently (though the list could be manually cleared).

### HTML Formatters

```haskell
quoted :: Snippet -> String  -- wraps in <div class="epigraph quote-of-the-day"><blockquote>...
sited :: Snippet -> String   -- wraps in <div class="site-of-the-day"><blockquote><p><a href=...
annotated :: ArchiveMetadata -> String -> String  -- generates Pandoc include link
```

`annotated` is more complex: it runs through `localizeLink` (archive resolution), `linkIcon` (icon decoration), and Pandoc to produce a partial-include annotation link.

---

## Key Patterns

### Coupon Collector Efficiency

The comment explains the math: to see all n items with random sampling requires E[n log n] draws. With 1,495 quotes, that's ~10,929 visits to guarantee all shown vs exactly 1,495 with exhaustive cycling. The module guarantees optimal coverage.

### Unsafe IO in Pure Context

`annotated` uses `unsafePerformIO` to call `localizeLink` within what's otherwise a pure formatter:

```haskell
annotated :: ArchiveMetadata -> String -> String
annotated a url = Unsafe.unsafePerformIO $ do
  lnk <- localizeLink a $ linkIcon $ Link ...
```

This is pragmatic—the archive lookup needs IO, but the type signature wants `String -> String` to match the `formatter` parameter of `generateSnippetAndWriteTTDB`. Safe here because the operation is idempotent.

### Set-Based State Toggle

The `snegate` helper flips a single item's flag:

```haskell
snegate :: Snippet -> Snippet
snegate (a,b,s) = (a,b,not s)
```

Bulk reset is `S.map snegate db`. The Set operations ensure no duplicates and make the "find-and-replace" pattern clean.

---

## Configuration

All paths and thresholds live in `Config.XOfTheDay`:

| Constant | Value | Purpose |
|----------|-------|---------|
| `quoteDBPath` | `metadata/quotes.hs` | Quote database file |
| `quotePath` | `metadata/today-quote.html` | Quote output snippet |
| `siteDBPath` | `metadata/sites.hs` | Site database file |
| `sitePath` | `metadata/today-site.html` | Site output snippet |
| `annotDayDB` | `metadata/annotations.hs` | Annotation tracking file |
| `annotPath` | `metadata/today-annotation.html` | Annotation output snippet |
| `minAnnotationAbstractLength` | 2000 | Minimum chars for annotation eligibility |
| `siteLinkMin` | 3 | Minimum backlinks for site recommendation |
| `siteBlackList` | ~460 domains | Excluded from site-of-the-day candidates |

The `siteBlackList` is extensive—major platforms (GitHub, Reddit, Wikipedia, arxiv), news sites, gwern.net itself, and various common domains that wouldn't make interesting "discoveries."

---

## Integration Points

### Input Dependencies

- **Metadata** (for `aotd`): The full annotation database, passed as parameter
- **ArchiveMetadata** (for `annotated`): Read via `readArchiveMetadata` from LinkArchive
- **BacklinksDB** (for `sitePrioritize`): Read via `readBacklinksDB` from LinkBacklink

### Output Files

All three functions write to `metadata/today-*.html` files. These are transcluded into pages via:

```html
<div class="qotd">
  <a class="include" href="/metadata/today-quote.html">Quote Of The Day</a>
</div>
```

The JavaScript transclusion system (`transclude.js`) fetches and injects these snippets client-side.

### CSS Classes

- `epigraph quote-of-the-day` — quote wrapper
- `site-of-the-day` — site link wrapper
- `annotation-of-the-day` — annotation include wrapper
- `include-annotation-partial`, `link-annotated`, `backlink-not`, `include-spinner-not` — annotation link classes

### Typography Processing

Both `quoted` and `sited` run content through `typesetHtmlField` from Typography.hs for proper quote marks, small caps, etc.

---

## See Also

- [Config.XOfTheDay](/backend/config-x-of-the-day-hs) - Configuration: paths, thresholds, and domain blacklist
- [Hakyll.hs](/backend/hakyll-hs) - Site generator that invokes XOTD generation
- [Typography.hs](/backend/typography-hs) - `typesetHtmlField` used for text formatting
- [LinkMetadata.hs](/backend/link-metadata-hs) - Metadata type consumed by `aotd`
- [LinkArchive.hs](/backend/link-archive-hs) - `localizeLink` and `readArchiveMetadata`
- [transclude.js](/frontend/transclude-js) - Client-side transclusion of output snippets
- [Blog.hs](/backend/blog-hs) - Related content generation system for blog posts
