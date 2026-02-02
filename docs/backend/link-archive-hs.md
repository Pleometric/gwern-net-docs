
# LinkArchive.hs

**Path:** `build/LinkArchive.hs` | **Language:** Haskell | **Lines:** ~395

> External link localization for preemptive link-rot prevention

**Config:** `build/Config/LinkArchive.hs` | **Lines:** ~700+ (mostly whitelist)

---

## Overview

LinkArchive.hs implements "preemptive local archiving"—the strategy of mirroring external URLs to local copies before they die. When gwern.net links to an external page, the system archives it to `/doc/www/` and rewrites the link to point to the local copy. This prevents link rot from affecting readers and ensures cited sources remain accessible.

The archive currently holds ~47GB across ~14,000+ snapshots (as of 2023), growing continuously. Archives are created using SingleFile (a Chromium extension that serializes DOMs to self-contained HTML files) or direct PDF downloads. The system is intentionally conservative: new URLs wait 60 days before archiving to allow content to stabilize, and many domains are whitelisted to skip archiving entirely.

Key design decisions:
- **Delayed archiving**: URLs are only archived 60+ days after first being seen, avoiding archiving draft posts or volatile content
- **SHA1 file naming**: Archive paths are `doc/www/$DOMAIN/SHA1($URL).html`, enabling deterministic lookup without database queries
- **Whitelist-heavy**: ~800+ domain patterns are skipped (Wikipedia, interactive services, video sites, etc.)
- **Dual-link system**: HTML links to local archive, but JS rewrites to original on page load (best of both worlds for readers)

---

## Public API

### `localizeLink :: ArchiveMetadata -> Inline -> IO Inline`

Main transform function. Takes a Pandoc `Link` inline and returns a modified version with archive attributes.

```haskell
-- Input:  Link nullAttr [] ("https://example.com/article", "")
-- Output: Link ("", [], [("data-url-archive", "/doc/www/example.com/abc123.html"),
--                        ("data-url-original", "https://example.com/article")])
--              [] ("/doc/www/example.com/abc123.html", "")
```

**Called by:** `hakyll.hs:pandocTransform` (line 348)
**Calls:** `rewriteLink`, `Config.LinkArchive.whiteList`, `transformURLsForMobile`, `transformURLsForLiveLinking`

### `localizeLinkURL :: ArchiveMetadata -> FilePath -> IO FilePath`

Simplified version of `localizeLink`—returns just the archive path (or original URL if not archived).

```haskell
localizeLinkURL am "https://news.ycombinator.com/item?id=123"
-- → "doc/www/news.ycombinator.com/abc123def.html"
```

**Called by:** `LinkMetadata.hs`

### `readArchiveMetadata :: IO ArchiveMetadata`

Fast path: reads `metadata/archive.hs` into memory as a `Map`.

**Called by:** `hakyll.hs:main`, `manualArchive`, `XOfTheDay.hs`

### `readArchiveMetadataAndCheck :: IO ArchiveMetadata`

Slow path with validation: checks that archive files exist, aren't empty, and checksums match URLs.

**Called by:** `hakyll.hs:main` (production builds), `Test.hs`

### `manualArchive :: Int -> IO ()`

Archives the first `n` pending URLs that are due. Used for batch archiving:

```bash
cd ~/wiki/ && ghci -istatic/build/ ./static/build/LinkArchive.hs -e 'manualArchive 10'
```

### `testLinkRewrites :: IO [(Inline, Inline)]`

Returns failing test cases (expected vs actual). Empty list = all tests pass. Uses `Config.LinkArchive.localizeLinkTestDB` (mock archive database) and `localizeLinktestCases` (test vectors).

### `calculateArchiveSizePercentiles :: ArchiveMetadata -> IO SizeDB`

Computes file sizes and percentile ranks for all archived URLs. Used for UI indicators.

---

## Internal Architecture

### Data Types

```haskell
-- From LinkMetadataTypes.hs:
type ArchiveMetadataItem = Either
  Integer           -- Left: "first seen" date (ModifiedJulianDay, e.g. 58810 = 2019-11-22)
  (Maybe FilePath)  -- Right: archive path (Just "doc/www/...") or Nothing (failed permanently)

type ArchiveMetadata = Map Path ArchiveMetadataItem
```

The `Either` encoding captures the URL lifecycle:
- `Left date` → URL seen but not yet archived (waiting for delay period)
- `Right (Just path)` → Successfully archived at `path`
- `Right Nothing` → Archiving failed permanently (manual intervention needed)

### Compile-time Flow (localizeLink)

```
Link encountered during Pandoc AST walk
       │
       ├─ Has "archive-not" class? ──────────────────→ Skip (return original)
       │
       └─ rewriteLink
            │
            ├─ In whitelist? ────────────────────────→ Skip (return original)
            │
            ├─ In archive DB as Right (Just path)? ──→ Return archived link
            │                                          (add data-url-* attrs)
            ├─ In archive DB as Right Nothing? ──────→ Skip (permanent failure)
            │
            ├─ In archive DB as Left date? ──────────→ Skip (still pending)
            │
            └─ Not in DB? ───────────────────────────→ Insert as Left today
                                                       (side effect: start queue)
```

### Batch Archiving Flow (manualArchive)

```
manualArchive n
       │
       ├─ readArchiveMetadataAndCheck
       │     └─ validate files exist, checksums match
       │
       ├─ Filter to Left _ entries (pending)
       │
       ├─ Sort by firstSeen date (oldest first)
       │
       ├─ Split into:
       │     ├─ "cheap" items (PDFs, HN, GitHub) → archive all
       │     └─ regular items → take first n
       │
       ├─ mapConcurrently archiveItem
       │     └─ calls linkArchive.sh for each URL
       │
       └─ writeArchiveMetadata (merge new Right results)
```

### URL → Filepath Mapping

```haskell
-- SHA1 hash of URL (with newline) determines filename
archivePath url = "doc/www/" ++ domain(url) ++ "/" ++ sha1(url ++ "\n") ++ ".html"

-- Example:
-- "https://www.example.com/page?q=1#anchor"
--   → strip anchor → "https://www.example.com/page?q=1"
--   → sha1("https://www.example.com/page?q=1\n")
--   → "doc/www/www.example.com/a1b2c3d4....html"
```

Anchors (`#fragment`) are stripped for hashing but preserved in the final link.

### Link Rewriting Output

`localizeLink` adds several data attributes:

| Attribute | Purpose |
|-----------|---------|
| `data-url-archive` | Path to local archive (`/doc/www/...`) |
| `data-url-original` | Original external URL |
| `data-href-mobile` | Mobile-friendly alternative (e.g., Ar5iv for arXiv) |
| `data-url-iframe` | Live-embeddable version (e.g., GreaterWrong for LessWrong) |

---

## Key Patterns

### URL Transformation Pipeline

Before archiving, URLs go through `transformURLsForArchiving`:

```haskell
transformURLsForArchiving url =
    arxivAbsTosPdf       -- arxiv.org/abs/1234.5678 → arxiv.org/pdf/1234.5678.pdf
  $ openReviewForumToPdf -- openreview.net/forum?id=X → openreview.net/pdf?id=X
  $ redditToOldReddit    -- www.reddit.com → old.reddit.com
  $ twitterToLocalNitter -- x.com → localhost:8081 (local Nitter instance)
  $ mediumToFreedium     -- medium.com → freedium.cfd (paywall bypass)
  $ fandomToAntifandom   -- *.fandom.com → antifandom.com (clean frontend)
  $ waybackToIfFrame     -- web.archive.org/web/123/ → web.archive.org/web/123if_/ (no toolbar)
  $ lesswrongToGW        -- lesswrong.com → greaterwrong.com
  $ url
```

This ensures we archive the most useful version of each URL.

### GreaterWrong Transform (Config.LinkArchive.transformURItoGW)

LessWrong, Alignment Forum, EA Forum, and Arbital URLs are rewritten to GreaterWrong mirrors for better popup embedding:

```haskell
-- Domain mapping:
--   lesswrong.com        → www.greaterwrong.com
--   alignmentforum.org   → www.greaterwrong.com
--   effectivealtruism.org → ea.greaterwrong.com
--   arbital.com          → arbital.greaterwrong.com

-- Comment ID handling:
--   ?commentId=abc123    → /comment/abc123
--   #abc123 (17 chars)   → /comment/abc123

-- Query params added:
--   ?format=preview&theme=classic
```

### Checksum Validation

When URLs are rewritten (e.g., fixing link rot), the archive DB can become stale. `checksumIsValid` detects this:

```haskell
checksumIsValid url (Right (Just file)) =
  let derivedChecksum = unpack $ encode $ hash $ pack $
        (transformURLsForArchiving (takeWhile (/='#') url) ++ "\n")
      storedChecksum = takeWhile (/= '.') $ takeFileName file
  in derivedChecksum == storedChecksum
```

The checksum uses SHA1 of the *transformed* URL (post-rewrite) with trailing newline, hex-encoded. Mismatched entries are reset to `Left 0` (re-queue immediately).

### Delayed Archiving Logic

```haskell
archiveDelay :: Integer
archiveDelay = 60  -- days

archiveItemDue today (Left firstSeen) = (today - firstSeen) < archiveDelay
archiveItemDue _     (Right _)        = False  -- already been tried
```

**Important:** Despite the naming, `archiveItemDue` returns `True` for items that have been waiting *less* than 60 days. The filter keeps items in the waiting window (< 60 days old) and *excludes* items that have waited 60+ days. Within the filtered set, `manualArchive` sorts by firstSeen date (oldest first), so the items closest to the 60-day threshold get archived first.

This means items are archived *before* they reach the delay threshold, not after. Items that exceed 60 days are excluded from archiving (they may be considered stale or require manual review).

---

## Configuration

### Config.LinkArchive.hs

#### `archiveDelay :: Integer`
Days to wait before archiving (default: 60).

#### `whiteList :: String -> Bool`
Returns `True` if URL should never be archived. Checks:
1. Local paths (`/`, `./`, `../`, `https://gwern.net`)
2. PDFs (always archived regardless of domain)
3. ~800+ domain patterns in `whiteListMatchesFixed`

#### `isCheapArchive :: String -> Bool`
URLs that don't count against the manual review limit (PDFs, HN, GitHub, etc.).

#### `transformURLsForArchiving :: String -> String`
URL rewrites before archiving (arXiv→PDF, Reddit→OldReddit, etc.).

#### `transformURLsForMobile :: String -> String`
Mobile-friendly alternatives (arXiv→Ar5iv HTML, x.com→Nitter).

#### `transformURLsForLiveLinking :: String -> String`
Live-embeddable versions (LessWrong→GreaterWrong, Fandom→Antifandom).

### Whitelist Categories

The whitelist (`whiteListMatchesFixed`) skips domains for various reasons:

| Reason | Examples |
|--------|----------|
| Stable/reliable | wikipedia.org, plato.stanford.edu, archive.org |
| Interactive services | kaggle.com, colab.research.google.com |
| Video/audio (can't snapshot) | youtube.com, vimeo.com, soundcloud.com |
| Requires login | patreon.com, facebook.com |
| Too heavy/dynamic | ai.googleblog.com, quora.com |
| Dead/defunct | silkroad*.onion, intrade.com |
| Low-quality archives | onlinelibrary.wiley.com (broken JS) |

---

## Integration Points

### Database File

**Location:** `metadata/archive.hs`
**Format:** Haskell association list, pretty-printed

```haskell
[("https://example.com/page1", Right (Just "doc/www/example.com/abc123.html"))
,("https://example.com/page2", Left 60123)  -- first seen on day 60123
,("https://example.com/page3", Right Nothing)  -- permanently failed
]
```

### linkArchive.sh

Shell script that performs actual archiving:

```bash
linkArchive.sh URL                    # Archive + preview in browser
linkArchive.sh --no-preview URL       # Archive without preview
linkArchive.sh --check URL            # Return existing path or empty
linkArchive.sh --dry-run URL          # Return would-be path (no IO)
```

Uses:
- **SingleFile** (Docker): HTML pages → self-contained .html
- **wget + ocrmypdf**: PDFs → optimized local copy
- **curl**: MIME type detection, 404 checking

### Directory Structure

```
doc/www/
├── arxiv.org/
│   ├── a1b2c3d4.pdf
│   └── e5f6g7h8.pdf
├── news.ycombinator.com/
│   ├── 1234abcd.html
│   └── 5678efgh.html
│       └── 5678efgh/          # Resources for >5MB files
│           ├── image1.png
│           └── style.css
└── x.com/
    └── 9abc0def.html
```

Large HTML files (>5MB) are split by `deconstruct_singlefile.php` into HTML + resource directory.

### SEO/Robots Protection

Archives are hidden from search engines:
- Excluded from `sitemap.xml`
- Blocked in `robots.txt`
- `rel="canonical"` points to original (set by SingleFile)
- `rel="archive nofollow"` on archive links
- HTTP headers: `noarchive/noindex/nofollow/nocache` via nginx

### Frontend JS Integration

The current strategy (as of April 2025):
1. HTML source links to local archive (`href="/doc/www/..."`)
2. `data-url-original` attribute stores original URL
3. On page load, JS rewrites `href` back to original
4. Popups show `[original]` link using `data-url-original`

This gives:
- **Bots/no-JS users**: See archived version (won't 404)
- **JS users**: Copy original URLs, see live content

---

## Common Operations

### Add URL to Archive Queue

```haskell
-- In GHCi:
insertLinkIntoDB (Left todayDay) "https://example.com/article"
```

### Force Re-archive

```haskell
-- Reset to Left 0 to re-archive immediately:
insertLinkIntoDB (Left 0) "https://example.com/article"
```

### Check Archive Status

```bash
# Does archive exist?
linkArchive.sh --check "https://example.com/article"

# What would the path be?
linkArchive.sh --dry-run "https://example.com/article"
```

### Manual Batch Archive

```bash
cd ~/wiki/
ghci -istatic/build/ ./static/build/LinkArchive.hs -e 'manualArchive 10'
```

---

## Failure Modes

### "Archive file not found" error
The DB references a path that doesn't exist on disk. Run `readArchiveMetadataAndCheck` to find and remove stale entries.

### Checksum mismatch
URL was rewritten but archive.hs wasn't updated. The entry will be reset to `Left 0` and re-archived.

### Empty archive file
SingleFile sometimes produces empty/tiny files. Files \<1KB are treated as failures.

### 404/403 during archiving
`linkArchive.sh` returns exit code 2. Entry becomes `Right Nothing` (permanent failure).

### SingleFile timeout
Complex pages may exceed the 200s timeout. Manually archive with browser extension.

---

## See Also

- [Config.LinkArchive](/backend/config-link-archive-hs) - URL whitelist, transformation rules, and archive configuration
- [linkArchive.sh](/shell/link-archive) - Shell script that performs actual SingleFile/PDF archiving
- [deconstruct_singlefile.php](/php/deconstruct-singlefile) - Splits large SingleFile archives into HTML + assets
- [hakyll.hs](/backend/hakyll-hs) - Orchestrates archive loading and link processing during builds
- [sync.sh](/backend/sync-sh) - Build system that triggers archive operations
- [LinkMetadata.hs](/backend/link-metadata-hs) - Calls localizeLink during Pandoc transforms
- [popups.js](/frontend/popups-js) - Frontend uses data-url-original for [original] links
