
# Config.LinkArchive

**Path:** `build/Config/LinkArchive.hs` | **Language:** Haskell | **Lines:** ~1,175

> Configuration module defining URL whitelists/blacklists and transformation rules for the link archiving system

---

## Overview

Config.LinkArchive is a large configuration module that controls which external URLs get archived locally and how they are transformed before archiving or display. It serves as the policy layer for gwern.net's aggressive link preservation strategy, deciding which domains are "safe" to link directly vs. which need local mirrors.

The module contains three main categories of logic: (1) URL transformation functions that rewrite URLs to better formats for archiving, mobile viewing, or live-linking (e.g., converting Arxiv abstract pages to PDFs, Reddit to Old Reddit, LessWrong to GreaterWrong); (2) a massive whitelist of ~850+ domain patterns that should NOT be archived because they are stable, interactive services, or otherwise unsuitable; and (3) test cases for validating the URL rewriting logic.

The design philosophy is "archive everything by default, whitelist exceptions." This ensures maximum link preservation but requires ongoing maintenance of the whitelist as new domains are encountered. The module also handles special cases like adding Amazon affiliate tags, transforming Wikipedia to mobile versions, and converting forum URLs to cleaner reader-mode formats.

---

## Public API

### `archiveDelay :: Integer`

Delay between archive operations (60 seconds).

**Used by:** Archiving scheduler

---

### `isCheapArchive :: String -> Bool`

Determines if a URL is "cheap" to archive (PDF, HN, GitHub, etc.) and shouldn't count against manual review limits.

**Called by:** Archive batch processing
**Calls:** `transformURLsForArchiving`, `anyInfix`

---

### `transformURLsForArchiving :: String -> String`

Rewrites URLs to optimal formats for local archiving. Key transformations:
- Arxiv `/abs/` → `/pdf/` (get the actual paper)
- OpenReview forum → pdf
- Reddit → Old Reddit
- X.com → localhost proxy
- Medium → Freedium (clean frontend)
- Fandom wikis → Antifandom
- Wayback Machine → iframe-friendly format

**Called by:** [link-archive-hs](link-archive-hs) archiving pipeline
**Calls:** `sed`, `replace`, `transformURItoGW`

---

### `transformURLsForMobile :: String -> String`

Rewrites URLs for mobile-friendly versions (`data-href-mobile` attribute):
- Arxiv → HTML version via `?fallback=original`
- X.com → Nitter

**Called by:** Link metadata processing
**Calls:** `sed`, `replace`

---

### `transformURLsForLiveLinking :: String -> String`

Rewrites URLs for iframe/popup display (`data-url-iframe`). Includes validation that transformed URLs pass live-link checks.

**Called by:** Popup/live-link system
**Calls:** `transformURItoGW`, `transformWPtoMobileWP`, `linkLiveP`, `hasHTMLSubstitute`

---

### `transformURItoGW :: String -> String`

Converts LessWrong, Alignment Forum, EA Forum, and Arbital URLs to GreaterWrong reader-mode equivalents with `?format=preview&theme=classic`. Handles comment ID transformations.

**Called by:** `transformURLsForArchiving`, `transformURLsForLiveLinking`
**Calls:** `parseURI`, URI manipulation functions

---

### `transformWPtoMobileWP :: String -> String`

Converts Wikipedia URLs to mobile versions with `#bodyContent` anchor for cleaner popups.

**Called by:** `transformURLsForLiveLinking`
**Calls:** `wikipediaLiveP`, `parseURI`

---

### `whiteList :: String -> Bool`

The main filtering function. Returns `True` if a URL should NOT be archived. Logic priority:
1. Always archive CiteSeerX (returns `False`)
2. Skip local paths, gwern.net paths, mailto, etc. (returns `True`)
3. Always archive PDFs (returns `False`)
4. Check against ~850+ domain patterns

**Called by:** [link-archive-hs](link-archive-hs) archive decision logic
**Calls:** `anyInfix`, `anyPrefix`, `anySuffix`

---

### `addAmazonAffiliate :: String -> String`

Appends `tag=gwernnet-20` affiliate parameter to Amazon URLs.

**Called by:** URL post-processing (currently commented out in main flow)

---

### `addGithubReadme :: String -> String`

Appends `#readme` to GitHub repository root URLs.

**Called by:** URL post-processing (currently commented out in main flow)

---

### `localizeLinktestCases :: [(T.Text, (T.Text, T.Text, T.Text, [T.Text]))]`

Test vectors for URL localization: (original URL, (archive path, mobile URL, HTML URL, classes)).

**Called by:** Test suite in [link-archive-hs](link-archive-hs)

---

### `localizeLinkTestDB :: ArchiveMetadata`

Mock database for testing, mapping URLs to their expected archive paths.

**Called by:** Test suite

---

## Internal Architecture

### URL Transformation Pipeline

```
Original URL
    │
    ├─► transformURLsForArchiving ─► local archive path
    │       (Arxiv→PDF, Reddit→Old, Medium→Freedium, etc.)
    │
    ├─► transformURLsForMobile ─► data-href-mobile
    │       (Arxiv→HTML, X→Nitter)
    │
    └─► transformURLsForLiveLinking ─► data-url-iframe
            (LW→GW, WP→mobile WP, IA→theater mode)
```

### GreaterWrong Rewriting

The `transformURItoGW` function handles four forum families:
- `lesswrong.com` → `www.greaterwrong.com`
- `alignmentforum.org` → `www.greaterwrong.com`
- `effectivealtruism.org` → `ea.greaterwrong.com`
- `arbital.com` → `arbital.greaterwrong.com`

Comment URLs are normalized from `?commentId=X` or `#X` to `/comment/X` path format.

### Whitelist Structure

The `whiteListMatchesFixed` list contains ~850 entries organized by category (in comments):
- File extensions (`.txt`, `.csv`, `.mp3`, etc.)
- Stable reference sites (Wikipedia, SEP, HathiTrust)
- Interactive services (Google services, Kaggle, Beeminder)
- Video platforms (YouTube, Vimeo, TikTok)
- Dead/defunct sites (Silk Road onions, Intrade)
- Sites with poor archive quality (video embeds, audio embeds)
- Homepages not worth archiving

---

## Key Patterns

### Sed-based URL Rewriting

Uses a custom `sed` function for regex transformations:
```haskell
sed "https://arxiv.org/abs/([0-9]+\\.[0-9]+)(#.*)?"
    "https://arxiv.org/pdf/\\1.pdf\\2"
```

### Defensive Live-Link Validation

`transformURLsForLiveLinking` validates transformed URLs won't break:
```haskell
if u == urlFinal || head urlFinal == '/' || linkLiveP (T.pack urlFinal)
then urlFinal
else error "..."
```

### Affiliate Tag Injection

Carefully handles URLs with/without existing query parameters:
```haskell
if "?" `isInfixOf` l then l ++ "&tag=gwernnet-20"
                     else l ++ "?tag=gwernnet-20"
```

---

## Configuration

All configuration is embedded as Haskell values:

| Constant | Value | Purpose |
|----------|-------|---------|
| `archiveDelay` | 60 | Seconds between archive requests |
| `whiteListMatchesFixed` | ~850 entries | Domains to skip archiving |
| `gwBlacklist` | 7 entries | LW pages that don't exist on GW |
| `originalDomains` | 4 entries | Forums to rewrite to GreaterWrong |

---

## Integration Points

### Imports From

- **LinkMetadataTypes**: `ArchiveMetadata`, `hasHTMLSubstitute`
- **LinkLive**: `linkLiveP`, `wikipediaLiveP` - URL validation
- **Utils**: `sed`, `anyInfix`, `anyPrefix`, `anySuffix`, `replace`

### Used By

- **[link-archive-hs](link-archive-hs)**: Main consumer - uses whitelist and transforms
- **Link processing pipeline**: Injects `data-url-archive`, `data-href-mobile`, `data-url-iframe` attributes

### Data Flow

```
Markdown link
    │
    ▼
LinkArchive.whiteList() ──► skip if True
    │
    ▼ (archive needed)
transformURLsForArchiving() ──► fetch & store
    │
    ▼
transformURLsForMobile() ──► data-href-mobile attr
    │
    ▼
transformURLsForLiveLinking() ──► data-url-iframe attr
```

---

## See Also

- [LinkArchive.hs](/backend/link-archive-hs) - Main archiving logic that consumes this config
- [linkArchive.sh](/shell/link-archive) - Shell script that performs the actual archiving
- [LinkLive.hs](/backend/link-live-hs) - URL liveness validation used by transforms
- [LinkMetadata.hs](/backend/link-metadata-hs) - Applies transformed URLs to link metadata
- [hakyll.hs](/backend/hakyll-hs) - Build system that loads archive configuration
- [deconstruct_singlefile.php](/php/deconstruct-singlefile) - Post-processes large archive files
