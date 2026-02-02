
# LinkLive.hs

**Path:** `build/LinkLive.hs` | **Language:** Haskell | **Lines:** ~156

Determines which external URLs can be shown as live iframe previews based on domain whitelists.

---

## Overview

LinkLive.hs decides at compile-time which external links can be previewed as live iframes (rather than requiring annotations or forcing full page loads). When a URL passes the whitelist checks, the module adds the CSS class `link-live` to the link. The frontend JavaScript (`content.js`) reads this class to enable iframe-based popups.

The challenge is that most external websites *cannot* be iframed: they set `X-Frame-Options` headers, use JavaScript that breaks in frames, have mixed HTTP/HTTPS content, or have unusable layouts (giant sticky headers, etc.). Gwern estimates only about 25% of external links work as live popups. Rather than disappointing readers with broken previews, the site whitelists domains that have been manually verified to work.

The configuration lives in `Config/LinkLive.hs` - a 4,600-line file containing domain lists organized by:
- **goodDomainsSimple**: exact domain matches (~700 domains)
- **goodDomainsSub**: suffix patterns like `.github.io` (~18 patterns)
- **badDomainsSimple**: known-broken exact domains (~2,300 domains)
- **badDomainsSub**: known-broken suffix patterns (~7 patterns)
- **goodLinks/badLinks**: specific URLs for test cases

---

## Public API

### `linkLive :: Inline -> Inline`

Main entry point. Adds `link-live` class to links that pass the whitelist.

```haskell
linkLive (Link (_, cl, kvs) _ (u, _))
  | "link-live-not" `elem` cl             = x           -- explicit opt-out
  | "data-url-archive" `elem` map fst kvs = addClass x  -- local archives always work
  | linkLiveP u                           = addClass x  -- whitelist check
  | otherwise                             = x
```

**Called by:** `Typography.hs` (in `typographyTransformPermanent`)
**Calls:** `linkLiveP`, `addClass`

---

### `linkLiveP :: Text -> Bool`

Predicate: can this URL be shown as a live iframe?

```haskell
linkLiveP u
  | u `elem` overrideLinkLive     = True   -- explicit override
  | u `elem` overrideLinkLiveNot  = False  -- explicit block
  | "http://" `isPrefixOf` u      = False  -- no HTTP (mixed content)
  | "/" `isPrefixOf` u            = False  -- local links don't match
  | otherwise                     = urlLive u == Just True
```

**Called by:** `linkLive`, external callers checking liveness

---

### `urlLive :: Text -> Maybe Bool`

Core lookup logic. Returns `Just True` (known good), `Just False` (known bad), or `Nothing` (unknown/untested).

```haskell
urlLive u
  | host u `elem` badDomainsSimple  = Just False
  | anySuffixT (host u) badDomainsSub  = Just False
  | host u `elem` goodDomainsSimple = Just True
  | anySuffixT (host u) goodDomainsSub = Just True
  | anyInfixT u wikipediaURLs       = Just $ wikipediaLiveP u
  | otherwise                       = miscUrlRules u
```

**Precedence:** bad simple > bad sub > good simple > good sub > Wikipedia > misc rules

---

### `wikipediaLiveP :: Text -> Bool`

Special handling for Wikipedia. Most Wikipedia pages work as live popups except `Special:*` namespace (sets X-Frame headers for security).

```haskell
wikipediaLiveP u = "link-live" `elem` wpPopupClasses u
```

Delegates to `Interwiki.wpPopupClasses` which parses the URL path and checks against `linkliveNamespacesNo = ["Special", "/w/index.php"]`.

---

### `linkLivePrioritize :: IO [(Int, Text)]`

Build-time helper: finds untested domains with high link counts. Appends test cases to `/lorem-link.md` for manual review.

```haskell
linkLivePrioritize = do
  backlinks <- readBacklinksDB
  -- Group by domain, filter untested, sort by count
  -- Write high-frequency domains to test page
```

**Called by:** `sync.sh` during build
**Writes to:** `lorem-link.md`

---

### `linkLiveTest :: [(Text, Bool)]`

Unit test suite. Returns URLs that fail their expected classification (good URL not getting `link-live`, bad URL incorrectly getting it).

**Called by:** `Test.hs`

---

### `linkLiveTestHeaders :: IO ()`

Runtime header check. Uses curl to fetch headers for all `goodLinks` and warns if `X-Frame-Options` or `frame-ancestors` is detected.

```haskell
linkLiveTestHeaders = forM_ goodLinks $ \u -> do
  (_, _, headers) <- runShellCommand "curl" ["--head", u]
  when ("x-frame" `isInfixOf` headers) $
    printRed u >> print "X-FRAME option detected"
```

---

## Internal Architecture

### Decision Flow

```
URL input
    │
    ├─ overrideLinkLive?     → True
    ├─ overrideLinkLiveNot?  → False
    ├─ http:// prefix?       → False (mixed content blocked by browsers)
    ├─ local path?           → False
    │
    └─ urlLive lookup:
        ├─ badDomainsSimple?   → False
        ├─ badDomainsSub?      → False
        ├─ goodDomainsSimple?  → True
        ├─ goodDomainsSub?     → True
        ├─ Wikipedia?          → wikipediaLiveP check
        └─ miscUrlRules        → special cases (YouTube embeds, Markdeep)
```

### Config/LinkLive.hs Structure

```haskell
-- Override lists (highest priority)
overrideLinkLive, overrideLinkLiveNot :: [Text]

-- Domain patterns
goodDomainsSub    :: [Text]  -- e.g., [".github.io", ".wordpress.com"]
goodDomainsSimple :: [Text]  -- e.g., ["gwern.net", "lesswrong.com"]
badDomainsSub     :: [Text]  -- e.g., [".substack.com", ".medium.com"]
badDomainsSimple  :: [Text]  -- e.g., ["arxiv.org", "twitter.com"]

-- Test URLs with known status
goodLinks, badLinks :: [Text]

-- Special-case rules
miscUrlRules :: Text -> Maybe Bool
miscUrlRules u
  | "youtube.com/embed/" `isPrefixOf` u = Just True  -- embeds work
  | "youtube.com/" `isPrefixOf` u       = Just False -- regular YT blocked
  | "casual-effects.com" `isPrefixOf` u = Just $ not $ ".md.html" `isInfixOf` u
  | otherwise = Nothing
```

---

## Key Patterns

### Why Whitelist Instead of Blacklist?

Only ~25% of external sites work in iframes. A blacklist would require maintaining thousands of entries and would still miss new broken sites. The whitelist approach means:
1. New domains default to "unknown" (no live popup offered)
2. Manual testing adds verified domains
3. `linkLivePrioritize` surfaces high-frequency untested domains

### HTTP vs HTTPS

HTTP links are unconditionally rejected:

```haskell
| "http://" `T.isPrefixOf` u = False
```

Browsers block "mixed content" - an HTTPS page (gwern.net) cannot load HTTP iframes. Even if the HTTP version of a site works, it's blocked by browser security.

### Local Archive Fallback

Links with `data-url-archive` always get `link-live`:

```haskell
| "data-url-archive" `elem` map fst kvs = addClass x
```

If gwern.net has a local mirror of the page (via `LinkArchive.hs`), the popup can use that instead of the live external site.

---

## Configuration

| Setting | Location | Effect |
|---------|----------|--------|
| `testPage` | Config/LinkLive.hs | Where `linkLivePrioritize` appends test cases |
| `linkLivePrioritizeMinimum` | Config/LinkLive.hs | Minimum backlink count to surface for testing (default: 3) |
| `linkLivePrioritizeBlacklist` | Config/LinkLive.hs | Domains to never suggest for testing |

---

## Integration Points

### Build-Time Integration

1. **Typography.hs** calls `linkLive` during permanent transforms
2. **Test.hs** runs `linkLiveTest` to catch regressions
3. **sync.sh** may call `linkLivePrioritize` to surface new domains

### Runtime Integration (Frontend)

`content.js` checks for `link-live` class:

```javascript
// content.js:465
&& link.classList?.contains("link-live") == true
```

When detected, the popup system creates an iframe instead of fetching/transforming content.

### Related Classes

- `link-live`: URL can be iframed
- `link-live-not`: Explicit opt-out (author override)
- `content-transform`: Use API-based content (Wikipedia, GitHub)
- `content-transform-not`: Skip content transforms

---

## See Also

- [Config.LinkLive](/backend/config-link-live-hs) - Domain whitelist/blacklist configuration consumed by this module
- [popups.js](/frontend/popups-js) - Frontend popup system that uses `link-live` class
- [extracts.js](/frontend/extracts-js) - Frontend coordinator that handles live link popups
- [content.js](/frontend/content-js) - Frontend content system that checks `link-live` class
- [hakyll.hs](/backend/hakyll-hs) - Main build system that integrates link processing
- [Typography.hs](/backend/typography-hs) - Calls `linkLive` in the transform pipeline
- [Interwiki.hs](/backend/interwiki-hs) - Wikipedia URL handling and `wpPopupClasses`
- [LinkArchive.hs](/backend/link-archive-hs) - Local archive system providing fallback for live links
