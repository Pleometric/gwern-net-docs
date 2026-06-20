---
title: "Config.LinkLive"
description: "Domain whitelist/blacklist configuration for iframe-able \"live\" link popups"
---

# Config.LinkLive

Domain whitelist/blacklist configuration for iframe-able "live" link popups

<div className="doc-meta">
  <div><strong>Path</strong><code>build/Config/LinkLive.hs</code></div>
  <div><strong>Language</strong>Haskell</div>
  <div><strong>Lines</strong>4618</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/Config/LinkLive.hs">build/Config/LinkLive.hs</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the Haskell build pipeline, generators, metadata code, or backend utility behavior around Config.LinkLive.
</div>

## Overview

Config.LinkLive is a pure configuration module containing curated lists of domains and URLs that determine whether external links can be displayed as "live" popups (rendered in iframes) versus requiring annotation fallbacks. This addresses a fundamental web compatibility problem: only ~25% of external websites work correctly inside iframes due to `X-Frame-Options` headers, JavaScript requirements, mixed-content blocking, and reader-unfriendly design patterns.

The module maintains six main lists: domains known to work (whitelisted), domains known to fail (blacklisted), and specific URL overrides for edge cases. Each list is split into "simple" (exact domain match) and "sub" (suffix match for subdomains) variants. The configuration is consumed by [LinkLive](../backend/link-live-hs), which applies `link-live` CSS classes at compile time, enabling the frontend JavaScript to offer live previews only for links that will actually work.

The lists are maintained through systematic manual testing using the `/lorem-link` test page. New domains are automatically prioritized for testing based on frequency of occurrence in the backlinks database, with a minimum threshold (default: 3 uses) before a domain is added to the review queue.

---

## Public API

### `testPage :: FilePath`

Local path to the Markdown file where new test cases are appended for manual review.

**Value:** `"lorem-link.md"`

---

### `linkLivePrioritizeBlacklist :: [T.Text]`

Domains to exclude from the auto-prioritization system (e.g., domains known to have many links but which are permanently problematic).

**Value:** `["omega.albany.edu"]`

---

### `linkLivePrioritizeMinimum :: Int`

Minimum number of occurrences before an untested domain is automatically added to the review queue.

**Value:** `3`

---

### `overrideLinkLive, overrideLinkLiveNot :: [T.Text]`

Exact URL overrides that bypass domain-level rules. `overrideLinkLive` forces specific URLs to be live-enabled; `overrideLinkLiveNot` forces specific URLs to be blocked from live popups.

**Usage:** Rare edge cases where a single URL behaves differently from its domain's general pattern.

---

### `wikipediaURLs :: [T.Text]`

Domain patterns for Wikipedia sites (matched via infix).

**Value:** `[".wikipedia.org"]`

**Note:** Wikipedia has special handling—most pages work, but the `Special:` namespace is blocked by headers.

---

### `miscUrlRules :: T.Text -> Maybe Bool`

Special-case URL pattern matching that cannot be expressed via simple domain lists.

**Returns:**
- `Just True` — URL is live-compatible
- `Just False` — URL is not live-compatible
- `Nothing` — No special rule applies

**Current rules:**
- YouTube embeds (`/embed/` paths) work; regular YouTube URLs don't
- Markdeep demos (`.md.html` URLs) don't work; the main site does

---

### Domain Lists

```haskell
goodDomainsSub :: [T.Text]      -- ~17 entries, suffix-matched (e.g., ".github.io")
goodDomainsSimple :: [T.Text]   -- ~730 entries, exact domain match
badDomainsSub :: [T.Text]       -- ~7 entries, suffix-matched (e.g., ".substack.com")
badDomainsSimple :: [T.Text]    -- ~1,600 entries, exact domain match
```

**Called by:** [LinkLive.urlLive](../backend/link-live-hs)
**Tested by:** [Test.hs](../backend/test-hs) (uniqueness, validity)

---

### URL Lists

```haskell
goodLinks :: [T.Text]  -- ~250 entries, full URL test cases
badLinks :: [T.Text]   -- Large list, full URL test cases
```

These are specific URLs used for unit testing the domain classification logic. Each URL is verified against its expected classification during test runs.

---

## Internal Architecture

### List Organization

The configuration uses a layered matching strategy with clear precedence:

1. **Exact URL overrides** (`overrideLinkLive` / `overrideLinkLiveNot`)
2. **Bad domain lists** (simple then sub) — block first
3. **Good domain lists** (simple then sub) — allow if not blocked
4. **Wikipedia special handling** — namespace-aware checks
5. **Miscellaneous URL rules** — pattern-based edge cases
6. **Default** — `Nothing` (untested/unknown)

### Subdomain Matching

"Sub" lists use suffix matching to handle entire domain families:
- `.github.io` matches `foo.github.io`, `bar.github.io`, etc.
- `.substack.com` matches all Substack blogs
- `.fandom.com` matches all Fandom wikis

### Test URL Selection

The `goodLinks` and `badLinks` lists serve dual purposes:
1. Unit test inputs for regression testing
2. Documentation of specific page behavior within a domain

---

<details className="generated-section">
<summary>Key Patterns</summary>

### Conservative Blocking

The module follows a "block by default for untested" philosophy. Unknown domains return `Nothing`, which the consumer ([LinkLive](../backend/link-live-hs)) treats as "don't offer live popup." This prevents broken user experiences.

### HTTP Exclusion

All HTTP (non-HTTPS) URLs are automatically blocked due to browser mixed-content security policies—HTTPS Gwern.net cannot load HTTP iframes.

### Frequency-Based Testing Priority

Rather than testing domains randomly, the build system counts domain occurrences across all backlinks and surfaces untested high-frequency domains for review. This ensures testing effort is spent on domains that provide the most value.

### Platform-Level Blocks

Some platforms block iframes site-wide via configuration:
- Substack (`.substack.com`)
- Stack Exchange (`.stackexchange.com`)
- Medium (`.medium.com`)
- Most academic publishers (`.oxfordjournals.org`, etc.)

---
</details>

<details className="generated-section">
<summary>Configuration</summary>

All configuration is compile-time constants in this file. To add a new domain:

1. Test the domain manually using `/lorem-link`
2. Add to appropriate list:
   - Single domain → `goodDomainsSimple` or `badDomainsSimple`
   - Domain family → `goodDomainsSub` or `badDomainsSub`
3. Add a representative URL to `goodLinks` or `badLinks` for regression testing

### Testing a Domain

1. Navigate to `/lorem-link#link-testcases`
2. Find or add a test link for the domain
3. Attempt to trigger a live popup
4. Verify the page renders correctly in the iframe (no headers blocking, no JS errors, readable content)

---
</details>

## Integration Points

### Consumed By

- **[LinkLive.hs](../backend/link-live-hs):** Main consumer; uses all domain/URL lists via `urlLive` function
- **[Test.hs](../backend/test-hs):** Validates list uniqueness and domain format correctness

### Runtime Effects

The classification flows through:
1. Build time: `LinkLive.linkLive` adds `link-live` class to qualifying links
2. Runtime: `extracts-contents.js` reads class to enable live popup behavior

### Shared State

None—this is a pure configuration module with no mutable state or IO.

---

<details className="generated-section">
<summary>See Also</summary>

- [LinkLive.hs](/backend/link-live-hs) - The logic module that consumes this configuration
- [extracts.js](/frontend/extracts-js) - Frontend coordinator for popup behavior
- [extracts-content.js](/frontend/extracts-content-js) - Frontend content types including live iframes
- [popups.js](/frontend/popups-js) - Desktop popup windowing system
- [Interwiki.hs](/backend/interwiki-hs) - Wikipedia-specific popup class handling
- [Typography.hs](/backend/typography-hs) - Transform pipeline that applies link-live classes
</details>
