---
id: 404-guesser-js
slug: 404-guesser-js
---

# 404-guesser.js

**Path:** `js/404-guesser.js` | **Language:** JavaScript | **Lines:** ~263

> Enhances 404 error pages with intelligent URL suggestions based on Levenshtein distance

---

## Overview

The 404 guesser provides a user-friendly enhancement to gwern.net's 404 error pages by automatically suggesting similar URLs that the user might have intended to visit. When a user lands on a non-existent page (such as `/rubiiks-cube` instead of `/rubiks-cube`), the script fetches the site's sitemap, compares the invalid URL against all valid URLs using a bounded Levenshtein distance algorithm, and presents the top 10 most similar matches.

The implementation is optimized for performance despite gwern.net's large sitemap (37,000+ URLs, 4.7MB uncompressed). It uses a bounded edit distance calculation that typically completes in ~3 seconds, and the sitemap compresses well (to ~0.5MB) making the bandwidth cost comparable to loading a couple of images. The script intelligently handles both full-path matching and basename-only matching as a fallback, catching cases where directory paths are wrong or missing.

Legitimate 404 errors are rare on gwern.net (\<10 per day) due to comprehensive redirect rules and maintained internal links, so this feature primarily benefits real users who make typos rather than bots. The script includes logic to skip suggestion generation when the current URL is `/404` (a redirect case where the original URL is lost).

---

## Public API

### Main Entry Point

```javascript
async function suggest404Alternatives()
```
**Purpose:** Main orchestration function
**Behavior:** Fetches sitemap, finds similar URLs, injects suggestions into page
**Called by:** `window.load` event listener
**Calls:** `fetchSitemap()`, `urlPathnamesFromSitemapString()`, `findSimilarUrlPaths()`, `injectSuggestions()`
**Early exit:** If current path ends with `/404` (redirected case, not a real 404)

---

## Internal Architecture

### Data Flow

1. **Sitemap Retrieval:** `fetchSitemap()` fetches `/sitemap.xml` via `fetch()` API
2. **XML Parsing:** `urlPathnamesFromSitemapString()` uses `DOMParser` to extract `loc` elements and convert to pathnames
3. **Similarity Matching:** `findSimilarUrlPaths()` compares current path against all sitemap URLs
4. **DOM Injection:** `injectSuggestions()` creates a section element and inserts before `#other-options`

### Core Functions

**fetchSitemap()**
```javascript
async function fetchSitemap()
```
- Fetches `location.origin + "/sitemap.xml"`
- Returns sitemap text or `null` on error
- Uses standard `fetch()` API with error handling

**urlPathnamesFromSitemapString(sitemapText)**
```javascript
function urlPathnamesFromSitemapString(sitemapText)
```
- Parses sitemap XML string using `DOMParser`
- Extracts all `loc` elements from the sitemap XML
- Returns array of pathname strings (just the path component, no domain)

**findSimilarUrlPaths(urlPaths, targetPath, numResults, maxDistance)**
```javascript
function findSimilarUrlPaths(urlPaths, targetPath, numResults, maxDistance)
```
- Full-path matching with bounded Levenshtein distance
- Pre-filters by length difference (`Math.abs(length_a - length_b) <= maxDistance`)
- Calculates edit distance for remaining candidates
- Sorts by distance ascending, takes top `numResults`
- Returns array of up to `numResults` pathname strings

**findSimilarByBasename(urlPaths, targetPath, numResults, maxDistance)**
```javascript
function findSimilarByBasename(urlPaths, targetPath, numResults, maxDistance)
```
- Fallback matcher for cases like `/ai-slop` → `/blog/ai-slop`
- Extracts last path segment (basename) using `URLFromString().pathSegments.last`
- Skips if basename has fewer than 3 characters or is a directory-only path
- Filters out single-character basenames
- Matches basenames only, returns full pathnames

**injectSuggestions(currentPath, suggestedUrlStrings)**
```javascript
function injectSuggestions(currentPath, suggestedUrlStrings)
```
- Builds HTML for suggestion list
- Wraps in a columns div if more than 3 suggestions
- Inserts before `#other-options` element in `#markdownBody`
- Calls `Extracts.addTargetsWithin()` to enable popups on suggestion links

---

## Key Patterns

### Bounded Levenshtein Distance

The script implements a custom `boundedLevenshteinDistance()` function that aborts early if the distance exceeds a threshold:

```javascript
function boundedLevenshteinDistance(a, b, maxDistance)
```

**Optimizations:**
1. **Length pre-check:** Returns `maxDistance + 1` if absolute length difference > `maxDistance`
2. **Row-wise early exit:** After computing each row of the dynamic programming matrix, checks if minimum distance in that row exceeds `maxDistance` and aborts if so
3. **Result:** Avoids computing full O(n*m) matrix for dissimilar strings

This is critical for performance: with 37,000+ URLs, full Levenshtein distance would be prohibitively slow. The bounded version with `maxDistance=8` completes in ~3 seconds.

### Two-Stage Matching Strategy

The algorithm combines both matching approaches:

1. **Basename matching first:** Matches only the last path segment with stricter threshold (`maxDistance * 0.5` = 4)
2. **Full-path matching second:** Matches entire pathnames with full threshold (`maxDistance` = 8)

Results are concatenated, deduplicated via `.unique()`, and limited to `maxNumResults` (10). This catches both wrong/missing directory structures (e.g., `/ai-slop` to `/blog/ai-slop`) and typos in the exact path.

### Sitemap Caching Considerations

The sitemap is fetched client-side and may be cached by the browser. The author notes this is acceptable because:
- Legitimate 404s are rare
- Most 404s are to old URLs, not newly created ones
- Probability of: (1) hitting 404, (2) caching sitemap, then (3) hitting 404 for brand-new URL is negligible

The server-side sitemap cache is flushed on every sync, ensuring server freshness.

### Performance Justification

Despite the 4.7MB sitemap size, the feature is deemed worthwhile:
- Compresses to ~0.5MB (comparable to a few images)
- Legitimate users hit 404s rarely (fewer than 10 per day)
- Ill-behaved bots typically don't execute JavaScript
- If bots do load it, they waste their own bandwidth (self-DoS)
- For legitimate users, 0.5MB + 3 seconds is faster than manually searching Google

---

## Integration Points

### Dependencies

**utility.js:**
- `elementFromHTML()` - Creates DOM element from HTML string
- `URLFromString().pathSegments.last` - Extracts basename from URL string

**Extracts.js:**
- `Extracts.addTargetsWithin(element)` - Enables popup/annotation functionality on injected links

### DOM Targets

**Injection point:**
- Inserts before `#other-options` element within `#markdownBody`

**Generated HTML structure:**

The script injects a section with ID `guessed-urls` containing a header, description paragraph, and a list of suggestion links. If more than 3 suggestions, the list is wrapped in a columns div for multi-column display.

### Event Hooks

**window.load:**
- Registers `suggest404Alternatives()` to run after page load
- Ensures DOM is ready before injection

---

## Configuration

### Constants

```javascript
const sitemapURL = location.origin + "/sitemap.xml";
```

### Tunable Parameters

**In suggest404Alternatives():**
- `numResults: 10` - Maximum number of suggestions to display
- `maxDistance: 8` - Maximum edit distance for full-path matching (chosen heuristically)

**In findSimilarUrlPaths():**
- Basename fallback uses `Math.ceil(maxDistance * 0.5)` = 4 for stricter matching

**In injectSuggestions():**
- `wrapInColumnsIfMoreEntriesThan: 3` - Column layout threshold

---

## Security Notes

The implementation **trusts the sitemap.xml** and does not sanitize XML or URLs. The author notes:

> Security Note: the sitemap.xml is trusted and assumed to contain clean valid XML with well-behaved alphanumerical/punctuation URLs and maybe a bit of Unicode. We do not attempt to sanitize or validate the XML or the parsed URLs, so if used in settings where attackers may control the sitemap.xml (eg. user-generated content), the XML part should probably be rewritten to be ultra-paranoid & do things like ping the final suggested URLs to make sure they exist.

This is acceptable for gwern.net's controlled environment but would need hardening for user-generated content scenarios.

---

## See Also

- [nginxredirectguesser.hs](/backend/nginx-redirect-guesser-hs) - Server-side counterpart using same Levenshtein algorithm
- [extracts.js](/frontend/extracts-js) - Popup system integration for suggestion links
- [initial.js](/frontend/initial-js) - Utility functions (`elementFromHTML`, `URLFromString`)
- [utility.js](/frontend/utility-js) - Core DOM utilities used for suggestion injection
- [rewrite.js](/frontend/rewrite-js) - DOM transformation framework
- [gwern.net.conf](/nginx/gwern-net-conf) - Nginx redirect rules that reduce 404 occurrences

---

**Author:** Gwern Branwen
**License:** CC-0
**Last Updated:** 2025-03-03

