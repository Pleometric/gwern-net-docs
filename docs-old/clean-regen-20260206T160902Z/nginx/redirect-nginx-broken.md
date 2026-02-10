# broken.conf

**Path:** `nginx/redirect/broken.conf` | **Language:** Nginx

Nginx redirect map configuration for handling broken, malicious, and malformed URLs on gwern.net.

---

## Overview

`nginx-broken.conf` is a comprehensive nginx map configuration containing **28,822 redirect rules** specifically designed to handle broken URLs, malicious crawler requests, typos, and malformed links. Unlike `nginx.conf` which handles legitimate content moves, this file serves as a defensive layer that cleans up garbage URLs and redirects broken requests to appropriate destinations.

This file represents years of accumulated fixes for broken external links, typosquatting, malicious bots, and URL corruption. It implements a "zero error log" philosophy where even garbage requests are handled gracefully rather than generating 404 errors.

## File Structure

**Location:** `nginx/redirect/broken.conf`

**Type:** Nginx map configuration

**Size:** 29,132 lines, ~1.9MB

**Redirect count:** 28,822 rules

**Format:** Same nginx map syntax as nginx.conf
```nginx
"~^/broken/url$" "/correct/url";
"~^/malicious/.*$" "/404";
```

## Major Categories

### 1. Literal Matches (Lines 1-100+)

**Purpose:** Fix common broken URLs and typos

```nginx
"~^//$" "/";  # Double slash → home
"~^/image/$" "/doc/index";  # Old image directory
"~^/doc/genetics/heritable/2018-prasad\.pdf.*$" "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5994200/";
```

**Patterns:**
- Malformed URLs (double slashes, missing paths)
- Redirects to external sources when local copy unavailable
- Common misspellings and variations

### 2. Newsletter Date Normalization (Throughout)

Extensive redirects for newsletter date variations:

```nginx
"~^/newsletter/2020/00$" "/newsletter/2020/01";  # Month 00 → 01
"~^/newsletter/2020/010.*$" "/newsletter/2021/10";  # Leading zero typos
"~^/newsletter/2018/1$" "/newsletter/2018/01";  # Single digit → zero-padded
```

**Commented-out redirects:**
```nginx
# "~^/2020/01$" "/newsletter/2020/01";
# "~^/2021/11$" "/newsletter/2021/11";
```

These commented redirects suggest year-only paths were considered but disabled (possibly to avoid conflicts).

### 3. Danbooru Dataset URL Variations (Lines 27-74)

**Background:** gwern.net hosts the Danbooru2020/2021 anime image dataset

Massive effort to catch all typo variations:

```nginx
"~^/Danbooru2020.*$" "/danbooru2021#danbooru2020";
"~^/danbooru2020$" "/danbooru2021#danbooru2020";
"~^/Dabooru2020$" "/danbooru2021#danbooru2020";  # Typo
"~^/Dabbooru2020.*" "/danbooru2021#danbooru2020";  # Double-b typo
"~^/Danboru2020$" "/danbooru#danbooru2020";  # Missing 'o'
"~^/Danbooru20120$" "/danbooru2021#danbooru2020";  # Year typo
"~^/danbooru204$" "/danbooru2021#danbooru2020";  # Truncated
"~^/Danboo$" "/danbooru201";  # Severely truncated
"~^/banboor.*$" "/danbooru201";  # Wrong first letter
```

**Temporally-limited redirects:**
```nginx
"~^/[Dd]anbooru2022.*$" "/danbooru2021"; # WARNING: TO REMOVE in JANUARY 2023
"~^/[Dd]anbooru2023.*$" "/danbooru2022"; # WARNING: TO REMOVE in JANUARY 2024
```

These handle people guessing future dataset names.

### 4. File Format Redirects (Throughout)

Fix incorrect file extensions and formats:

```nginx
"~^/path/file.png.*$" "/path/file.jpg";  # Wrong extension
"~^/path/file.jpeg.*$" "/path/file.jpg";  # JPEG → JPG normalization
```

Similar to nginx.conf but for more obscure/broken variants.

### 5. External Link Redirects (Throughout)

When local PDFs aren't available, redirect to authoritative sources:

```nginx
"~^/doc/genetics/heritable/2018-prasad\.pdf.*$"
    "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5994200/";

"~^/doc/psychology/1943-maslow.pdf.*$"
    "https://psycnet.apa.org/journals/rev/50/4/370/";
```

**Pattern:** Local path → PubMed Central, PsycNET, Archive.org, etc.

### 6. Author Name Variations (Lines 10000-10026)

Extensive disambiguation for common surnames:

```nginx
# Schwartz variations
"~^/1994-schwartz\.pdf.*$" "/doc/iq/1994-schwartz.pdf";
"~^/2017-schwartz\.pdf.*$" "/doc/genetics/heritable/correlation/2017-schwartz.pdf";
"~^/1997-schwartz\.pdf.*$" "/doc/statistics/bias/1997-schwartz.pdf";
"~^/1998-swartz\.pdf.*$" "/doc/fiction/1998-swartz.pdf";  # Different author

# Horowitz/Horwitz variations
"~^/2019-horowitz\.pdf.*$" "/doc/sociology/2019-horowitz.pdf";
"~^/1990-horwitz\.pdf.*$" "/doc/statistics/causality/1990-horwitz.pdf";
"~^/2018-horwitz\.pdf.*$" "/doc/genetics/heritable/2018-horwitz.pdf";
```

**Problem:** Year-author citations are ambiguous when multiple papers exist

**Solution:** Redirect based on most likely context or most popular paper

### 7. Malicious/Broken Crawler Requests (End of file)

**Purpose:** Prevent error log spam from broken bots

```nginx
"~^//content.php.*$" "/404";
"~^/class-php.*$" "/404";
"~^//bp.php.*$" "/404";
"~^/.windsurf.*$" "/404";  # Editor config files
"~^/.cursor.*$" "/404";     # Cursor editor
"~^/smartoptimizer.*$" "/404";
"~^/OVA-UN.*$" "/404";
"~^/outfits.*$" "/404";
"~^//modules/.*$" "/404";
```

**Blocked patterns:**
- PHP files (gwern.net is static)
- WordPress paths (site isn't WordPress)
- Config files (`.bashrc`, `.profile`, etc.)
- Credentials (`.credentials.json`, `aws.json`, `stripe.*`)
- Development artifacts (`fly.toml`, `k8s.yml`, `meteor.settings`)
- Editor configs (`.windsurf`, `.cursor`)

### 8. Security-Sensitive Blocks

Explicit blocking of credential-seeking requests:

```nginx
"~^/\.*_credentials\\.json$" "/404";
"~^/\.bash_profile.*$" "/404";
"~^/\.bashrc.*$" "/404";
"~^/\.profile.*$" "/404";
"~^/\.sqlite3.*$" "/404";
"~^/aws\.json.*$" "/404";
"~^/gcp-key.*$" "/404";
"~^/stripe\..*$" "/404";
"~^/stripe_.*$" "/404";
"~^/~/\.aws/.*$" "/404";
```

**Attack pattern:** Bots scanning for exposed credentials and config files

**Defense:** Explicitly return 404 (could also return 403 Forbidden)

### 9. Removed Features (End of file)

```nginx
# removed feature:
"~^/static/previews/.*\.png$" "/404";
```

Link previews feature was removed; old requests get 404.

### 10. Regex Substitution Rules (Near end)

**Pattern cleanup using capture groups:**

```nginx
"~(?<u>.*)\.$" "$u";  # Strip trailing dots
"~(?<u>.*)/trackback$" "$u";  # Remove /trackback suffix
"~(?<u>.*)/trackback/$" "$u";  # Remove /trackback/ suffix
"~(?<u>.*)\'$" "$u";  # Strip trailing apostrophes
```

**Named capture syntax:**
- `(?<u>.*)` - Capture everything into variable `u`
- `$u` - Reference captured content in destination

**Purpose:** Clean up malformed URLs from broken crawlers/tools

### 11. Special Cases and Edge Cases

```nginx
# Link bibliography redirect
"~^/doc/link-bibliography.*$" "/404";

# Google URL mangling
"~^https:/creativecommons.org/publicdomain/zero/1.0/$"
    "https:/creativecommons.org/publicdomain/zero/1.0/";

# External site misdirections
"~^/item\?id=18675280.*$" "https://news.ycombinator.com/item?id=18675280";
"~^/pubmed/26853120.*$" "https://pubmed.ncbi.nlm.nih.gov/pubmed/26853120";
```

## Redirect Destination Types

### 1. Internal Redirects

Point to correct path within gwern.net:
```nginx
"~^/wrong/path$" "/correct/path";
```

### 2. External Redirects

Point to authoritative external sources:
```nginx
"~^/local/file.pdf$" "https://pubmed.ncbi.nlm.nih.gov/...";
```

### 3. Explicit 404s

Send obviously-malicious requests to 404:
```nginx
"~^/malicious/pattern.*$" "/404";
```

### 4. Anchor-Based Redirects

Redirect to section of a page:
```nginx
"~^/old-page$" "/new-page#section";
"~^/Danbooru2000$" "/danbooru2021#danbooru2020";
```

## URL Corruption Patterns Handled

### 1. Encoding Issues

```nginx
"~^/Danbooru2020%E3%81.*$" "/dabooru2020";  # UTF-8 encoding garbage
"~^/danbooru2020%C3%AF%C2%BC%C5%922020$" "/danbooru2020";
```

**Cause:** Double-encoding, charset mismatches, broken link parsers

### 2. Path Duplication

```nginx
"~^/doc/rotten.*/https/www.edge.org/conversation/...$"
    "https://www.edge.org/conversation/...";
```

**Pattern:** `/doc/rotten.com/` prefix incorrectly prepended to external URLs

### 3. Truncation

```nginx
"~^/Danboo$" "/danbooru201";
"~^/banboor.*$" "/danbooru201";
```

**Cause:** Character limits in referrers, truncated bookmarks

### 4. Case Variations

```nginx
"~^/[Dd]anbooru2022.*$" "/danbooru2021";
```

**Pattern:** Case-insensitive matching for common typos

## Pattern Analysis

### Common Typo Classes

**Transposition:**
- `dabooru` instead of `danbooru` (transposed 'n' and 'a')

**Omission:**
- `danboru` instead of `danbooru` (missing 'o')

**Duplication:**
- `dabbooru` instead of `danbooru` (doubled 'b')

**Truncation:**
- `danbooru204` instead of `danbooru2020` (incomplete year)

### Newsletter Date Issues

**Zero-padding confusion:**
- `2020/1` vs `2020/01`
- `2020/010` (double-digit typo)
- `2020/00` (zero month)

**Pattern:** Users forget zero-padding, automation adds extra zeros

## Performance Characteristics

### Map Size Impact

**28,822 rules** in a single nginx map:

**Lookup performance:**
- Literal matches: O(1) hash lookup (fast)
- Regex matches: O(n) sequential evaluation (slower)
- This file is almost entirely regex, so worst-case performance

**Memory footprint:**
- ~2MB config file
- Loaded into nginx memory on startup
- Shared across all worker processes

**Mitigation strategies:**
- Most requests hit cache/CDN (redirects infrequent)
- Regex patterns anchored (`^`/`$`) for early rejection
- Common patterns listed first (likely optimization)

## Maintenance Philosophy

### Zero Error Log Policy

**Goal:** Every URL request should have a defined response (even if 404)

**Evidence:**
- Exhaustive typo coverage
- Malicious pattern blocking
- Explicit 404s for removed features

**Benefit:** Error logs contain only genuine issues, not noise

### Defensive Programming

**Approach:** Anticipate all possible broken inputs

**Patterns:**
- Multiple redirects for same destination (typo variants)
- Year-by-year newsletter redirects (dates 2014-2027+)
- Security blocks for common vulnerability scanners

### Historical Accumulation

**Growth pattern:**
- File grows as new broken links are discovered
- Each external citation creates potential for future typos
- Bot attacks add new malicious patterns to block

**Evidence:** Temporal comments about removing redirects in future years

## Integration with nginx.conf

### Division of Responsibility

| File | Purpose | Example |
|------|---------|---------|
| **nginx.conf** | Legitimate content moves | Reorganized files, renamed pages |
| **nginx-broken.conf** | Broken/malicious URLs | Typos, bots, malformed requests |

### Combined Processing

Both files are likely included in the same nginx map:

```nginx
map $request_uri $redirect_uri {
    include /path/to/redirect/nginx.conf;
    include /path/to/redirect/nginx-broken.conf;
}
```

**Evaluation order:**
1. First match wins
2. nginx.conf rules probably evaluated first (legitimate redirects)
3. nginx-broken.conf catches remaining broken patterns
4. Default (empty) means no redirect → serve normally or 404

## Security Considerations

### Bot Protection

Blocks common vulnerability scanner patterns:
- WordPress paths (`/wp-admin`, `/modules/`)
- PHP files (site is static)
- Config files (credentials, env files)
- Infrastructure configs (Kubernetes, Docker, etc.)

**Goal:** Reduce attack surface and log noise

### Credential Harvesting Defense

Explicit blocks for:
- AWS credentials
- GCP keys
- Stripe API keys
- Generic `_credentials.json`
- Shell config files

**Attack vector:** Automated scanners looking for exposed secrets

### Rate Limit Considerations

**Impact:** Each redirect is a server response

**Mitigation:**
- Rate limiting on IP level (separate nginx config)
- CDN/cache layer prevents most redirect hits
- 404 responses are cheap (no disk I/O)

## Temporal Redirects

### Future-Dated Patterns

```nginx
"~^/[Dd]anbooru2022.*$" "/danbooru2021"; # WARNING: TO REMOVE in JANUARY 2023
"~^/[Dd]anbooru2023.*$" "/danbooru2022"; # WARNING: TO REMOVE in JANUARY 2024
```

**Strategy:** Catch people guessing future dataset names

**Maintenance burden:** Requires manual removal in future years

**Alternative approach:** Generic pattern like `"~^/[Dd]anbooru20[2-9][0-9].*$"` (but less precise)

### Newsletter Redirects for Future Years

```nginx
# "~^/2025/10$" "/newsletter/2025/10";
# "~^/2026/10$" "/newsletter/2026/10";
# "~^/2027/10$" "/newsletter/2027/10";
```

**Status:** Commented out

**Reason:** Presumably too forward-looking or causing conflicts

## Common Failure Modes Addressed

### 1. Copy-Paste Errors

Users copy URL with trailing punctuation:
```nginx
"~(?<u>.*)\'$" "$u";  # Strip trailing apostrophe
"~(?<u>.*)\.$" "$u";   # Strip trailing period
```

### 2. Referrer Truncation

Referrer headers or bookmarks truncate URLs:
```nginx
"~^/Danboo$" "/danbooru201";
```

### 3. URL Encoding Corruption

Double-encoding or charset issues:
```nginx
"~^/Danbooru2020%E3%81.*$" "/dabooru2020";
```

### 4. Autocorrect Interference

User's device autocorrects URL:
```nginx
"~^/Danbooru2020It$" "/danbooru2021#danbooru2020";  # "It" autocorrect
```

## Regex Patterns Used

### Named Captures

```nginx
"~(?<u>.*)\.$" "$u";
```

**Syntax:** `(?<name>pattern)` captures into `$name`

### Wildcards and Anchors

```nginx
"~^/exact-prefix.*$" "/destination";
```

- `^` - Start of URL
- `.*` - Any characters
- `$` - End of URL

### Character Classes

```nginx
"~^/[Dd]anbooru2022.*$" "/danbooru2021";
```

- `[Dd]` - Matches 'D' or 'd'

### Escape Sequences

```nginx
"~^/\.*_credentials\\.json$" "/404";
```

- `\\.` - Literal dot
- `\.*` - Any number of literal dots (e.g., `._credentials.json`)

## Maintenance Workflow

### Adding New Broken URL Fixes

**Process:**
1. Monitor nginx error logs for 404s
2. Identify patterns in broken URLs
3. Add redirect rule to nginx-broken.conf
4. Test redirect
5. Deploy and monitor

**Example workflow:**
```bash
# Find common 404 patterns
grep "404" /var/log/nginx/error.log | sort | uniq -c | sort -rn

# Add redirect rule
echo '"~^/broken-pattern.*$" "/correct-path";' >> nginx-broken.conf

# Test configuration
nginx -t

# Reload
nginx -s reload
```

### Cleanup Strategy

**When to remove rules:**
- Temporal redirects past their expiration date
- Redirects with zero hits for extended period
- Superseded by broader pattern matches

**Challenges:**
- Hard to know if redirect still needed
- External sites may link to old URLs indefinitely
- Conservative approach: keep everything

## Statistics and Insights

| Metric | Value |
|--------|-------|
| Total redirects | 28,822 |
| File size | 1.9 MB |
| Lines | 29,132 |
| Literal 404 blocks | ~50+ security-related |
| Danbooru typo variants | 15+ variations |
| Newsletter date fixes | 100+ date variations |
| External redirects | Hundreds (to PubMed, LibGen, etc.) |
| Author name disambiguations | Dozens |

**Pattern distribution (estimated):**
- 40% File path corrections and format fixes
- 25% Typo and variation handling
- 20% External source redirects
- 10% Security blocks (bots, malicious requests)
- 5% URL corruption cleanup

---

## See Also

- [redirect-nginx](/nginx/redirect-nginx) - Legitimate content move redirects
- [gwern.net.conf](/nginx/gwern-net-conf) - Main nginx config that includes these redirect maps
- [NginxRedirectGuesser.hs](/backend/nginx-redirect-guesser-hs) - Haskell tool for generating redirect rules
- [LinkMetadata.hs](/backend/link-metadata-hs) - Link metadata database with canonical URLs
- [LinkArchive.hs](/backend/link-archive-hs) - Link archival system (may track broken links)
- [sync.sh](/backend/sync-sh) - Build/deploy script that syncs nginx configs

## Philosophy: Embrace the Chaos

This file represents a pragmatic approach to the messy reality of the web:

**Accept that URLs will break:**
- Typos happen
- Bots will misbehave
- URLs get corrupted in transit
- Users copy-paste carelessly

**Handle it gracefully:**
- Redirect to correct content when possible
- Return clean 404s for malicious requests
- Reduce error log noise
- Maintain link graph integrity

**Learn from history:**
- Each broken URL teaches a pattern
- Accumulate fixes over time
- Build comprehensive coverage

**Result:** A robust, resilient link structure that degrades gracefully even when users (or bots) provide garbage input.
