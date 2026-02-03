# move.conf

**Path:** `nginx/redirect/move.conf` | **Language:** Nginx

Nginx redirect map configuration for gwern.net URL canonicalization and historical redirects.

---

## Overview

`move.conf` is a massive nginx map configuration file containing **12,810 redirect rules** that handle URL canonicalization, historical page moves, file reorganizations, and path standardization across gwern.net. This file ensures that old URLs, externally-linked content, and reorganized resources continue to work as the site evolves.

The configuration uses nginx's `map` directive to define pattern-based redirects, supporting both literal matches and regex patterns. It serves as a historical record of site reorganization decisions while maintaining link permanence for external citations and bookmarks.

**Key insight:** This file implements the server-side portion of gwern.net's URL redirect strategy. Client-side redirects for in-page anchors (hash fragments) are handled separately via JavaScript (see style-guide#renaming).

## File Structure

**Location:** `nginx/redirect/move.conf`

**Type:** Nginx map configuration

**Size:** 12,836 lines, ~1.5MB

**Format:** Key-value redirect map
```nginx
"~^/old/path$" "/new/path";
```

## Nginx Map Syntax

The file defines a map with this structure:

```nginx
default "";

"~^/pattern$" "/destination";
```

**Components:**

1. **Default value:** Empty string (no redirect)
2. **Source pattern:** Regex or literal string (prefixed with `~^` for regex)
3. **Destination:** New path (relative to gwern.net root)

**Pattern format:**
- `~^` - Regex match (nginx syntax)
- `$` - End anchor (prevents partial matches)
- `.*$` - Match any trailing query strings or junk
- Named capture groups: `(?<u>...)` for reuse in destination

## Major Redirect Categories

### 1. Repository Restructuring (Lines 7-45)

**Purpose:** Moved build scripts and source code from site root to `/static/build/`

**Pattern:** Build files, Haskell modules, shell scripts

```nginx
"~^/Definition.hs$" "/static/build/Definition.hs";
"~^/LinkMetadata.hs$" "/static/build/LinkMetadata.hs";
"~^/sync-gwern.net.sh$" "/static/build/sync-gwern.net.sh";
```

**Rationale:** Separates source code from content, cleaner site organization

### 2. Metadata Reorganization (Lines 22-24)

```nginx
"~^/static/metadata/archive.hs$" "/metadata/archive.hs";
"~^/static/metadata/auto.yaml$" "/metadata/auto.gtx";
"~^/static/metadata/custom.yaml$" "/metadata/full.gtx";
```

**Changes:**
- Moved out of `/static/` directory
- YAML format changed to GTX format
- `custom.yaml` renamed to `full.gtx`

### 3. Logo Image Consolidation (Lines 25-38)

All logo variants moved from `/images/logo*` to `/static/img/logo/`:

```nginx
"~^/images/logo-smooth.svg$" "/static/img/logo/logo-smooth.svg";
"~^/images/logo-whitebg.png$" "/static/img/logo/logo-whitebg.png";
```

**Affected files:** 14+ logo variants (SVG, PNG, JPG, different themes)

### 4. The Great Image Renaming (Lines 47+)

**Scope:** Thousands of images moved from `/image/` to `/doc/` subdirectories

**Sections:**
- **Fonts (Lines 48-194):** Dropcap fonts consolidated under `/static/font/dropcap/`
- **Images (Lines 195+):** Topic-specific images moved to match content structure

**Pattern:**
```nginx
"~^/image/anime/eva/...jpg.*$" "/doc/anime/eva/...jpg";
"~^/image/biology/...png.*$" "/doc/biology/...png";
```

**Rationale:** Organize images alongside related content rather than in flat `/image/` directory

### 5. SICP Page Extension Changes (Lines 40-45)

```nginx
"~^/sicp/Chapter-1\.1.page$" "/sicp/Chapter-1-1.md";
"~^/sicp/Chapter-1\.1$" "/sicp/Chapter-1-1";
```

**Changes:**
- `.page` extension removed (changed to `.md`)
- Dots in filenames changed to hyphens
- Both with and without extension supported

### 6. File Format Corrections (Throughout)

Hundreds of redirects fixing incorrect file extensions:

```nginx
"~^/path/file.png.*$" "/path/file.jpg";  # PNG → JPG
"~^/path/file.jpeg.*$" "/path/file.jpg"; # JPEG → JPG
```

**Common patterns:**
- `.png` → `.jpg` (image format standardization)
- `.jpeg` → `.jpg` (extension normalization)
- Trailing `.*$` catches query strings and junk

### 7. Content Reorganization by Topic (Throughout)

Files moved into topic-specific subdirectories:

```nginx
"~^/doc/psychology/1996-berman.pdf.*$" "/doc/psychology/inner-voice/1996-berman.pdf";
"~^/doc/statistics/bayes/2007-pascual.pdf.*$" "/doc/statistics/bayes/abc/2007-pascual.pdf";
```

**Pattern:** Flat topic directory → topical subdirectory structure

### 8. Short URL Aliases (End of file)

```nginx
"~^/nadia$" "/doc/anime/nadia/index";
"~^/darcs$" "/doc/cs/haskell/darcs/index";
"~^/queueing$" "/doc/statistics/probability/queueing/index";
"~^/phenibut$" "/doc/nootropic/phenibut/index";
```

**Purpose:** Memorable short URLs redirect to full paths

## Redirect Pattern Types

### 1. Exact Match Redirects

```nginx
"~^/exact/path$" "/new/path";
```

Matches only the exact path, no variations.

### 2. Extension-Flexible Redirects

```nginx
"~^/path/file\.jpg.*$" "/new/path/file.jpg";
```

The `.*$` suffix handles:
- Query strings: `file.jpg?version=2`
- Fragments: `file.jpg#section`
- Garbage characters appended by broken crawlers

### 3. Capture Group Redirects

```nginx
"~^/image/(?<topic>.*)/(?<file>.*)$" "/doc/$topic/$file";
```

Captures parts of the old URL for reuse in the new URL (though most redirects in this file use explicit paths).

## Regex Patterns Used

### Anchors
- `~^` - Start anchor (nginx map syntax for regex)
- `$` - End anchor

### Wildcards
- `.*` - Match any characters
- `.*$` - Match anything until end of URL

### Escaping
- `\.` - Literal dot (escaped)
- `\-` - Literal hyphen (in some contexts)

### Character Classes
- `[0-9]` - Digits (used in some rules)

## Comments and Documentation

The file includes section headers:

```nginx
## The great image renaming:
### Fonts:
### Images:
## Redirect the small ones too:
```

**Header comment (Lines 3-4):**
```nginx
# NOTE: nginx redirects can only handle URL-level redirects for updating/moving things.
# They cannot handle links to IDs *within* pages, using hashes.
# To do that, we use custom client-side JS which read '.redirect-from-id' classes/attributes.
# See </style-guide#renaming> for more.
```

**Key limitation:** Hash-based redirects (e.g., `#section-id`) require JavaScript, as nginx never sees hash fragments.

## Historical Context

### Repository Splitting
Early redirects (lines 7-45) suggest gwern.net underwent a "great repo splitting" where:
- Build scripts moved out of root
- Source code consolidated under `/static/build/`
- Clear separation between content and tooling

### Image Organization Evolution
The "great image renaming" (thousands of rules) shows transition from:
- **Old:** Flat `/image/` directory by topic
- **New:** Images live alongside content in `/doc/` subdirectories

This mirrors modern static site best practices (colocate assets with content).

### Format Standardization
Many redirects normalize file formats:
- JPEG → JPG
- PNG → JPG (when format was wrong)
- Page extensions removed or standardized

## Nginx Integration

This file is included in the main nginx configuration via a `map` directive:

```nginx
map $request_uri $new_uri {
    include /path/to/redirect/move.conf;
}

server {
    if ($new_uri) {
        rewrite ^(.*)$ $new_uri permanent;
    }
}
```

The map populates `$new_uri` which triggers a permanent rewrite when non-empty.

## Maintenance Patterns

### Adding New Redirects

When moving/renaming a file:

1. Add redirect entry: `"~^/old/path$" "/new/path";`
2. If file has extension, use `.*$` to catch query strings
3. Group related redirects with comments
4. Test redirect before deploying

### Testing Redirects

```bash
# Test nginx configuration
nginx -t -c /path/to/nginx.conf

# Test specific redirect
curl -I https://gwern.net/old/path
# Should return: HTTP/1.1 301 Moved Permanently
# Location: /new/path
```

## Performance Considerations

**Map efficiency:**
- Nginx maps are hash-based for literal keys (O(1) lookup)
- Regex patterns require sequential evaluation (O(n) worst case)
- 12,000+ rules could impact performance on regex matches

**Optimization opportunities:**
- Group common patterns
- Use literal matches when possible (faster than regex)
- Consider separating frequently-accessed redirects

**Current approach:**
- Accepts linear regex evaluation cost
- Values link permanence over microsecond performance
- Redirects are infrequent (users access final URLs after first visit)

## Companion File: nginx-broken.conf

See `nginx-broken.conf` documentation for:
- Broken crawler handling
- Malformed URL cleanup
- Security-related blocks
- Regex substitutions for garbage removal

**Division of responsibility:**
- **nginx.conf:** Legitimate redirects for moved content
- **nginx-broken.conf:** Handle broken/malicious requests

## Link Permanence Philosophy

This file embodies gwern.net's commitment to **Cool URIs don't change**:

> "When you change a URI on your server, you can never completely tell who will have links to the old URI. They might have bookmarked your page. They might have scrawled the URI in the margin of a book. They might have written it on the back of an envelope."
> — Tim Berners-Lee

**Evidence:**
- 12,810 redirect rules maintained indefinitely
- Redirects span years of site evolution
- Even minor file moves get permanent redirects
- External citations continue working indefinitely

## Common Redirect Patterns

### Pattern 1: Directory Reorganization

```nginx
"~^/doc/topic/file.pdf.*$" "/doc/topic/subtopic/file.pdf";
```

Moving files deeper into category hierarchy.

### Pattern 2: Asset Consolidation

```nginx
"~^/images/asset.png$" "/static/img/asset.png";
```

Consolidating assets into standard directories.

### Pattern 3: Extension Normalization

```nginx
"~^/path/file.jpeg.*$" "/path/file.jpg";
```

Standardizing file extensions.

### Pattern 4: Short URL Creation

```nginx
"~^/short$" "/doc/category/long/path/index";
```

Creating memorable aliases for deep paths.

## Build System Integration

This file is likely:
1. **Generated partially** - Some redirects may be auto-generated from file moves
2. **Hand-maintained partially** - Custom redirects added manually
3. **Version controlled** - Git tracks all redirect additions/changes
4. **Deployed atomically** - Nginx reloads apply all redirects at once

---

## See Also

- [redirect-nginx-broken](/nginx/redirect-nginx-broken) - Broken URL and security-related redirects
- [gwern.net.conf](/nginx/gwern-net-conf) - Main nginx config that includes these redirect maps
- [NginxRedirectGuesser.hs](/backend/nginx-redirect-guesser-hs) - Haskell tool for generating redirect rules
- [LinkArchive.hs](/backend/link-archive-hs) - Link archival and URL management system
- [LinkMetadata.hs](/backend/link-metadata-hs) - Link metadata database (tracks canonical URLs)
- [sync.sh](/backend/sync-sh) - Build/deploy script that syncs nginx configs

## Summary Statistics

| Metric | Value |
|--------|-------|
| Total redirects | 12,810 |
| File size | 1.5 MB |
| Lines | 12,836 |
| Major categories | 8+ (repo split, images, fonts, logos, content reorg) |
| Oldest redirects | Repo splitting (early site history) |
| Newest redirects | Ongoing content reorganization |
| Regex patterns | ~95% (most use `~^` prefix) |
| Extension handling | Extensive `.*$` suffix usage |
