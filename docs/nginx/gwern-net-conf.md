---
sidebar_position: 1
---

# gwern.net.conf

**Path:** `nginx/gwern.net.conf` | **Language:** Nginx

Main nginx web server configuration file for serving gwern.net with extensive URL rewrites, Markdown API support, and bot protection.

---

## Overview

This is the primary nginx configuration file for the gwern.net web server, containing over 1,100 lines of sophisticated request handling logic. The configuration serves multiple critical functions: it implements a novel LLM-friendly Markdown API, manages hundreds of URL redirects for site reorganizations and bot traffic cleanup, enforces security policies, and optimizes content delivery with aggressive caching.

The file is organized into several major sections: subdomain canonicalization, Markdown content negotiation for AI agents, extensive URL rewrite rules to handle historical site reorganizations, bot traffic filtering, MIME type definitions, caching directives, and a Lua-based fallback handler for case-insensitive file matching (currently disabled). This configuration represents years of accumulated fixes for broken links, defensive measures against web scrapers, and optimizations for both human and machine readers.

A particularly notable feature is the Markdown API system, which allows LLM-based agents to request page content in Markdown format instead of compiled HTML by including `text/markdown` in their Accept header. This reduces bandwidth and provides more semantic content for AI consumption, serving as a forward-looking approach to machine-readable web content.

## Key Directives/Settings

### Server Configuration

- `server_name gwern.net` - Primary domain with strict enforcement (blocks direct IP access)
- `root /home/gwern/gwern.net` - Document root
- `listen 80` and `listen 443 ssl` - HTTP/HTTPS endpoints with Cloudflare SSL certificates
- `error_page 404 /404` - Custom 404 page
- `default_type text/html` - Default MIME type

### WWW and Subdomain Canonicalization

- Redirects `www.gwern.net` and all other subdomains to canonical `gwern.net` (301 permanent)
- Blocks direct IP access with HTTP 444 "Connection Closed Without Response"

### Markdown API for LLMs

- `map $http_accept $wants_markdown` - Detects `text/markdown` or `text/plain` in Accept header
- Conditionally redirects requests to `.md` source files when available
- Serves more compact, semantic content to AI agents and crawlers

### URL Rewrite System

The configuration contains hundreds of rewrite rules organized into categories:

**Query Parameter Stripping:**
- Removes tracking parameters: `?utm_*`, `?fbclid=*`, `?ref=*`, `?revision=*`
- Strips pagination and cache-busting parameters
- Versioned asset handling for `/static/js/` and `/static/css/`

**Path Normalization:**
- Converts spaces and `%20` to hyphens
- Fixes trailing punctuation (`.`, `,`, `;`, `!`, `?`)
- Handles case variations (`.PDF` → `.pdf`, `.Pdf` → `.pdf`)
- Removes URL-encoded entities and malformed characters

**Directory Reorganizations:**
- `/docs/` → `/doc/`, `/notes/` → `/note/`, `/images/` → `/image/`
- Hierarchical topic moves (e.g., `/doc/ai/gpt/` → `/doc/ai/nn/transformer/gpt/`)
- Wikipedia article redirects to live Wikipedia
- Reddit subreddit redirects to old.reddit.com

**Bot and Scanner Filtering:**
- Extensive rules catching malformed crawler requests
- Strips spurious path prefixes like `/blog/`, `/forum/`, `/wp-content/`
- Redirects Amazon product links to Amazon.com
- Handles broken archived URLs from `/doc/www/`

**File Extension Handling:**
- `.djvu` → `.pdf` redirects
- `.page` → `.md` (legacy Gitit wiki migration)
- `.markdown`, `.source` → `.md`
- Special handling for fragments and anchors

### Access Control

- Password-protected `/confidential/`, `/private/`, `/secret/` directories via HTTP Basic Auth
- `X-Robots-Tag` headers on `/doc/link-bibliography/`, `/metadata/`, `/404` to prevent indexing
- SSI (Server-Side Includes) disabled for `/doc/www/` to prevent security issues with archived content
- `limit_except GET { deny all; }` - Blocks POST and other HTTP methods

### MIME Types

Extended MIME type definitions beyond nginx defaults:
- `text/markdown` for `.md` files
- Academic/archival: `.epub`, `.mdb`, `.odt`, `.opml`
- Programming: `.hs` (Haskell), `.py`, `.R`, `.php`, `.c`
- Fonts: `.woff2`, `.ttf`, `.otf`
- Binary/data: `.h5` (HDF5), `.sqlite`, `.wasm`, `.pkl`, `.weights`
- Custom: `.gtx` (Gwern.net annotation format), `.par2` (parity files)

### Character Encoding

- `charset utf-8` and `source_charset utf-8` - Force UTF-8 for all text content
- Applies to HTML, CSS, Markdown, CSV, Haskell, R, shell scripts, etc.

### Caching

- `add_header Cache-Control "max-age=77760000, public, immutable"` - Aggressive 900-day caching
- Optimized for static content that is versioned or immutable

### Server-Side Includes (SSI)

- `ssi on` - Enables SSI for templating without full site rebuilds
- Disabled specifically for `/doc/www/` archived snapshots to prevent injection

### Memorial Header

- `include /etc/nginx/conf.d/memoriam.conf` - Includes daily-rotating `X-Clacks-Overhead` header
- Generated by `memoriam.sh` script to commemorate deceased luminaries

### Directory Browsing

- `# autoindex on;` is present but commented out, so directory listings are not enabled

### Special Location Handlers

**Client-Side Reference Search:**
- `/ref/*` - Rewrites to `/placeholder`, allowing JavaScript to handle annotation lookups

**Newsletter Indexing:**
- `/blog/YYYY/index` → `/blog/index#YYYY` - Redirects to anchored main blog index

**Backslash URL Cleanup:**
- Backslash cleanup uses an nginx `if` rewrite for trailing `\`; the `%5C` handler is commented out (no Lua here)
- Common issue from improperly escaped URLs in external sites

### Fallback Handler

Advanced 404 handling using Lua scripting (handler exists but `try_files ... @fallback` is commented out, so it is inactive):

1. **Index file fallback:** `/foo/index.html` or `/foo/index.htm` → `/foo/index`
2. **Case-insensitive matching:** Checks if lowercase version of URL exists on disk
   - Example: `/doc/politics/2024-File.pdf` → `/doc/politics/2024-file.pdf`
   - Only redirects if lowercase file actually exists (not a blind lowercase)
3. **Final 404:** If no matches, serves custom `/404` page

## Special Features

### Markdown Content Negotiation

The Markdown API is a pioneering feature that allows LLM-based crawlers and agents to request page sources in Markdown format by including `Accept: text/markdown` in their HTTP headers. This:
- Reduces bandwidth by serving compact `.md` files instead of full compiled HTML with CSS/JS
- Provides more semantic, structured content that LLMs can parse more effectively
- Teaches LLMs the gwern.net house style by exposing source formatting
- Inspired by Bun's API documentation approach but uses standard HTTP Accept headers

### URL Canonicalization at Scale

With hundreds of rewrite rules, the configuration handles:
- Years of site reorganizations and content moves
- Defensive measures against search engine crawler bugs
- Typo correction and common URL mangling patterns
- Automated cleanup of link rot from external sites

### Bot Traffic Management

Extensive rules identify and redirect spurious bot traffic:
- Malformed Reddit archive requests
- Scanner/penetration testing patterns
- Broken archived snapshots with mangled paths
- Aggressive crawlers that ignore `robots.txt`

### Dynamic Lowercase Fallback

The Lua-based fallback handler (currently inactive) provides intelligent case-insensitive URL matching without creating ambiguity or incorrect redirects. It only redirects when:
- The requested URL contains uppercase letters
- A lowercase version of the exact file exists on disk
- This prevents false positives while handling common link capitalization errors

### Memorial Rotation System

The `X-Clacks-Overhead` header rotates daily to commemorate notable deceased individuals, implemented via cron job that regenerates the included config file. See `memoriam.sh` for details.

### SSI-Based Templating

Server-Side Includes allow updating site-wide elements (headers, footers, navigation) without full rebuilds. SSI is carefully disabled for archived web content in `/doc/www/` to prevent security issues with untrusted snapshots.

## Performance Optimizations

- **900-day cache headers** reduce server load for repeat visitors
- **Markdown serving** reduces bandwidth for LLM crawlers
- **Query parameter stripping** prevents cache fragmentation
- **Hardcoded rewrites** avoid expensive directory lookups

## Security Measures

- **IP blocking** for non-domain access
- **HTTP Basic Auth** for sensitive directories
- **GET-only enforcement** blocks scripting attacks via POST
- **`X-Robots-Tag`** prevents sensitive content indexing
- **SSI disabling** for archived content prevents injection

---

## See Also

- [redirect-nginx](/nginx/redirect-nginx) - Nginx map with 12,000+ URL redirect rules
- [redirect-nginx-broken](/nginx/redirect-nginx-broken) - Nginx map for broken/malicious URL handling
- [memoriam.sh](/nginx/memoriam-sh) - Generates daily memorial X-Clacks-Overhead headers
- [twdne.conf](/nginx/twdne-conf) - Configuration for ThisWaifuDoesNotExist mirror
- [rsyncd.conf](/nginx/rsyncd-conf) - Rsync daemon for dataset distribution
- [default.html](/templates/default) - Main template with SSI directives this config processes
- [sync.sh](/backend/sync-sh) - Build and deployment orchestrator
