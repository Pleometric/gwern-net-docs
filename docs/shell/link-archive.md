---
sidebar_position: 4
---

# linkArchive.sh

**Path:** `build/linkArchive.sh` | **Language:** Bash | **Lines:** ~190

Archives web pages and PDFs locally using SingleFile snapshots for preemptive link preservation.

---

## Overview

This script implements gwern.net's preemptive local archiving system, which creates permanent local copies of linked web pages to prevent link rot. It uses SingleFile (via Docker) to create static, self-contained HTML snapshots of web pages, and downloads PDFs directly. All archived content is stored in a deterministic directory structure: `doc/www/$DOMAIN/$SHA1.{html,pdf}`.

The script intelligently handles multiple modes (normal archiving, checking for existing archives, dry-run path calculation, no-preview mode), detects content types (HTML vs PDF), handles special cases (Substack sites that break with JavaScript, anchor fragments), and optionally previews archived pages in a browser for quality verification.

For large (>5MB) HTML files, it can optionally decompose the monolithic SingleFile snapshot back into a normal HTML+assets structure for more efficient loading.

## Key Commands/Variables

- **`URL`**: The URL to archive
- **`CHECK`**: Mode flag - only check if archive exists, never create
- **`NO_PREVIEW`**: Mode flag - archive but don't open browser preview
- **`DRY_RUN`**: Mode flag - return hypothetical file path without any I/O
- **`HASH`**: SHA1 hash of URL (minus anchor) for deterministic filenames
- **`DOMAIN`**: Extracted domain for directory organization
- **`ANCHOR`**: Fragment identifier (#...) from URL, preserved in output path
- **`MIME_REMOTE`**: Content-Type from HTTP headers
- **`USER_AGENT`**: Firefox user agent string
- **`REMOVE_SCRIPTS`**: Boolean flag for sites that break with JavaScript (e.g., Substack)

## Usage

```bash
./linkArchive.sh [--check|--no-preview|--dry-run] <URL>
```

**Modes:**

1. **Normal mode** (default): Archive URL and preview in browser
   ```bash
   $ linkArchive.sh 'http://example.com/article'
   doc/www/example.com/a1b2c3d4e5f6...html
   # Opens browser with original vs archived comparison
   ```

2. **`--check`**: Only check if archive exists, never create
   ```bash
   $ linkArchive.sh --check 'http://example.com/article'
   doc/www/example.com/a1b2c3d4e5f6...html
   # Or empty string if not found
   ```

3. **`--no-preview`**: Archive without browser preview
   ```bash
   $ linkArchive.sh --no-preview 'http://example.com/article'
   doc/www/example.com/a1b2c3d4e5f6...html
   ```

4. **`--dry-run`**: Calculate hypothetical path without I/O
   ```bash
   $ linkArchive.sh --dry-run "https://x.com/user/status/123"
   doc/www/x.com/8e93ed7854e0d7d8323cdbc667f946fee6f98d3d
   # Note: no extension, as PDF vs HTML is unknown without fetching
   ```

**Examples:**

```bash
# Archive with preview
$ linkArchive.sh 'http://www.jacurutu.com/viewtopic.php?p=101694'
/home/gwern/wiki/doc/www/www.jacurutu.com/718b0de585ef3dcd778a196fb2b8c842b42c7bc2.html

# Anchor fragments are preserved in output but not in filename
$ linkArchive.sh 'http://example.com/page.html#section'
doc/www/example.com/a1b2c3d4.html#section
```

**Archive directory structure:**
```
doc/www/
├── example.com/
│   ├── a1b2c3d4e5f6...html      # SingleFile snapshot
│   └── 9f8e7d6c5b4a...pdf       # Downloaded PDF
├── arxiv.org/
│   └── b2c3d4e5f6a1...pdf
└── twitter.com/
    └── c3d4e5f6a1b2...html
```

**Special handling:**

- **PDFs**: Detected by MIME type or URL pattern, downloaded with `wget`, optimized with `ocrmypdf` and JBIG2
- **Substack sites**: JavaScript stripped to prevent error-page refreshes
- **Export.arxiv.org**: URLs rewritten to use export mirror while preserving directory structure
- **Nitter domains**: Full request used instead of `--head` due to lying servers
- **Large files** (>5MB): Optionally deconstructed with `deconstruct_singlefile.php`

**Exit codes:**
- `1`: `curl` request failed
- `2`: 404 or 403 HTTP status
- `3`: `curl` HEAD request failed
- `4`: PDF download/validation failed
- `5`: HTML snapshot contains error page markers
- `98`: Wrong number of arguments
- `99`: No URL argument provided

**Dependencies:**
- `docker` with SingleFile image
- `chromium` browser (for SingleFile)
- `curl`, `wget`: HTTP clients
- `sha1sum`: Hash calculation
- `timeout`: Command timeouts
- `file`: MIME type detection
- `ocrmypdf`, `pdftk`: PDF processing (optional)
- `php` with `deconstruct_singlefile.php`: Large file decomposition (optional)

## See Also

- [LinkArchive.hs](/backend/link-archive-hs) - Haskell integration and database management
- [Config.LinkArchive](/backend/config-link-archive-hs) - URL whitelists and transformation rules
- [deconstruct_singlefile.php](/php/deconstruct-singlefile) - Splits large archives into HTML + assets
- [sync.sh](/backend/sync-sh) - Build process coordination
- [hakyll.hs](/backend/hakyll-hs) - Build system that triggers archive operations
- [gwsed.sh](/shell/gwsed) - URL rewriting that may trigger archiving
