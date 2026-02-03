
# sync.sh

**Path:** `build/sync.sh` | **Language:** Bash | **Lines:** ~1,900

Master build orchestrator for gwern.net: coordinates pre-processing, compilation, post-processing, validation, and deployment.

---

## Overview

`sync.sh` is the monolithic build script that transforms the gwern.net source (Markdown essays, annotations, images, PDFs) into a deployable static website. It runs from start to finish in a single invocation, typically taking 30-60+ minutes for a full build.

The script enforces a strict "fail fast" philosophy with `set -e`—any command failure aborts the entire build. This is intentional: partial builds can leave the site in inconsistent states. The script also lowers its own priority (`renice`/`ionice`) since builds are CPU and I/O intensive.

The architecture reflects years of organic growth. What began as a simple Hakyll wrapper has accumulated URL rewrites, string normalizations, validation checks, and deployment logic. The result is comprehensive but dense—modifications require understanding how phases interact.

---

## Build Phases

### Phase 1: Initialization & Dependency Check (lines 1-75)

Validates environment before any work begins.

```bash
DEPENDENCIES=(bc curl shuf dos2unix du elinks emacs exiftool ...)
```

- Sources `bash.sh` helper library
- Checks 50+ required tools are installed
- Verifies 6GB+ free disk space
- Ensures no other Hakyll process is running
- Cleans `_cache/` and `_site/` directories
- Removes Emacs temp files (`flycheck_*.hs`, `#*.md#`)

**Depends on:** System having all tools installed
**Produces:** Clean working directory, validated environment

### Phase 2: Infrastructure Update (lines 95-98)

```bash
git pull -Xtheirs --no-edit 'https://gwern.obormot.net/static/.git/' master
```

Pulls latest CSS/JS from Said Achmiz's repo. Uses `-Xtheirs` to auto-resolve conflicts favoring upstream.

### Phase 3: String Rewrites (lines 100-177, SLOW only)

Automated cleanup of common errors and style inconsistencies across all Markdown files. Runs only in `--slow` mode.

**URL rewrites:**
- `twitter.com/` → `x.com/`
- `http://arxiv.org` → `https://arxiv.org`
- `https://www.gwern.net` → `https://gwern.net`
- Strips tracking parameters (`?utm_`, `?fbclid=`, etc.)

**Name/entity fixes:**
- `Yann Le Cun` → `Yann LeCun`
- ` GPT2` → ` GPT-2`
- `chain of thought` → `chain-of-thought`

**Citation normalization:**
- `et al.` → `et al`
- `Foo et al (2020)` → `Foo et al 2020`

Uses `gwsed` function (defined in `bash.sh`) for safe global search-replace.

### Phase 4: Haskell Compilation (lines 182-200)

Compiles all Haskell build tools via Cabal.

```bash
cd ./static/build
cabal install
cabal clean
cd ../../
```

**Key binaries installed:**
- `hakyll` - Main site generator
- `generateLinkBibliography` - Creates per-page link bibliographies
- `generateDirectory` - Generates tag directory index pages
- `checkMetadata` - Validates annotation metadata
- `generateSimilarLinks` - Similar links computation (run via cron)

**Depends on:** GHC, Cabal, Haskell dependencies
**Produces:** Compiled executables installed to Cabal bin path

### Phase 5: Metadata Validation (lines 198-265, SLOW only)

Runs `checkMetadata` to validate all annotations. Checks for:
- Empty links (`href=""`)
- Malformed annotations
- File/directory name collisions (`foo.md` vs `foo/`)

Also regenerates link suggestions database and updates tag directories.

### Phase 6: Pre-Hakyll Generation (lines 245-265, SLOW only)

```bash
./static/build/hakyll build --annotation-missing-one-shot
./static/build/generateLinkBibliography +RTS -N"$N" -RTS
./static/build/generateDirectory +RTS -N3 -RTS $DIRECTORY_TAGS
```

Generates content that Hakyll needs:
1. Writes missing annotation HTML snippets
2. Creates link bibliographies for all pages
3. Builds tag directory index pages

### Phase 7: Asset Generation (lines 276-490)

Parallel generation of supporting assets:

**Video posters (3×3 filmstrip):**
```bash
SAMPLE_FPS=$(echo "scale=6; 10 / $DURATION" | bc)
ffmpeg -y -i "$VIDEO" \
    -vf "fps=$SAMPLE_FPS,scale=iw*sar:ih,setsar=1,scale=iw/3:ih/3,tile=3x3:nb_frames=9:padding=2:color=black" \
    -frames:v 1 "$POSTER"
compressJPG "$POSTER"
```
Generates 3×3 filmstrip grid sampling 9 frames evenly across video duration. Output dimensions match original video (each cell is 1/3 size, tiled 3×3). Used as `data-video-poster` attribute for lazy-loaded preview via IntersectionObserver (standard `poster=` attribute doesn't support lazy loading).

**Large video posters (5×5 filmstrip):**
```bash
ffmpeg -y -i "$VIDEO" \
    -vf "fps=$SAMPLE_FPS,scale=...,tile=5x5:nb_frames=25:padding=3:color=black" \
    -frames:v 1 "$TMPSTRIP"
convert "$TMPSTRIP" -gravity North -background '#000000' -splice 0x60 \
    -font IBM-Plex-Mono-Bold -pointsize 24 -fill white \
    -annotate +0+13 "$META_LINE" "$POSTER"
```
Generates 5×5 filmstrip (25 frames) with metadata overlay bar showing: filepath, duration, dimensions, file size, codec, bitrate, audio presence. Loaded on hover via JS at `$VIDEO-poster-large.jpg`. For videos &lt;1s, falls back to single enlarged frame.

**Image thumbnails:**
```bash
convert "$image" -resize 256x "$thumbnail_path"
```
Creates 256px-wide thumbnails at `/metadata/thumbnail/256px/` with URL-encoded paths. Thumbnails are generated for all locally-hosted JPGs/PNGs (excluding mirrors like doc/www/).

### Phase 8: Hakyll Build (lines 492)

The core compilation step:

```bash
time hakyll build +RTS -N"$N" -RTS
```

Hakyll (now installed via `cabal install`) processes all Markdown files through Pandoc, applies templates, and generates HTML. This is the longest single step (10-30+ minutes).

**Produces:** `_site/` directory with all HTML pages

### Phase 8.5: X-of-the-Day Updates (SLOW only)

After Hakyll build, updates rotating content features:

```bash
# Annotation-of-the-day
ghci -istatic/build/ ./static/build/XOfTheDay.hs ./static/build/LinkMetadata.hs \
     -e 'do {md <- LinkMetadata.readLinkMetadata; aotd md; }'

# Quote-of-the-day
ghci -istatic/build/ ./static/build/XOfTheDay.hs -e 'qotd'

# Site-of-the-day
ghci -istatic/build/ ./static/build/XOfTheDay.hs -e 'sotd'
```

These generate daily rotating content for the homepage.

### Phase 9: Post-Processing (lines 340-600)

Transforms Hakyll output for production:

**Sitemap generation (lines 356-369):**
```bash
find -L _site/doc/ _site/static/ ... | xargs urlencode -m | sed ...
```

**Document conversion (lines 371-435):**
LibreOffice converts `.doc`, `.docx`, `.xlsx`, `.csv` to HTML for popup previews.

**Syntax highlighting (lines 445-499):**
```bash
syntaxHighlight () {
    pandoc --from=markdown+smart --write=html5 --standalone \
           --template=./static/template/pandoc/sourcecode.html5 ...
}
```
Creates `.html` versions of source code files for popup previews.

**MathJax compilation (lines 507-527):**
```bash
~/src/mathjax-node-page/bin/mjpage --output CommonHTML --fontURL '/static/font/mathjax'
```
Pre-renders LaTeX to static CSS+HTML.

**Class cleanup (lines 584-600):**
Strips compile-time-only CSS classes (`archive-not`, `link-annotated-not`, etc.).

### Phase 10: Validation (lines 602-1247, SLOW only)

Extensive automated checks. Each check is wrapped in `λ(){}; wrap λ "description"` pattern—if the lambda produces output, it's displayed with a red warning.

**Content validation:**
- LaTeX rendering completeness
- Sitemap file count sanity (>20,000 entries)
- Compiled file count sanity (>115,000 files)
- Link/backlink database integrity

**HTML validation:**
- Unauthorized HTML classes (whitelist at lines 666-724)
- Broken anchors (`anchor-checker.php`)
- Tidy HTML5 validation
- Duplicate footnote IDs

**Markdown validation:**
- YAML metadata (title, description, created, status required)
- Grammar checks (`a` vs `an`)
- Formatting consistency
- Citation style

**Annotation validation (GTX files):**
- Syntax errors
- Punctuation issues
- En-dash/em-dash misuse
- Fraudulent author blacklist (Gino, Ariely, Stapel, etc.)

### Phase 11: Deployment (lines 1249-1283)

```bash
REMOTE="gwern@176.9.41.242:/home/gwern/gwern.net/"
rsync --perms --chmod='a+r' --recursive --checksum ./_site/ "$REMOTE"
```

Three rsync passes:
1. `static/` - Forced checksum sync (infrastructure changes)
2. Pages - Checksum sync of Hakyll output
3. Everything else - Size-only sync (faster), with periodic full checksum

**Cloudflare cache expiration:**
```bash
curl --request POST "https://api.cloudflare.com/client/v4/zones/.../purge_cache" \
    --data "{\"files\":[\"$URL\"]}"
```
Expires up to 100 recently-modified files.

### Phase 12: Post-Deploy Validation (lines 1285-1523, SLOW only)

Live site checks:
- W3C HTML validation (random page)
- W3C link checker
- MIME type verification
- Redirect correctness
- Content spot-checks
- Password protection verification

---

## Key Flags and Modes

| Flag | Effect |
|------|--------|
| `--fast` | Skips validation, string rewrites, metadata checks. ~5-10 min build |
| `--slow` | (default) Full build with all validation. 30-60+ minutes |
| `--skip-directories` | Skips tag directory regeneration |
| `N` (number) | Sets parallelism level (default: 14) |

**Example usage:**
```bash
./sync.sh              # Full slow build
./sync.sh --fast       # Quick build for testing
./sync.sh --fast 8     # Quick build with 8 cores
```

---

## Environment Variables

| Variable | Purpose |
|----------|---------|
| `N` | Parallelism level (default: 14, inherited from bash.sh) |
| `SLOW` | Set to "true" for full build, empty for fast |
| `SKIP_DIRECTORIES` | Set to "true" to skip directory generation |
| `TODAY` | Current date in YYYY-MM-DD format |
| `CLOUDFLARE_CACHE_TOKEN` | API token for cache purging |
| `CLOUDFLARE_TARGET` | Cloudflare zone ID |

---

## Key Helper Functions

### From bash.sh

**`wrap λ "message"`**
Executes lambda, prints red warning if output is non-empty. Core pattern for validation checks.

**`bold "message"`**, **`red "message"`**
Colored terminal output.

**`gf`**, **`ge`**, **`gfv`**, **`gev`**
Grep shortcuts: fixed-strings, extended-regexp, with `-v` variants for invert-match.

**`gwsed OLD NEW`**
Safe site-wide search-and-replace across Markdown and metadata files.

**`path2File`**
Converts URLs/paths to local filesystem paths.

**`everyNDays N`**
Returns true every N days. Used to schedule periodic expensive checks.

### Defined in sync.sh

**`compile`** (line 183)
GHC compilation wrapper with optimization and warnings.

**`syntaxHighlight`** (lines 445-472)
Generates HTML previews of source code files.

**`staticCompileMathJax`** (lines 510-522)
Pre-renders LaTeX to static HTML.

**`convert_to_html`** (lines 374-423)
LibreOffice document conversion for popups.

---

## Common Failure Points

### "Hakyll errored out!" (line 322)
**Cause:** Hakyll compilation failed—usually a Markdown syntax error or template issue.
**Debug:** Check the Hakyll error output for file path and line number. Common issues:
- Unclosed code blocks
- Invalid YAML metadata
- Missing required fields

### Validation lambdas producing output
**Cause:** Content doesn't meet style/quality checks.
**Debug:** Each warning includes the check name. Search for `wrap λ "..."` with that name to find the check logic.

### MathJax compilation failures
**Cause:** Invalid LaTeX syntax or mjpage crash.
**Debug:** Look for "failed MathJax compilation" messages. Check the source file for malformed math.

### rsync failures
**Cause:** Network issues, permission problems, disk full on server.
**Debug:** rsync output shows which files failed. Check server disk space, SSH connectivity.

### "Bad or banned blacklisted domains" (lines 1055-1082)
**Cause:** Links to deprecated/problematic domains.
**Debug:** Update the link to use an archive or remove it. Blacklist includes: ResearchGate, Academia.edu, JSTOR direct links, old Gwern.net domains.

### Tidy validation errors (lines 1197-1231)
**Cause:** Invalid HTML5 in generated pages.
**Debug:** Tidy output shows the specific issue. Often caused by Pandoc edge cases or template bugs.

---

## Integration Points

### External Tools Called

| Tool | Purpose |
|------|---------|
| `ghc`/`ghci`/`runghc` | Haskell compilation and scripting |
| `pandoc` | Markdown → HTML conversion |
| `hakyll` | Static site generator (compiled locally) |
| `ffmpeg` | Video poster extraction |
| `imagemagick` (`convert`, `mogrify`, `identify`) | Image manipulation |
| `libreoffice` | Document → HTML conversion |
| `mjpage` | MathJax pre-rendering |
| `tidy` | HTML validation |
| `rsync` | Deployment |
| `curl` | HTTP requests, cache expiration |
| `parallel` | Parallel execution |
| `exiftool` | PDF metadata |
| `pdftk` | PDF manipulation |
| `linkchecker` | Dead link detection |
| `git` | Version control, infrastructure updates |

### Files Read

- `metadata/*.gtx` - Annotation databases
- `metadata/archive.hs` - Link archive mappings
- `metadata/backlinks.hs` - Backlink database
- All `*.md` files - Source content

### Files Written

- `_site/*` - All compiled output
- `sitemap.xml` - Search engine sitemap
- `metadata/annotation/*.html` - Rendered annotations
- `metadata/thumbnail/256px/*` - Image thumbnails
- Various `*.html` syntax-highlighted versions

### External Services

- **Cloudflare** - Cache purging via API
- **gwern.net server** (176.9.41.242) - Deployment target
- **W3C validators** - HTML/link validation (browser opened)
- **Google PageSpeed** - Performance checks (browser opened)

---

## See Also

- [hakyll.hs](/backend/hakyll-hs) - Core static site generator that sync.sh orchestrates
- [bash.sh](/backend/bash-sh) - Helper function library sourced by sync.sh
- [pre-commit.hook.php](/php/pre-commit-hook) - Asset build pipeline for CSS/JS
- [build_unified_assets.php](/php/build_unified_assets) - Unified CSS/JS concatenation
- [Typography.hs](/backend/typography-hs) - Text transforms applied during build
- [generate-directory.hs](/backend/generate-directory-hs) - Tag directory generation
- [generate-link-bibliography.hs](/backend/generate-link-bibliography-hs) - Link bibliography generation
