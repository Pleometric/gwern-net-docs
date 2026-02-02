---
sidebar_position: 0
---

# Page Lifecycle

How a gwern.net page transforms from Markdown source to rendered HTML in the browser—and why each step exists.


## The Big Picture

A single essay goes through **10 distinct phases** before a reader sees it. Each phase exists because gwern.net optimizes for *long-term reading*, not authoring convenience. The complexity pays off in reader experience: hover any link to see a rich preview, every dollar amount adjusts for inflation, dead links automatically fall back to archived copies.

| Phase | What happens |
|-------|--------------|
| 1. Source Files | Markdown + GTX annotation databases |
| 2. Pre-processing | URL normalization, string fixes |
| 3. Content Generation | Backlinks, similar links, directories |
| 4. Pandoc Parse | Markdown → Pandoc AST |
| 5. AST Transforms | 14 AST enrichments (the core) |
| 6. Render to HTML | AST → HTML + templates |
| 7. Post-processing | MathJax, syntax highlighting |
| 8. Validation | 50+ automated checks |
| 9. Deployment | rsync + cache purge |
| 10. Browser Runtime | JS event system, popups |

Let's walk through what happens at each phase.


## 1. Source Files

**Input:** Markdown files (`.md`) + GTX annotation databases

**Where:** `*.md` files in repo root, `metadata/*.gtx` files

Gwern writes essays in Markdown with YAML frontmatter:

```markdown
---
title: "The Scaling Hypothesis"
created: 2020-04-15
modified: 2024-01-10
status: finished
confidence: likely
tags: [AI, scaling]
---

Deep learning's surprising effectiveness may be explained by...
```

Annotations live separately in GTX files (a custom tab-separated format):

```
https://arxiv.org/abs/2001.08361	Scaling Laws for Neural Language Models	Kaplan et al 2020	2020-01-23	We study empirical scaling laws...	AI/scaling
```

**Why separate files?** Annotations are reused across hundreds of essays. Storing them centrally enables:
- Single source of truth (fix one annotation, fix everywhere)
- Efficient lookup (20,000+ annotations in memory during build)
- Priority layering (`full.gtx` hand-written overrides `auto.gtx` machine-generated)

**Deeper dive:** [GTX.hs](/backend/gtx-hs), [LinkMetadata.hs](/backend/link-metadata-hs)


## 2. Pre-processing

**Input:** Raw Markdown files

**Output:** Cleaned Markdown files

**Where:** [sync.sh](/backend/sync-sh) lines 100-177

Before compilation, sync.sh runs ~50 search-and-replace operations across all Markdown:

| Category | Example | Why |
|----------|---------|-----|
| URL normalization | `twitter.com/` → `x.com/` | Canonical URLs for deduplication |
| Tracking removal | `?utm_source=...` → (removed) | Clean links |
| Name fixes | `Yann Le Cun` → `Yann LeCun` | Consistent author names |
| Citation style | `et al.` → `et al` | No trailing period before year |
| Protocol upgrade | `http://arxiv.org` → `https://arxiv.org` | Security |

**Before:**
```markdown
See [Scaling Laws](http://arxiv.org/abs/2001.08361?utm_source=twitter)
by Kaplan et al. (2020).
```

**After:**
```markdown
See [Scaling Laws](https://arxiv.org/abs/2001.08361)
by Kaplan et al 2020.
```

**Why automated?** These are corrections Gwern would make manually anyway. Automating them means the source stays clean without constant vigilance.

**Deeper dive:** [bash.sh](/backend/bash-sh) (`gwsed` function)


## 3. Content Generation

**Input:** Cleaned Markdown + metadata databases

**Output:** Generated Markdown files, annotation HTML fragments

**Where:** [sync.sh](/backend/sync-sh) lines 245-320

Several types of content must be generated before Hakyll runs:

| Generated | Source | Why |
|-----------|--------|-----|
| Backlinks | Scan all pages for incoming links | "What links here" sections |
| Similar links | OpenAI embeddings + RP-tree search | Related content discovery |
| Tag directories | Tag metadata from frontmatter | Browsable topic indexes |
| Link bibliographies | Per-page link collection | Reference lists |
| Annotation HTML | GTX → rendered blockquotes | Pre-rendered for popup speed |

**Before (no backlinks section):**
```markdown
---
title: GPT-3
---

GPT-3 is a large language model...
```

**After (backlinks generated):**
```markdown
<!-- metadata/backlinks/gpt-3.html contains -->
<section id="backlinks">
  <h2>Backlinks</h2>
  <ul>
    <li><a href="/scaling">The Scaling Hypothesis</a></li>
    <li><a href="/llm-economics">LLM Economics</a></li>
  </ul>
</section>
```

**Why pre-generate?** Computing backlinks requires reading *all* pages. Doing this inside Hakyll would be slow and complex. Generating once and including is cleaner.

**Deeper dive:** [generate-backlinks.hs](/backend/generate-backlinks-hs), [generate-similar.hs](/backend/generate-similar-hs), [generate-directory.hs](/backend/generate-directory-hs)


## 4. Pandoc Parse

**Input:** Pre-processed Markdown

**Output:** Pandoc AST (Abstract Syntax Tree)

**Where:** [hakyll.hs](/backend/hakyll-hs) via `pandocCompilerWithTransformM`

Pandoc parses Markdown into a structured tree representation:

**Markdown:**
```markdown
# Introduction

See [GPT-3](https://arxiv.org/abs/2005.14165) for details.
```

**AST (simplified):**
```haskell
Pandoc Meta
  [ Header 1 ("introduction", [], []) [Str "Introduction"]
  , Para [ Str "See "
         , Link ("", [], []) [Str "GPT-3"]
                ("https://arxiv.org/abs/2005.14165", "")
         , Str " for details."
         ]
  ]
```

**Why an AST?** Direct text manipulation is fragile—regex can't reliably distinguish code blocks from prose. The AST makes transformations safe: you can walk the tree, match specific node types, and transform without breaking structure.

**Deeper dive:** [Query.hs](/backend/query-hs), [Utils.hs](/backend/utils-hs)


## 5. AST Transforms

**Input:** Pandoc AST

**Output:** Enriched Pandoc AST

**Where:** [hakyll.hs](/backend/hakyll-hs) `pandocTransform` function

This is where gwern.net's magic happens. The AST passes through **14 transforms** in sequence:

#### a. Interwiki Links

Expands shorthand wiki links to full URLs.

**Before:** `[!W:Scaling hypothesis](!W)`

**After:** `[Scaling hypothesis](https://en.wikipedia.org/wiki/Scaling_hypothesis)`

**Why?** Faster to write, cleaner source. [Interwiki.hs](/backend/interwiki-hs)

#### b. Auto-linking

Converts ~1,000 citation patterns into hyperlinks automatically.

**Before:** `Kaplan et al 2020 found that...`

**After:** `[Kaplan et al 2020](https://arxiv.org/abs/2001.08361) found that...`

**Why?** Academic citations are tedious to link manually. The system knows major papers. [LinkAuto.hs](/backend/link-auto-hs)

#### c. Annotation Creation

Triggers scraping for any URL lacking metadata. If you link to an arXiv paper, the system fetches title/authors/abstract.

**Before:** Link exists, no annotation in database

**After:** GTX entry created via scraper (arXiv API, PDF metadata, HTML parsing)

**Why?** Rich hover previews need metadata. Getting it automatically means every link can have a preview. [Annotation.hs](/backend/annotation-hs)

#### d. Annotation Marking

Adds `.link-annotated` class to links that have annotations in the database.

**Before:** `<a href="https://arxiv.org/abs/2001.08361">paper</a>`

**After:** `<a href="https://arxiv.org/abs/2001.08361" class="link-annotated">paper</a>`

**Why?** The browser needs to know which links have popups available. [LinkMetadata.hs](/backend/link-metadata-hs)

#### e. File Sizes

Adds file size metadata to PDF/video links.

**Before:** `<a href="/doc/ai/scaling.pdf">PDF</a>`

**After:** `<a href="/doc/ai/scaling.pdf" data-file-size="2.1MB">PDF</a>`

**Why?** Readers should know download sizes before clicking. [LinkMetadata.hs](/backend/link-metadata-hs)

#### f. Inflation Adjustment

Converts nominal dollar amounts to current dollars with subscript showing original.

**Before:** `$100 in 1990`

**After:** `$240<sub>$100 in 1990</sub>`

**Why?** Historical dollar amounts are misleading without inflation context. [Inflation.hs](/backend/inflation-hs)

#### g. Link Archiving

Rewrites external URLs to local archived copies when available.

**Before:** `<a href="https://example.com/article">article</a>`

**After:** `<a href="/doc/www/example.com/article.html" data-url-original="https://example.com/article">article</a>`

**Why?** Links rot. Gwern archives important pages locally. If the original dies, the archived copy serves automatically. [LinkArchive.hs](/backend/link-archive-hs)

#### h. Link Icons

Assigns icons based on URL patterns (PDF icon, Wikipedia icon, etc.).

**Before:** `<a href="https://arxiv.org/abs/2001.08361">paper</a>`

**After:** `<a href="..." class="link-icon-arxiv" data-link-icon-type="svg">paper</a>`

**Why?** Visual cues help readers anticipate link destinations. [LinkIcon.hs](/backend/link-icon-hs)

#### i. Typography

Applies text polish: citation formatting, date subscripts, ruler cycling, title case.

**Before:** `Kaplan et al 2020`

**After:** `<span class="cite"><span class="cite-author">Kaplan et al</span> <span class="cite-date">2020</span></span>`

**Why?** Consistent typography improves readability. Semantic spans enable hover effects. [Typography.hs](/backend/typography-hs)

#### j. Image Dimensions

Adds width/height attributes to images to prevent layout shift.

**Before:** `<img src="/image/gpt3.png">`

**After:** `<img src="/image/gpt3.png" width="800" height="600">`

**Why?** Without dimensions, the page jumps as images load. [Image.hs](/backend/image-hs)

#### k. Cleanup

Final passes that normalize the document:

- **Header self-links:** Makes every `<h2>`/`<h3>` clickable to copy its anchor URL
- **Paragraph wrapping:** Converts stray `Plain` blocks to `Para` for consistent `<p>` tags
- **Prefetch hints:** Marks links eligible for instant.page prefetching
- **Class sanitization:** Strips build-time-only classes like `archive-not`, `link-annotated-not`


## 6. Render to HTML

**Input:** Transformed AST

**Output:** HTML string

**Where:** [hakyll.hs](/backend/hakyll-hs) via Pandoc writer + templates

Pandoc renders the enriched AST to HTML, then Hakyll wraps it in templates:

```html
<!DOCTYPE html>
<html>
<head>
  <title>$title$</title>
  <meta name="description" content="$description$">
</head>
<body class="page-$safe-url$">
  <article>
    <header>
      <h1>$title$</h1>
      <div class="page-metadata">$created$ · $status$</div>
    </header>
    <div id="markdownBody">
      $body$
    </div>
    $if(backlinks-yes)$
    <section id="backlinks">...</section>
    $endif$
  </article>
</body>
</html>
```

**Why templates?** Separation of content and presentation. Update the template once, all pages change. [Template: default](/templates/default)


## 7. Post-processing

**Input:** Raw HTML from Hakyll

**Output:** Production-ready HTML

**Where:** [sync.sh](/backend/sync-sh) lines 340-600

Several transforms happen *after* Hakyll:

| Transform | What | Why |
|-----------|------|-----|
| MathJax | LaTeX → CSS+HTML | Pre-render math server-side for fast load |
| Syntax highlighting | Code → colored spans | Consistent highlighting across languages |
| Class cleanup | Remove build-only classes | Smaller HTML, cleaner DOM |
| Document conversion | .docx → .html | Popup previews for Office files |

**Before (MathJax):**
```html
<span class="math">E = mc^2</span>
```

**After (pre-rendered):**
```html
<span class="mjx-math"><span class="mjx-mi">E</span><span class="mjx-mo">=</span>...</span>
```

**Why server-side?** MathJax in the browser causes visible rendering delay. Pre-rendering eliminates it.


## 8. Validation

**Input:** Complete site in `_site/`

**Output:** Warnings, errors, or clean pass

**Where:** [sync.sh](/backend/sync-sh) lines 602-1247

Over 50 automated checks run in `--slow` mode:

| Check | What | Why |
|-------|------|-----|
| HTML validity | Tidy HTML5 validation | Catch malformed markup |
| Anchor integrity | All `#id` links resolve | No broken in-page links |
| YAML completeness | Required fields present | Consistent metadata |
| Grammar | "a" vs "an" before vowels | Style consistency |
| Banned domains | No links to problematic sites | Quality control |
| Duplicate footnotes | No repeated `#fn1` IDs | Valid HTML |

**Why so many checks?** With 1,000+ pages, manual review is impossible. Automated checks catch regressions immediately.

**Deeper dive:** [check-metadata.hs](/backend/check-metadata-hs), [anchor-checker.php](/php/anchor-checker)


## 9. Deployment

**Input:** Validated `_site/`

**Output:** Live site at gwern.net

**Where:** [sync.sh](/backend/sync-sh) lines 1249-1283

Three rsync passes deploy the site:

1. **Infrastructure** (`static/`): Force checksum sync for CSS/JS
2. **Pages**: Checksum sync of HTML
3. **Assets**: Size-only sync for large files

Then Cloudflare cache purge for recently-changed URLs.

**Why multiple passes?** Different content types have different invalidation needs. Static assets change rarely but must sync perfectly. Pages change often but only need checksum comparison.


## 10. Browser Runtime

**Input:** Static HTML + JS

**Output:** Interactive reading experience

**Where:** [initial.js](/frontend/initial-js), [rewrite.js](/frontend/rewrite-js), [popups.js](/frontend/popups-js)

When the page loads, JavaScript enhances it further:

### The Event System

```javascript
DOMContentLoaded
    ↓
GW.contentDidLoad (transclude phase)
    ↓
  Transclude handlers resolve include-links
    ↓
GW.contentDidLoad (rewrite phase)
    ↓
  Rewrite handlers transform DOM
    ↓
GW.contentDidInject (eventListeners phase)
    ↓
  Event binding (hover, click handlers)
```

### Runtime Transforms

| Handler | What | Why |
|---------|------|-----|
| `wrapImages` | Wrap `<img>` in `<figure>` | Consistent figure structure |
| `makeTablesSortable` | Enable column sorting | Interactive data exploration |
| `hyphenate` | Add soft hyphens | Better text flow |
| Link icon management | Enable/disable icons | Context-dependent display |

### Popup System

When you hover an annotated link:

1. `extracts.js` detects the link type (annotation, local page, etc.)
2. After 750ms delay, `popups.js` spawns a popup frame
3. `content.js` fetches annotation HTML from `/metadata/annotation/`
4. `transclude.js` resolves any include-links in the annotation
5. `rewrite.js` handlers transform the popup content
6. Popup displays with smart positioning

**Why client-side enhancement?** Some transforms depend on viewport size, user preferences, or dynamic state that can't be known at build time.

**Deeper dive:** [extracts.js](/frontend/extracts-js), [popups.js](/frontend/popups-js), [transclude.js](/frontend/transclude-js)

## A Link's Complete Journey

Let's trace what happens when you write this in Markdown:

```markdown
Kaplan et al 2020 showed that loss scales as a power law.
```

### 2. Pre-processing
`et al.` → `et al` (if period present)

### 5b. Auto-linking
Pattern `Kaplan et al 2020` matches database → wrapped in link:
```markdown
[Kaplan et al 2020](https://arxiv.org/abs/2001.08361) showed...
```

### 5c. Annotation Creation
arXiv URL checked against GTX database → annotation exists (or created via API)

### 5d. Annotation Marking
Link gets class:
```html
<a href="https://arxiv.org/abs/2001.08361" class="link-annotated">Kaplan et al 2020</a>
```

### 5h. Link Icons
arXiv domain → icon class:
```html
<a href="..." class="link-annotated link-icon-arxiv">Kaplan et al 2020</a>
```

### 5i. Typography
Citation text wrapped in semantic spans:
```html
<a href="..." class="link-annotated link-icon-arxiv">
  <span class="cite">
    <span class="cite-author">Kaplan et al</span>
    <span class="cite-date">2020</span>
  </span>
</a>
```

### 6. Render to HTML
Wrapped in page template with all other content.

### 10. Browser Runtime
On hover after 750ms:
1. Popup spawns
2. Annotation fetched: title, authors, abstract, tags
3. Displayed in positioned popup with typography transforms applied

**Final reader experience:** Hover "Kaplan et al 2020" → see paper title, authors, publication date, abstract, and tags—all from a plain text citation in the source.

## Why This Complexity?

Every phase serves gwern.net's core goal: **optimize for readers, not writers**.

| Complexity | Reader Benefit |
|------------|----------------|
| 14 AST transforms | Rich, consistent formatting without manual markup |
| Annotation system | Instant previews for 20,000+ links |
| Link archiving | Content survives link rot |
| Pre-rendered MathJax | No rendering flicker |
| Inflation adjustment | Historical context without manual calculation |
| Backlinks | Discover related content |
| Validation suite | No broken links or malformed pages |

The build takes 30-60 minutes. But readers spend far more cumulative time on the site. Investing authoring complexity for reading quality is the right tradeoff for long-form content meant to be read for decades.

## See Also

- [Component Taxonomy](/overview/component-taxonomy) — Complete file listing by function
- [sync.sh](/backend/sync-sh) — Master build orchestrator
- [hakyll.hs](/backend/hakyll-hs) — Pandoc pipeline and template system
- [LinkMetadata.hs](/backend/link-metadata-hs) — Annotation database manager
- [Typography.hs](/backend/typography-hs) — Text transform pipeline
- [rewrite.js](/frontend/rewrite-js) — Browser DOM transforms
- [popups.js](/frontend/popups-js) — Popup windowing system
