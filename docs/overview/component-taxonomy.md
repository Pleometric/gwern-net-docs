---
sidebar_position: 1
---

# Functional Taxonomy

This document organizes every documented file by functional purpose. Each file is assigned to exactly one category by what it does and not implementation language. An importance score is assigned by how critical the function is.

## Importance Scoring

| Score | Meaning | Criteria |
|-------|---------|----------|
| **5** | Critical Path | Site breaks without it; core build/rendering |
| **4** | Major Feature | Key user-facing functionality |
| **3** | Supporting | Enables major features to work |
| **2** | Enhancement | Improves UX/performance, not essential |
| **1** | Peripheral | Utilities, edge cases, rarely used |

---

## Category Overview

| Category | Description | Files | Critical (5) |
|----------|-------------|-------|--------------|
| **[Build Pipeline](#1-build-pipeline)** | Site generation, compilation, deployment | 15 | 3 |
| **[Annotation & Metadata](#2-annotation--metadata)** | Link metadata scraping, storage, display | 33 | 2 |
| **[Popup System](#3-popup-system)** | Hover popups, popins, extract coordination | 12 | 2 |
| **[Link Processing](#4-link-processing)** | Archives, icons, auto-linking, IDs | 22 | 1 |
| **[Content Rendering](#5-content-rendering)** | Transclusion, DOM rewriting, content loading | 10 | 2 |
| **[Typography & Layout](#6-typography--layout)** | Text transforms, sidenotes, columns | 17 | 1 |
| **[Theming & UI](#7-theming--ui)** | Dark mode, reader mode, colors, CSS | 20 | 0 |
| **[Utilities & Infrastructure](#8-utilities--infrastructure)** | Helpers, config, templates, server | 65 | 1 |

**Total: 194 files**

---

## 1. Build Pipeline

Core infrastructure that compiles Markdown into the deployed website. Without these, no site gets built.

| File | Score | Role |
|------|-------|------|
| **[sync-sh](/backend/sync-sh)** | 5 | Master build orchestrator (~1,900 lines), coordinates all phases |
| **[hakyll-hs](/backend/hakyll-hs)** | 5 | Hakyll site generator entry point, Pandoc transform pipeline |
| **[bash-sh](/backend/bash-sh)** | 5 | Bash helper library used by sync.sh |
| [preprocess-markdown-hs](/backend/preprocess-markdown-hs) | 4 | Markdown preprocessing before Hakyll |
| [generate-directory-hs](/backend/generate-directory-hs) | 4 | Generates tag directory index pages |
| [generate-link-bibliography-hs](/backend/generate-link-bibliography-hs) | 4 | Creates per-page link bibliographies |
| [check-metadata-hs](/backend/check-metadata-hs) | 3 | Validates annotation metadata database |
| [test-hs](/backend/test-hs) | 3 | Test suite runner |
| [pre-commit-hook](/php/pre-commit-hook) | 3 | Git hook triggering asset rebuilds |
| [markdown-lint](/shell/markdown-lint) | 2 | Markdown linting checks |
| [markdown-footnote-length-hs](/backend/markdown-footnote-length-hs) | 2 | Footnote length warnings |
| [markdown-length-checker-hs](/backend/markdown-length-checker-hs) | 2 | Code block line length checker |
| [anchor-checker](/php/anchor-checker) | 2 | HTML anchor validation |
| [duplicate-quote-site-finder-hs](/backend/duplicate-quote-site-finder-hs) | 1 | Near-duplicate quote detection |
| [cycle-hs](/backend/cycle-hs) | 2 | Prevents infinite rewrite loops |

---

## 2. Annotation & Metadata

The annotation system provides rich metadata for links (title, author, date, abstract, tags) displayed in popups.

### Core System

| File | Score | Role |
|------|-------|------|
| **[link-metadata-hs](/backend/link-metadata-hs)** | 5 | Central manager: lookup, creation, caching |
| **[annotation-hs](/backend/annotation-hs)** | 5 | Master dispatcher routing URLs to scrapers |
| [link-metadata-types-hs](/backend/link-metadata-types-hs) | 4 | Type definitions (Metadata, MetadataItem) |
| [gtx-hs](/backend/gtx-hs) | 4 | GTX format parser/writer |
| [annotations-js](/frontend/annotations-js) | 4 | Frontend annotation data loading/caching |

### Scrapers

| File | Score | Role |
|------|-------|------|
| [annotation-arxiv-hs](/backend/annotation-arxiv-hs) | 3 | Arxiv API metadata extraction |
| [annotation-biorxiv-hs](/backend/annotation-biorxiv-hs) | 3 | BioRxiv/MedRxiv HTML scraping |
| [annotation-gwernnet-hs](/backend/annotation-gwernnet-hs) | 3 | Local page annotation extraction |
| [annotation-openreview-hs](/backend/annotation-openreview-hs) | 3 | OpenReview.net scraping |
| [annotation-pdf-hs](/backend/annotation-pdf-hs) | 3 | PDF metadata via exiftool |
| [annotation-dump-hs](/backend/annotation-dump-hs) | 2 | Annotation database dump utility |
| [openreviewabstract](/shell/openreviewabstract) | 2 | Shell wrapper for OpenReview |
| [preprocess-annotation](/shell/preprocess-annotation) | 2 | Annotation preprocessing |

### Metadata Processing

| File | Score | Role |
|------|-------|------|
| [metadata-author-hs](/backend/metadata-author-hs) | 3 | Author name canonicalization |
| [metadata-date-hs](/backend/metadata-date-hs) | 3 | Date parsing and validation |
| [metadata-format-hs](/backend/metadata-format-hs) | 3 | Abstract/title HTML cleaning |
| [metadata-title-hs](/backend/metadata-title-hs) | 3 | Title extraction from pages |
| [paragraph-hs](/backend/paragraph-hs) | 3 | GPT-4o paragraph splitting |
| [date-guesser](/python/date-guesser) | 2 | LLM date extraction |
| [title-cleaner](/python/title-cleaner) | 2 | LLM title cleanup |
| [paragraphizer](/python/paragraphizer) | 2 | LLM paragraph splitting |
| [tagguesser](/python/tagguesser) | 2 | LLM tag suggestions |
| [italicizer](/python/italicizer) | 1 | LLM italicization |
| [text2epositive](/python/text2epositive) | 1 | Text sentiment analysis |

### Config

| File | Score | Role |
|------|-------|------|
| [config-metadata-author-hs](/backend/config-metadata-author-hs) | 2 | Author canonicalization rules |
| [config-metadata-format-hs](/backend/config-metadata-format-hs) | 2 | HTML rewrite patterns |
| [config-metadata-title-hs](/backend/config-metadata-title-hs) | 2 | Bad title string filters |

---

## 3. Popup System

Displays preview content when hovering over links—the most distinctive user-facing feature.

### Core Popup/Popin

| File | Score | Role |
|------|-------|------|
| **[popups-js](/frontend/popups-js)** | 5 | Main popup positioning, windowing (~2,700 lines) |
| **[extracts-js](/frontend/extracts-js)** | 5 | Orchestrates content → popup coordination |
| [popins-js](/frontend/popins-js) | 4 | Mobile-friendly popin variant |
| [extracts-annotations-js](/frontend/extracts-annotations-js) | 4 | Annotation-specific extract handling |
| [extracts-content-js](/frontend/extracts-content-js) | 4 | Content extraction for popups |
| [extracts-options-js](/frontend/extracts-options-js) | 3 | User preferences for extracts |
| [extracts-load-js](/frontend/extracts-load-js) | 3 | Extract system bootstrapping |

### Backend Support

| File | Score | Role |
|------|-------|------|
| [link-live-hs](/backend/link-live-hs) | 3 | Determines if URL can be iframed |
| [config-link-live-hs](/backend/config-link-live-hs) | 2 | Domain whitelist/blacklist for live popups |

### Templates

| File | Score | Role |
|------|-------|------|
| [pop-frame-title-standard](/templates/pop-frame-title-standard) | 3 | Standard popup title template |
| [annotation-blockquote-inside](/templates/annotation-blockquote-inside) | 3 | Annotation quote template variant |
| [annotation-blockquote-outside](/templates/annotation-blockquote-outside) | 3 | Annotation quote template variant |

---

## 4. Link Processing

Multiple subsystems for enriching, archiving, and managing links.

### Link Archives (Preemptive link-rot prevention)

| File | Score | Role |
|------|-------|------|
| **[link-archive-hs](/backend/link-archive-hs)** | 5 | Rewrites external URLs to local mirrors |
| [config-link-archive-hs](/backend/config-link-archive-hs) | 3 | Archive whitelist/blacklist (~800 domains) |
| [link-archive](/shell/link-archive) | 3 | SingleFile-based archiving script |
| [deconstruct-singlefile](/php/deconstruct-singlefile) | 2 | Splits monolithic archives |

### Link Icons

| File | Score | Role |
|------|-------|------|
| [link-icon-hs](/backend/link-icon-hs) | 4 | Assigns icon based on URL/domain |
| [config-link-icon-hs](/backend/config-link-icon-hs) | 3 | URL→icon mapping rules |
| [build-icon-sprite-file](/php/build-icon-sprite-file) | 2 | SVG icon sprite generation |

### Auto-Linking & Interwiki

| File | Score | Role |
|------|-------|------|
| [link-auto-hs](/backend/link-auto-hs) | 4 | Auto-hyperlinks ~1000 terms/citations |
| [config-link-auto-hs](/backend/config-link-auto-hs) | 3 | Regex patterns for auto-linking |
| [interwiki-hs](/backend/interwiki-hs) | 3 | !W → Wikipedia expansion |
| [config-interwiki-hs](/backend/config-interwiki-hs) | 2 | Interwiki patterns |

### Link IDs & Bibliography

| File | Score | Role |
|------|-------|------|
| [link-id-hs](/backend/link-id-hs) | 3 | Generates citation-style IDs (foo-2020) |
| [config-link-id-hs](/backend/config-link-id-hs) | 2 | ID override mappings |
| [link-suggester-hs](/backend/link-suggester-hs) | 2 | Generates Emacs link suggestions |
| [link-extractor-hs](/backend/link-extractor-hs) | 2 | Extracts URLs from Markdown |
| [link-prioritize-hs](/backend/link-prioritize-hs) | 2 | Ranks unannotated links |
| [link-titler-hs](/backend/link-titler-hs) | 2 | Adds tooltips to bare links |
| [link-tooltip-hs](/backend/link-tooltip-hs) | 2 | Parses tooltips to metadata |

### Links CSS

| File | Score | Role |
|------|-------|------|
| [links-css](/frontend/links-css) | 4 | Link icons and annotation styling |
| [links](/css/links) | 4 | CSS link styling rules |

---

## 5. Content Rendering

Loads, transforms, and displays content—the backbone of the frontend.

### Core Frontend Framework

| File | Score | Role |
|------|-------|------|
| **[initial-js](/frontend/initial-js)** | 5 | GW namespace, notification center (~1,335 lines) |
| **[rewrite-js](/frontend/rewrite-js)** | 5 | 80+ DOM transformation handlers |
| [content-js](/frontend/content-js) | 4 | Polymorphic content loading |
| [transclude-js](/frontend/transclude-js) | 4 | Include-link resolution |
| [rewrite-initial-js](/frontend/rewrite-initial-js) | 3 | Early DOM rewrites |
| [utility-js](/frontend/utility-js) | 4 | General JavaScript utilities |

### Backend Generators

| File | Score | Role |
|------|-------|------|
| [generate-backlinks-hs](/backend/generate-backlinks-hs) | 3 | Generates reverse citations |
| [generate-similar-hs](/backend/generate-similar-hs) | 3 | RP-tree embedding search |
| [generate-similar-links-hs](/backend/generate-similar-links-hs) | 3 | Generates similar link HTML |
| [link-backlink-hs](/backend/link-backlink-hs) | 3 | Backlinks database I/O |

---

## 6. Typography & Layout

Text transformation, formatting, and page structure.

### Typography Transforms

| File | Score | Role |
|------|-------|------|
| **[typography-hs](/backend/typography-hs)** | 5 | Title case, wbr, rulers, citations |
| [config-typography-hs](/backend/config-typography-hs) | 3 | Typography settings |
| [typography-js](/frontend/typography-js) | 3 | Client-side typography enhancements |

### Layout Components

| File | Score | Role |
|------|-------|------|
| [sidenotes-js](/frontend/sidenotes-js) | 4 | Sidenote positioning algorithm |
| [collapse-js](/frontend/collapse-js) | 3 | Collapsible section handling |
| [layout-js](/frontend/layout-js) | 3 | Block layout primitives |
| [columns-hs](/backend/columns-hs) | 2 | Multi-column list detection |

### Inflation Adjustment

| File | Score | Role |
|------|-------|------|
| [inflation-hs](/backend/inflation-hs) | 3 | Dollar/Bitcoin inflation adjustment |
| [config-inflation-hs](/backend/config-inflation-hs) | 2 | CPI/PCE/Bitcoin rate data |

### Image Processing

| File | Score | Role |
|------|-------|------|
| [image-hs](/backend/image-hs) | 3 | Dimensions, inversion detection, lazy loading |
| [image-focus-js](/frontend/image-focus-js) | 3 | Lightbox/image viewer |
| [invertornot](/python/invertornot) | 2 | GPT-4V inversion classifier |
| [image-margin-checker](/python/image-margin-checker) | 1 | Image margin analysis |
| [png](/shell/png) | 2 | PNG optimization |
| [should-image-have-outline](/php/should-image-have-outline) | 2 | Corner analysis for outline CSS |
| [build-inlined-images](/php/build-inlined-images) | 2 | Base64 inline images |

---

## 7. Theming & UI

Visual customization, styling, and user interface.

### Dark Mode

| File | Score | Role |
|------|-------|------|
| [dark-mode-js](/frontend/dark-mode-js) | 4 | Dark mode toggle and persistence |
| [dark-mode-initial-js](/frontend/dark-mode-initial-js) | 4 | Early dark mode (FOUC prevention) |
| [dark-mode-adjustments](/css/dark-mode-adjustments) | 3 | Dark mode image filters |
| [dark-mode-adjustments-css](/frontend/dark-mode-adjustments-css) | 3 | Frontend dark mode CSS |
| [build-mode-css](/php/build-mode-css) | 2 | Generates dark mode CSS |

### Reader Mode

| File | Score | Role |
|------|-------|------|
| [reader-mode-js](/frontend/reader-mode-js) | 3 | Reader mode functionality |
| [reader-mode-initial-js](/frontend/reader-mode-initial-js) | 3 | Early reader mode setup |
| [reader-mode-css](/frontend/reader-mode-css) | 3 | Reader mode CSS |
| [reader-mode-initial](/css/reader-mode-initial) | 3 | Initial reader mode CSS |

### Colors & Styling

| File | Score | Role |
|------|-------|------|
| [color-js](/frontend/color-js) | 3 | Color manipulation utilities |
| [colors](/css/colors) | 4 | Light mode color variables |
| [colors-css](/frontend/colors-css) | 4 | Frontend color definitions |
| [color-scheme-convert](/php/color-scheme-convert) | 2 | Color scheme conversion |

### Core CSS

| File | Score | Role |
|------|-------|------|
| [default](/css/default) | 4 | Main site stylesheet |
| [default-css](/frontend/default-css) | 4 | Frontend default CSS |
| [initial](/css/initial) | 4 | Initial/critical path CSS |
| [initial-css](/frontend/initial-css) | 4 | Frontend initial CSS |
| [light-mode-adjustments](/css/light-mode-adjustments) | 2 | Light mode specific adjustments |

### Special Features

| File | Score | Role |
|------|-------|------|
| [special-occasions-js](/frontend/special-occasions-js) | 2 | Holiday themes (Halloween, Christmas) |
| [special-occasions-css](/frontend/special-occasions-css) | 2 | Holiday CSS |

---

## 8. Utilities & Infrastructure

Supporting code, configuration, templates, and server setup.

### Backend Utilities

| File | Score | Role |
|------|-------|------|
| **[utils-hs](/backend/utils-hs)** | 5 | ~150 utility functions |
| [query-hs](/backend/query-hs) | 3 | Pandoc AST queries |
| [unique-hs](/backend/unique-hs) | 2 | Duplicate detection |
| [string-replace-hs](/backend/string-replace-hs) | 2 | Parallel string replacement |
| [rename-hs](/backend/rename-hs) | 2 | Page rename script generator |

### Frontend Utilities

| File | Score | Role |
|------|-------|------|
| [misc-js](/frontend/misc-js) | 2 | Miscellaneous features |
| [console-js](/frontend/console-js) | 2 | Console utilities |
| [404-guesser-js](/frontend/404-guesser-js) | 2 | 404 page redirect suggestions |

### Tags & Navigation

| File | Score | Role |
|------|-------|------|
| [tags-hs](/backend/tags-hs) | 4 | Tag management, directory listing |
| [config-tags-hs](/backend/config-tags-hs) | 3 | Tag aliases, hierarchy |
| [guess-tag-hs](/backend/guess-tag-hs) | 2 | Tag suggestion from partial input |
| [change-tag-hs](/backend/change-tag-hs) | 2 | Batch tag operations |

### Content Features

| File | Score | Role |
|------|-------|------|
| [blog-hs](/backend/blog-hs) | 3 | Blog entry generation |
| [x-of-the-day-hs](/backend/x-of-the-day-hs) | 2 | Quote/site/annotation of the day |
| [config-x-of-the-day-hs](/backend/config-x-of-the-day-hs) | 2 | XOTD database paths |

### PHP Asset Pipeline

| File | Score | Role |
|------|-------|------|
| [build_unified_assets](/php/build_unified_assets) | 4 | CSS/JS concatenation |
| [build_asset_versions](/php/build_asset_versions) | 3 | Asset version manifest |
| [build_font_css](/php/build-font-css) | 3 | Font CSS generation |
| [build_versioned_font_css](/php/build-versioned-font-css) | 3 | Versioned font CSS |
| [build_head_includes](/php/build_head_includes) | 3 | Head HTML includes |
| [build_body_includes](/php/build_body_includes) | 3 | Body HTML includes |
| [build_standalone_includes](/php/build_standalone_includes) | 3 | Standalone page includes |
| [build_paths](/php/build_paths) | 2 | Build path constants |
| [build_variables](/php/build_variables) | 2 | Build variables |
| [build_functions](/php/build_functions) | 2 | Shared build utilities |
| [version_asset_links](/php/version_asset_links) | 2 | Cache busting |
| [asset](/php/asset) | 2 | Asset management |

### SVG/Font Processing

| File | Score | Role |
|------|-------|------|
| [svg-squeeze](/php/svg-squeeze) | 2 | SVG optimization |
| [svg-strip-background](/php/svg-strip-background) | 1 | SVG background removal |
| [font-spec](/php/font-spec) | 2 | Font specification |

### Python Utilities

| File | Score | Role |
|------|-------|------|
| [clean-pdf](/python/clean-pdf) | 2 | GPT-4 OCR/formatting cleanup |
| [daterange-checker](/python/daterange-checker) | 1 | Date range validation |
| [collapse-checker](/python/collapse-checker) | 1 | Collapse validation |
| [htmlAttributesExtract](/python/htmlAttributesExtract) | 1 | HTML attribute extraction |
| [latex2unicode](/python/latex2unicode) | 2 | LaTeX to Unicode conversion |

### Shell Utilities

| File | Score | Role |
|------|-------|------|
| [embed](/shell/embed) | 3 | OpenAI embeddings API wrapper |
| [upload](/shell/upload) | 2 | File upload with processing |
| [download-title](/shell/download-title) | 2 | Download and extract title |
| [gwsed](/shell/gwsed) | 2 | Site-wide string replacement |

### Configuration Modules

| File | Score | Role |
|------|-------|------|
| [config-misc-hs](/backend/config-misc-hs) | 3 | Global settings |
| [config-paragraph-hs](/backend/config-paragraph-hs) | 2 | Paragraph settings |
| [config-link-suggester-hs](/backend/config-link-suggester-hs) | 2 | Link suggester config |
| [config-generate-similar-hs](/backend/config-generate-similar-hs) | 2 | Embedding settings |
| [nginx-redirect-guesser-hs](/backend/nginx-redirect-guesser-hs) | 1 | 404 redirect suggestions |

### HTML Templates

| File | Score | Role |
|------|-------|------|
| [default](/templates/default) | 4 | Main HTML template |
| [sourcecode](/templates/sourcecode) | 3 | Source code display template |
| [annotation-blockquote-not](/templates/annotation-blockquote-not) | 2 | Annotation without blockquote |
| [annotation-partial-inline](/templates/annotation-partial-inline) | 2 | Partial annotation template |
| [github-issue-blockquote-not](/templates/github-issue-blockquote-not) | 2 | GitHub issue template |
| [github-issue-blockquote-outside](/templates/github-issue-blockquote-outside) | 2 | GitHub issue template variant |
| [google-cse](/templates/google-cse) | 2 | Google custom search template |
| [google-search](/templates/google-search) | 2 | Google search template |
| [tweet-blockquote-not](/templates/tweet-blockquote-not) | 2 | Tweet template |
| [tweet-blockquote-outside](/templates/tweet-blockquote-outside) | 2 | Tweet template variant |
| [wikipedia-entry-blockquote-inside](/templates/wikipedia-entry-blockquote-inside) | 2 | Wikipedia template |
| [wikipedia-entry-blockquote-not](/templates/wikipedia-entry-blockquote-not) | 2 | Wikipedia template variant |
| [wikipedia-entry-blockquote-title-not](/templates/wikipedia-entry-blockquote-title-not) | 2 | Wikipedia template variant |

### Include Templates

| File | Score | Role |
|------|-------|------|
| [include-footer](/templates/include-footer) | 3 | Footer include |
| [include-sidebar](/templates/include-sidebar) | 3 | Sidebar include |
| [include-inlined-asset-links](/templates/include-inlined-asset-links) | 2 | Asset link include |
| [include-inlined-head](/templates/include-inlined-head) | 2 | Head include |
| [include-inlined-standalone](/templates/include-inlined-standalone) | 2 | Standalone include |
| [template-html5-articleedit](/templates/template-html5-articleedit) | 2 | Article edit template |

### Server & Nginx

| File | Score | Role |
|------|-------|------|
| [gwern-net-conf](/nginx/gwern-net-conf) | 4 | Main nginx configuration |
| [redirect-nginx](/nginx/redirect-nginx) | 3 | Nginx redirect rules |
| [redirect-nginx-broken](/nginx/redirect-nginx-broken) | 2 | Broken redirect handling |
| [memoriam-sh](/nginx/memoriam-sh) | 1 | Memorial system |
| [rsyncd-conf](/nginx/rsyncd-conf) | 2 | rsync daemon config |
| [twdne-conf](/nginx/twdne-conf) | 1 | TWDNE subdomain config |

### Misc Templates

| File | Score | Role |
|------|-------|------|
| [idealconditionsdonotexistandwillneverhappen](/templates/idealconditionsdonotexistandwillneverhappen) | 1 | Error template |
| [unfortunatelytheclockisticking](/templates/unfortunatelytheclockisticking) | 1 | Error template |

---

## Data Flow Summary

<div style={{textAlign: 'center'}}>

```
┌───────────────────────────────────────────────────────────┐
│                  BUILD TIME (sync.sh)                     │
├───────────────────────────────────────────────────────────┤
│                                                           │
│  Markdown → Pandoc AST → Haskell Transforms → HTML + JSON │
│                                                           │
│  Key transforms:                                          │
│  • Typography.hs: text polish                             │
│  • LinkAuto.hs: auto-linking                              │
│  • LinkMetadata.hs: annotation marking                    │
│  • LinkArchive.hs: archive localization                   │
│  • LinkIcon.hs: icon assignment                           │
│                                                           │
│  Asset generation:                                        │
│  • PHP scripts: CSS/JS bundling                           │
│  • Annotation fragments: HTML for popups                  │
│  • Embeddings: similar link computation                   │
│                                                           │
└───────────────────────────────────────────────────────────┘
  ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓
┌───────────────────────────────────────────────────────────┐
│                   RUNTIME (Browser)                       │
├───────────────────────────────────────────────────────────┤
│                                                           │
│  initial.js → GW.notificationCenter (pub/sub event bus)   │
│                                                           │
│  Content events: GW.contentDidLoad → GW.contentDidInject  │
│                                                           │
│  Phase order: transclude → rewrite → eventListeners       │
│                                                           │
│  Key modules:                                             │
│  • extracts.js: popup/popin coordination                  │
│  • popups.js: windowing system                            │
│  • transclude.js: include-link resolution                 │
│  • rewrite.js: DOM transformations                        │
│                                                           │
└───────────────────────────────────────────────────────────┘
```

</div>

---

## Critical Path Files (Score 5)

These 12 files are essential—the site won't build or function without them:

| Category | File | Why Critical |
|----------|------|--------------|
| Build | sync-sh | Master orchestrator |
| Build | hakyll-hs | Site generator |
| Build | bash-sh | Build helpers |
| Annotation | link-metadata-hs | Annotation system core |
| Annotation | annotation-hs | Scraper dispatcher |
| Popup | popups-js | Popup display system |
| Popup | extracts-js | Content coordination |
| Content | initial-js | Event system foundation |
| Content | rewrite-js | DOM transformation |
| Typography | typography-hs | Text transforms |
| Link | link-archive-hs | Archive system |
| Utilities | utils-hs | Shared helpers |

---

## See Also

- [Page Lifecycle](/overview/page-lifecycle) - How a page transforms from Markdown to final HTML
- [Documentation Home](/) - Main index
- [sync.sh](/backend/sync-sh) - Master build orchestrator coordinating all phases
- [hakyll.hs](/backend/hakyll-hs) - Hakyll site generator and Pandoc pipeline
- [LinkMetadata.hs](/backend/link-metadata-hs) - Central annotation database manager
- [popups.js](/frontend/popups-js) - Main popup windowing system
- [extracts.js](/frontend/extracts-js) - Content extraction for popups
- [Typography.hs](/backend/typography-hs) - Text transforms: title case, citations, rulers
