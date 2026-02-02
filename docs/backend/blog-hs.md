
# Blog.hs

**Path:** `build/Blog.hs` | **Language:** Haskell | **Lines:** ~273

> Generates standalone blog pages from authored annotations and directory indexes

---

## Overview

Blog.hs addresses a design problem in gwern.net: how to publish "small" or "one-off" writings without the overhead of creating full top-level essays. The traditional essay format requires extensive metadata, formatting, a stable URL, and ongoing maintenance—but many popular writings originated as quick comments on LessWrong, Hacker News, or Reddit.

The solution leverages the annotation system. Off-site writings get saved as annotations with a custom ID (e.g., `gwern-2025-drl-scaling`), and this module automatically generates a standalone page at `/blog/2025/drl-scaling` that transcludes that annotation. This gives blog posts the benefits of both systems: lightweight authoring via annotations, but with stable URLs, discoverability through `/blog/index`, and the ability to be promoted to full essays later.

The module generates three outputs: individual blog post pages (`/blog/YYYY/slug.md`), a full directory index (`/blog/index.md`), and a simplified recent-posts list (`/blog/newest.md`) intended for homepage transclusion.

---

## Public API

### `writeOutBlogEntries :: Metadata -> IO ()`

Main entry point. Filters the annotation database for authored writings, validates them, generates individual blog post files, and creates the index pages.

**Called by:** `hakyll.hs` (line 79, during site build initialization)
**Calls:** `filterForAuthoredAnnotations`, `writeOutBlogEntry`, `generateDirectoryBlog`, `generateDirectoryBlogSimplified`, `metadataItem2ID` (from LinkID)

---

## Internal Architecture

### Data Flow

```
Metadata (annotation DB)
    │
    ▼
filterForAuthoredAnnotations  ─► Filter by author="Gwern" + length>600
    │
    ▼
Validation Pipeline:
  - Check for overly long posts (>30k chars) → warning
  - Validate dates (must be 10 chars: YYYY-MM-DD)
  - Validate titles (max 51 chars for mobile)
  - Validate IDs (checkIdent)
    │
    ▼
Path Generation:
  ID "gwern-2025-foo" → "blog/2025/foo.md"
    │
    ▼
┌─────────────────────────────────────────┐
│ For each entry:                         │
│   writeOutBlogEntry → blog/YYYY/slug.md │
│                                         │
│ generateDirectoryBlog → blog/index.md   │
│ generateDirectoryBlogSimplified →       │
│                       blog/newest.md    │
└─────────────────────────────────────────┘
```

### Key Functions

**`filterForAuthoredAnnotations :: Metadata -> MetadataList`**

Filters annotations to find blog candidates:
- URL must not be local OR must already be under `/blog/`
- Author must start with "Gwern Branwen"
- Abstract must exceed `lengthMin` (600 characters)

**`checkIdent :: String -> Bool`**

Validates ID format. Expects `gwern-YYYY/slug` or `gwern-YYYY-slug`:
- Minimum length: 13 characters (`gwern-YYYY?s`)
- Maximum length: 47 characters
- Year must be 2009 through current year
- Slug: alphanumeric and hyphens only

**`annotation2Markdown :: Path -> MetadataItem -> String`**

Generates the Markdown content for a blog post page. Creates YAML frontmatter from annotation metadata, then a single transclusion directive:

```markdown
---
title: 'Title Here'
author: Gwern Branwen
description: "..."
created: YYYY-MM-DD
modified: YYYY-MM-DD
status: finished
...

[Description](https://original-url.com){.include-annotation ...}

<div class='text-center' id='return-to-blog-index-link'>...</div>
```

Key metadata fields extracted from annotation key-value pairs:
- `description`, `status`, `importance`, `confidence`
- `css-extension` (defaults to `dropcaps-de-zs toc-not`)
- `thumbnail`, `thumbnail-text`

**`generateDirectoryBlog :: [(FilePath, Path, MetadataItem)] -> IO ()`**

Creates `/blog/index.md` with:
1. **Link sections by year** - Bulleted lists of blog post links grouped by year
2. **Transclude sections** - Full annotation transclusions for reading without clicking

Uses Pandoc AST to generate Markdown. Current/last year sections get special IDs (`year-current`, `year-last`) for CSS styling.

**`generateDirectoryBlogSimplified :: [(FilePath, Path, MetadataItem)] -> IO ()`**

Creates `/blog/newest.md` with the 29 most recent posts in a two-column bulleted list. Intended for transclusion onto the homepage.

---

## Key Patterns

### Recursive Blog Posts

A clever design feature: since the system generates blog pages from annotations, you can create a "recursive" blog post by writing an annotation for a `/blog/YYYY/foo` URL. The annotation creates the page, which transcludes itself. This allows true standalone blog content rather than requiring an external source URL.

### ID Format Flexibility

The system accepts two ID formats and normalizes them:
- `gwern-2025/foo` (slash separator)
- `gwern-2025-foo` (hyphen separator)

Both become `blog/2025/foo.md`. This accommodates different annotation ID conventions.

### Transclusion Classes

Blog post pages use specific transclusion classes:
- `.include-annotation` - Triggers annotation system
- `.include-strict` - Strict mode (fail if missing)
- `data-include-template='annotation-blockquote-not'` - Custom template
- `.include-spinner-not` - No loading spinner
- `.id-not` - Don't generate automatic ID

For non-local URLs, adds `rel='canonical'` to indicate the original source.

---

## Configuration

### Constants (hardcoded in module)

| Constant | Value | Purpose |
|----------|-------|---------|
| `prefix` | `"blog"` | Output directory prefix |
| `authorU` | `"Gwern Branwen"` | Full author name for filtering |
| `authorID` | `"gwern"` | ID prefix for annotations |
| `lengthMin` | 600 | Minimum abstract length to qualify |
| `lengthMax` | 30000 | Warning threshold for long posts |
| `titleMax` | 51 | Title length for mobile display |

### From Config.Misc

- `C.cd` - Changes to site root directory
- `C.currentYear` / `C.currentYearS` - For year validation
- `C.lastYearS` - For CSS class assignment
- `C.author` / `C.authorL` - Author name variants

---

## Integration Points

### Input Dependencies

- **LinkMetadata** - `sortByDatePublished` for chronological ordering
- **LinkMetadataTypes** - `Metadata`, `MetadataList`, `MetadataItem`, `Path` types
- **LinkID** - `metadataItem2ID` extracts the custom ID from annotations
- **Metadata.Date** - `isYear` validates year strings
- **Typography** - `titlecase'` for title formatting

### Output Files

| File | Purpose |
|------|---------|
| `blog/YYYY/slug.md` | Individual blog post pages |
| `blog/index.md` | Full directory with links + transclusions |
| `blog/newest.md` | Recent posts for homepage |

### Build Integration

Called early in `hakyll.hs` build process (line 79), before main Hakyll compilation. Uses `writeUpdatedFile` to only write files when content changes (avoiding unnecessary rebuilds).

---

## Validation & Error Handling

The module is strict about data quality:

1. **Empty results** - Errors if no blog posts found
2. **Long posts** - Warning (not error) for posts >30k chars
3. **Invalid dates** - Error if not exactly 10 characters
4. **Long titles** - Warning for titles >51 chars
5. **Invalid IDs** - Error with details about expected format
6. **Path uniqueness** - Uses `isUniqueList` to detect duplicate paths
7. **Year validation** - Must be 2009 through current year

Error messages include the specific offending entries for debugging.

---

## See Also

- [Hakyll.hs](/backend/hakyll-hs) - Site generator that calls this module during builds
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database providing blog content
- [GTX.hs](/backend/gtx-hs) - Annotation data format storing blog entries
- [sync.sh](/backend/sync-sh) - Build orchestrator coordinating blog generation
- [transclude.js](/frontend/transclude-js) - Client-side transclusion that renders blog posts
- [XOfTheDay.hs](/backend/x-of-the-day-hs) - Related daily content rotation system
- [Typography.hs](/backend/typography-hs) - Text processing for blog titles
