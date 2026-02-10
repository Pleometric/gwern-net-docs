---
sidebar_position: 6
---

# github-issue-blockquote-not.tmpl

**Path:** `template/include/github-issue-blockquote-not.tmpl` | **Language:** HTML5/Pandoc | **Lines:** ~2

Template for rendering GitHub issue content without blockquote wrapper elements.

## Overview

This template produces a simple wrapper for GitHub issue content in gwern.net's content display system. It wraps the issue content in a `<div>` with appropriate classes but without using blockquote elements, making it suitable for contexts where the content should be displayed as primary content rather than quoted material.

The template is part of gwern.net's content transformation system, which handles various external content types including GitHub issues. When a GitHub issue is loaded (either as popup content or transcluded into a page), this template provides the HTML structure wrapping the rendered issue content.

This layout is generated during content processing, either at build time by the Haskell backend or at runtime by the JavaScript frontend when loading GitHub issue content dynamically. The "blockquote-not" variant indicates this is for display contexts where blockquote styling would be inappropriate.

## Template Variables

| Variable | Type | Description |
|----------|------|-------------|
| `contentTypeClass` | String | CSS class indicating content type (likely "github-issue" or similar) |
| `issueContent` | HTML | The rendered GitHub issue content (title, body, comments, metadata) |

## Output Structure

The template generates a minimal two-div wrapper:

```html
<div class="content-transform [content-type-class]">
  <div class="data-field issue-content">[rendered issue HTML]</div>
</div>
```

Key structural features:

- **Outer wrapper**: `content-transform` class indicates this is transformed external content
- **Content type class**: Dynamic class for content-type-specific styling (e.g., `github-issue`)
- **Inner wrapper**: `data-field issue-content` contains the actual rendered issue
- **No blockquote**: Uses only `<div>` elements, not `<blockquote>`
- **Minimal structure**: Two-level nesting with straightforward semantics

## Content Type Classification

The `contentTypeClass` variable allows the frontend to style different content types appropriately:

- **github-issue**: For GitHub issue/PR content
- **github-gist**: For GitHub Gist embeds
- **Other types**: Potentially used for other external content (Stack Overflow, Reddit, etc.)

This class is applied by the content loading system based on the URL pattern or content source detection.

## Integration with Content System

This template is used by:

1. **Content loaders**: JavaScript functions that fetch and transform GitHub content
2. **Popup system**: When displaying GitHub issues in popup windows
3. **Transclusion**: When embedding GitHub issues directly into page content
4. **Build system**: Possibly used during static compilation for pre-rendered includes

The `issueContent` variable is populated by:

- **API responses**: GitHub API data transformed into HTML
- **Scraped content**: GitHub page HTML parsed and cleaned
- **Cached content**: Pre-rendered issue content from annotation database

## Styling Hooks

The wrapper structure provides CSS hooks for:

- **.content-transform**: General styling for all transformed external content
- **.[contentTypeClass]**: Specific styling for content type (e.g., `.github-issue`)
- **.data-field**: Consistent styling for data fields across all templates
- **.issue-content**: Specific targeting of the issue content container

This allows the CSS to:

- Apply consistent spacing/borders to all external content
- Style GitHub issues differently from other content types
- Target issue-specific elements (comments, metadata, code blocks)

## Semantic Considerations

Unlike "blockquote-outside" variants, this template uses `<div>` elements instead of `<blockquote>`:

- **Not quotation**: Indicates content is displayed as primary material, not quoted reference
- **Screen readers**: Won't announce as quoted content
- **Styling**: Won't inherit blockquote indentation/styling
- **Semantic neutrality**: Generic container without semantic implications

This is appropriate when GitHub issues are displayed as main content (e.g., in a dedicated popup or embedded section) rather than as quoted references.

---

## See Also

- [github-issue-blockquote-outside.tmpl](/templates/github-issue-blockquote-outside) - Variant with blockquote wrapper
- [annotation-blockquote-not.tmpl](/templates/annotation-blockquote-not) - Similar "no-blockquote" pattern for annotations
- [Annotation.hs](/backend/annotation-hs) - Scrapes GitHub issue metadata and content
- [content.js](/frontend/content-js) - Content type system that determines template selection
- [popups.js](/frontend/popups-js) - Popup system that displays issue popups
- [transclude.js](/frontend/transclude-js) - Transclusion system for embedding external content
- [extracts.js](/frontend/extracts-js) - Coordinates popup/transclude content display
