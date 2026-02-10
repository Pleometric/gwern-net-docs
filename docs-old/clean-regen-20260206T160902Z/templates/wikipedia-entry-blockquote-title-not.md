---
sidebar_position: 15
---

# wikipedia-entry-blockquote-title-not.tmpl

**Path:** `template/include/wikipedia-entry-blockquote-title-not.tmpl` | **Language:** HTML5/Pandoc | **Lines:** ~7

Template fragment for rendering Wikipedia entry content with blockquote but no title.

## Overview

This template generates the HTML structure for displaying Wikipedia article excerpts in popups and transclusions where only the article content is shown within a `<blockquote>`, without a title line. This is the most minimal Wikipedia template variant, focusing purely on the quoted excerpt.

The "title-not" designation indicates that while the template structure could accommodate a title, in practice this variant is used when no title display is needed—likely because the context already provides sufficient attribution, or because the excerpt is being used in a context where the title would be redundant. The blockquote wrapper provides citation semantics for the content.

Like other Wikipedia templates, this variant supports optional thumbnail images through conditional logic. The thumbnails are placed inside the blockquote with the content. This template integrates with the standard annotation pipeline where `Annotation.hs` scrapes Wikipedia data and `extracts.js` handles rendering.

## Template Variables

- `contentTypeClass` - CSS class for content type styling (e.g., `content-transform-wikipedia`)
- `thumbnailFigure` - Optional `<figure>` element containing the Wikipedia article's thumbnail image
- `entryContent` - The main excerpt/summary content from the Wikipedia article

Note: Unlike other Wikipedia templates, this variant does not use a `titleLine` variable.

## Output Structure

The template produces this minimal conditional structure:

```html
<div class="content-transform [type-class]">
    <blockquote class="data-field entry-content">
        <!-- If thumbnail exists: -->
        <figure>[thumbnail]</figure>
        <!-- Always: -->
        [entry content]
    </blockquote>
</div>
```

The structure is streamlined: an outer container div, then a blockquote containing the optional thumbnail and the article excerpt. The absence of a title element makes this suitable for contexts where space is limited or where external context provides sufficient attribution.

---

## See Also

- [wikipedia-entry-blockquote-inside.tmpl](/templates/wikipedia-entry-blockquote-inside) - Variant with title and blockquote
- [wikipedia-entry-blockquote-not.tmpl](/templates/wikipedia-entry-blockquote-not) - Variant with title but no blockquote
- [annotation-blockquote-not.tmpl](/templates/annotation-blockquote-not) - Similar pattern for general annotations
- [Annotation.hs](/backend/annotation-hs) - Scrapes Wikipedia article content and metadata
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database manager
- [extracts.js](/frontend/extracts-js) - Popup/transclude content display coordinator
- [popups.js](/frontend/popups-js) - Popup rendering system
- [content.js](/frontend/content-js) - Content type routing and template selection
