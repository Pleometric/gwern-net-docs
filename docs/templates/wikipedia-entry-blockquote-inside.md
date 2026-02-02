---
sidebar_position: 13
---

# wikipedia-entry-blockquote-inside.tmpl

**Path:** `template/include/wikipedia-entry-blockquote-inside.tmpl` | **Language:** HTML5/Pandoc | **Lines:** ~8

Template fragment for rendering Wikipedia entry content with blockquote inside the container.

## Overview

This template generates the HTML structure for displaying Wikipedia article excerpts in popups and transclusions. Unlike the "outside" blockquote variant, this template places the `<blockquote>` element around only the entry content itself, not the entire container. The article title remains outside the blockquote, positioned above it as a separate `<p>` element with a colon suffix.

This layout choice emphasizes the distinction between the title (metadata/attribution) and the quoted excerpt (content). It's semantically appropriate when you want the title to introduce the quotation rather than be part of it. The template also supports optional thumbnail images through conditional logic, which are placed inside the blockquote with the content.

The template works with the annotation pipeline: `Annotation.hs` scrapes Wikipedia articles, `LinkMetadata.hs` manages the database, and `extracts.js` renders the populated template for display.

## Template Variables

- `contentTypeClass` - CSS class for content type styling (e.g., `content-transform-wikipedia`)
- `titleLine` - The Wikipedia article title
- `thumbnailFigure` - Optional `<figure>` element containing the Wikipedia article's thumbnail image
- `entryContent` - The main excerpt/summary content from the Wikipedia article

## Output Structure

The template produces this conditional structure:

```html
<div class="content-transform [type-class]">
    <p class="data-field title">[title]:</p>
    <blockquote class="data-field entry-content">
        <!-- If thumbnail exists: -->
        <figure>[thumbnail]</figure>
        <!-- Always: -->
        [entry content]
    </blockquote>
</div>
```

The title appears as an introduction with a colon, followed by a blockquote containing the optional thumbnail and the article excerpt. The `<[IF thumbnailFigure]>` conditional syntax is processed during template rendering to include or omit the thumbnail.

---

## See Also

- [wikipedia-entry-blockquote-not.tmpl](/templates/wikipedia-entry-blockquote-not) - Variant without blockquote wrapper
- [wikipedia-entry-blockquote-title-not.tmpl](/templates/wikipedia-entry-blockquote-title-not) - Variant with blockquote but no title
- [annotation-blockquote-not.tmpl](/templates/annotation-blockquote-not) - Similar pattern for general annotations
- [Annotation.hs](/backend/annotation-hs) - Scrapes Wikipedia article content and metadata
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database manager
- [extracts.js](/frontend/extracts-js) - Popup/transclude content display coordinator
- [popups.js](/frontend/popups-js) - Popup rendering system
- [content.js](/frontend/content-js) - Content type routing
