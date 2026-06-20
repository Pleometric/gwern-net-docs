---
title: "wikipedia-entry-blockquote-not.tmpl"
description: "This template generates the HTML structure for displaying Wikipedia article excerpts in popups and transclusions without using a element."
sidebar_position: 14
---

# wikipedia-entry-blockquote-not.tmpl

This template generates the HTML structure for displaying Wikipedia article excerpts in popups and transclusions without using a element.

<div className="doc-meta">
  <div><strong>Path</strong><code>template/include/wikipedia-entry-blockquote-not.tmpl</code></div>
  <div><strong>Language</strong>HTML5/Pandoc</div>
  <div><strong>Lines</strong>7</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/template/include/wikipedia-entry-blockquote-not.tmpl">template/include/wikipedia-entry-blockquote-not.tmpl</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the HTML/Pandoc templates and include fragments that shape rendered gwern.net pages around wikipedia-entry-blockquote-not.tmpl.
</div>

## Overview

This template generates the HTML structure for displaying Wikipedia article excerpts in popups and transclusions without using a `<blockquote>` element. Instead, all content is wrapped in semantic `<div>` elements, making it suitable for contexts where the Wikipedia content is presented as informational material rather than as a direct quotation.

The template creates a clean, structured layout with an optional thumbnail figure, a title line, and the article excerpt. The absence of blockquote semantics means the content is styled as general information rather than cited material. This variant is likely used in contexts like informational popups or educational annotations where citation semantics aren't required.

The layout uses conditional logic to optionally include thumbnail images, which appear before the title when present. The template integrates with the same annotation pipeline as other Wikipedia templates, receiving populated data from `Annotation.hs` and `LinkMetadata.hs`.

## Template Variables

- `contentTypeClass` - CSS class for content type styling (e.g., `content-transform-wikipedia`)
- `thumbnailFigure` - Optional `<figure>` element containing the Wikipedia article's thumbnail image
- `titleLine` - The Wikipedia article title
- `entryContent` - The main excerpt/summary content from the Wikipedia article

## Output Structure

The template produces this conditional structure:

```html
<div class="content-transform [type-class]">
    <!-- If thumbnail exists: -->
    <figure>[thumbnail]</figure>
    <!-- Always: -->
    <p class="data-field title">[title]</p>
    <div class="data-field entry-content">[entry content]</div>
</div>
```

The optional thumbnail appears first (when present), followed by the title in a `<p>` tag, and finally the article excerpt in a `<div>`. The `data-field` classes enable JavaScript and CSS targeting for dynamic behavior and styling.

---

<details className="generated-section">
<summary>See Also</summary>

- [wikipedia-entry-blockquote-inside.tmpl](/templates/wikipedia-entry-blockquote-inside) - Variant with blockquote around content
- [wikipedia-entry-blockquote-title-not.tmpl](/templates/wikipedia-entry-blockquote-title-not) - Variant with blockquote but no title
- [annotation-blockquote-not.tmpl](/templates/annotation-blockquote-not) - Similar pattern for general annotations
- [Annotation.hs](/backend/annotation-hs) - Scrapes Wikipedia article content and metadata
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database manager
- [extracts.js](/frontend/extracts-js) - Popup/transclude content display coordinator
- [popups.js](/frontend/popups-js) - Popup rendering system
- [content.js](/frontend/content-js) - Content type routing and template selection
</details>
