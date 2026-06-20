---
title: "github-issue-blockquote-outside.tmpl"
description: "This template generates the HTML structure for displaying GitHub issue content in popups and transclusions."
sidebar_position: 10
---

# github-issue-blockquote-outside.tmpl

This template generates the HTML structure for displaying GitHub issue content in popups and transclusions.

<div className="doc-meta">
  <div><strong>Path</strong><code>template/include/github-issue-blockquote-outside.tmpl</code></div>
  <div><strong>Language</strong>HTML5/Pandoc</div>
  <div><strong>Lines</strong>3</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/template/include/github-issue-blockquote-outside.tmpl">template/include/github-issue-blockquote-outside.tmpl</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the HTML/Pandoc templates and include fragments that shape rendered gwern.net pages around github-issue-blockquote-outside.tmpl.
</div>

## Overview

This template generates the HTML structure for displaying GitHub issue content in popups and transclusions. It wraps the issue content in a `<blockquote>` element, making it visually distinct from regular content and indicating that it's quoted material from an external source.

The template is part of the gwern.net annotation system, which fetches and displays rich previews of GitHub issues when users hover over or click on GitHub issue links. The "outside" variant places the blockquote wrapper around the entire content container, as opposed to other variants that might place semantic elements differently.

This template works in conjunction with the `Annotation.hs` module (which scrapes GitHub issue data) and the popup/transclude JavaScript modules (which render the populated template in the browser).

## Template Variables

- `contentTypeClass` - CSS class indicating the content type (e.g., `content-transform-github-issue`)
- `issueContent` - The main body content of the GitHub issue, including text, formatting, and any embedded media

## Output Structure

The template produces a simple two-level structure:

```html
<blockquote class="content-transform [type-specific-class]">
    <div class="data-field issue-content">[issue content]</div>
</blockquote>
```

The outer `<blockquote>` provides semantic meaning and default citation styling. The inner `<div>` with class `data-field issue-content` contains the actual issue body and may be targeted by CSS or JavaScript for specialized formatting.

---

<details className="generated-section">
<summary>See Also</summary>

- [github-issue-blockquote-not.tmpl](/templates/github-issue-blockquote-not) - Variant without blockquote wrapper
- [annotation-blockquote-not.tmpl](/templates/annotation-blockquote-not) - Similar pattern for annotations
- [Annotation.hs](/backend/annotation-hs) - Scrapes GitHub issue metadata and content
- [content.js](/frontend/content-js) - Content type system that determines template selection
- [popups.js](/frontend/popups-js) - Handles popup windowing and positioning
- [transclude.js](/frontend/transclude-js) - Handles inline content transclusion
- [extracts.js](/frontend/extracts-js) - Coordinator for displaying popup/transclude content
</details>
