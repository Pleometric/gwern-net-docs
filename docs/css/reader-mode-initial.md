---
title: "reader-mode-initial.css"
description: "This stylesheet (157 lines) implements a focused reading mode that strips away navigation, metadata, sidebars, and visual flourishes to present a clean, minimal reading."
sidebar_position: 7
---

# reader-mode-initial.css

This stylesheet (157 lines) implements a focused reading mode that strips away navigation, metadata, sidebars, and visual flourishes to present a clean, minimal reading.

<div className="doc-meta">
  <div><strong>Path</strong><code>css/reader-mode-initial.css</code></div>
  <div><strong>Language</strong>CSS</div>
  <div><strong>Lines</strong>162</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/css/reader-mode-initial.css">css/reader-mode-initial.css</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing styling, layout, typography, color, or mode-specific CSS around reader-mode-initial.
</div>

## Overview

This stylesheet (157 lines) implements a focused reading mode that strips away navigation, metadata, sidebars, and visual flourishes to present a clean, minimal reading experience. When activated via the page toolbar, it hides non-essential UI elements and optionally masks links to reduce visual interruptions.

Reader mode is inspired by browser reading modes and reading apps like Instapaper or Pocket. It's particularly useful for long-form essays where the reader wants to focus on the content without distractions from navigation, timestamps, or metadata fields.

The implementation uses `body.reader-mode-active` as a scoping class, allowing normal styles to remain unchanged while reader mode selectively overrides specific elements. There's also a `masked-links-hidden` mode that goes further by hiding link styling entirely, making links blend into the text.

## Key Selectors

### Elements Removed (`display: none`)

Complete removal of non-essential elements:
- `.reader-mode-not`: Explicitly marked non-reader-mode content
- `#navbar`: Top navigation bar
- Page metadata fields: `.page-status`, `.page-confidence`, `.page-importance`, `.page-backlinks`, `.page-link-bibliography`, `.page-similars`
- Table of Contents decorations: numbering (`#TOC ul li::before`), collapse button
- Sidenote columns: `#sidenote-column-left`, `#sidenote-column-right`
- Navigation and footer: `#navigation`, `#footer`
- Link icons: `.link-icon-hook`, `.recently-modified-icon-hook`
- Metadata decoration: inflation adjustments, date range subscripts (`.date-range sub`), file includes
- Annotation auxiliary links (for blog posts)

### Elements Hidden (`visibility: hidden`)

Preserve layout while hiding:
- `.date-range .subsup sub`: Date range subscripts are also set to `visibility: hidden`, but `display: none` from `.date-range sub` means spacing is not preserved

### Masked Links Mode

When `body.reader-mode-active.masked-links-hidden`:
- Link opacity reduced to 0 for: `.footnote-ref`, `.footnote-back`, `.footnote-back-block`
- Links inherit text color: `a:not(.popup-open):not(.popover-open)`
- Link underlines removed: `a:link` text-shadow removed
- Link cursor becomes text cursor
- Indicator hooks become invisible
- Code element borders restored to show boundaries

### Link Adjustments

- `.has-indicator-hook`: Reset margins and z-index
- `.indicator-hook`: Adapt backgrounds to blend with reader mode
- Footnotes section self-link: Maintain hover/click functionality

### Citation Formatting

Reader mode adjusts academic citations to be inline rather than superscript:
- `.cite-joiner`: Display normally (vs. hidden)
- `.cite-date::before`: Add space before date
- `.cite-date`: Reset vertical-align, font-size, line-height
- `.cite-author-plural::after`: Add space after authors

### Miscellaneous

- `p code`: Remove background/border (blend into text)
- `.date-range .subsup`: Reset margins
- `.reader-mode-disable-when-clicked`: Pointer cursor (for toggling off reader mode)
- `.reader-mode-style-not *`: Resets font weight/style/variant/color (not a full reset)

## Mobile Adjustments

```css
@media all and (max-width: 649px) {
    body.reader-mode-active article {
        margin-top: 0;
    }
}
```

Removes top margin on mobile for immediate content access.

## Loading

This file is bundled into `head-GENERATED.css` and loaded via `/static/css/head.css`, ensuring reader mode styles are available on first render without a separate request for this file.

The styles are scoped to only apply when `body.reader-mode-active` class is present, so they don't affect normal browsing. The class is toggled by JavaScript in response to user interaction with the reader mode button in the page toolbar.

---

<details className="generated-section">
<summary>See Also</summary>

- [reader-mode-js](/frontend/reader-mode-js) - JavaScript that toggles reader mode and manages masked links
- [reader-mode-initial-js](/frontend/reader-mode-initial-js) - Early reader mode bootstrap
- [initial](/css/initial) - Base styles that reader mode selectively overrides
- [dark-mode-adjustments](/css/dark-mode-adjustments) - Dark mode styling that works alongside reader mode
- [colors](/css/colors) - Color variables referenced by reader mode styles
</details>
