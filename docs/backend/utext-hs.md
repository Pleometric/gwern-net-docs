---
title: "Utext.hs"
description: "Converts Pandoc documents and metadata strings into Unicode-rich plain text for social cards and other non-HTML contexts."
---

# Utext.hs

Converts Pandoc documents and metadata strings into Unicode-rich plain text for social cards and other non-HTML contexts.

<div className="doc-meta">
  <div><strong>Path</strong><code>build/Utext.hs</code></div>
  <div><strong>Language</strong>Haskell</div>
  <div><strong>Lines</strong>773</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/Utext.hs">build/Utext.hs</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the Haskell build pipeline, generators, metadata code, or backend utility behavior around Utext.
</div>

## Overview

`Utext.hs` renders Pandoc blocks and inlines to styled Unicode text instead of HTML. It is intended for social media cards, Open Graph descriptions, and other places where HTML markup is stripped but Unicode characters still render.

The module maps common Pandoc/HTML styling to Unicode equivalents: emphasis to mathematical italic, strong text to mathematical bold, code to mathematical monospace, superscripts/subscripts to Unicode super/subscript characters, strike/underline to combining marks, and small caps to IPA-style small capitals. Links render as label plus URL, while images render as parenthesized label/title/URL text.

## Public API

Pure conversion functions include `pandocToUtext`, `inlinesToUtext`, `inlineToUtext`, `rawText2Utext`, `rawHtml2Utext`, and `rawMarkdown2Utext`.

The IO API, including `pandocToUtextIO`, `rawText2UtextIO`, `rawHtml2UtextIO`, and `rawMarkdown2UtextIO`, uses `latex2unicode.py` for math conversion before rendering.

<details className="generated-section">
<summary>Configuration</summary>

Rendering style state and tests live in [Config.Utext](/backend/config-utext-hs). `Utext.hs` imports `Config.Utext.defaultStyle`, `Style`, and `traceLimit`.
</details>

## Limitations

The source documents several lossy cases: footnotes are dropped, tables are not fully rendered, ordered-list numbering style is ignored, arbitrary CSS in spans is ignored, and complex figures are best-effort.

