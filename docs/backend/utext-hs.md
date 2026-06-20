# Utext.hs

**Path:** `build/Utext.hs` | **Language:** Haskell | **Lines:** 773

> Converts Pandoc documents and metadata strings into Unicode-rich plain text for social cards and other non-HTML contexts.

---

## Overview

`Utext.hs` renders Pandoc blocks and inlines to styled Unicode text instead of HTML. It is intended for social media cards, Open Graph descriptions, and other places where HTML markup is stripped but Unicode characters still render.

The module maps common Pandoc/HTML styling to Unicode equivalents: emphasis to mathematical italic, strong text to mathematical bold, code to mathematical monospace, superscripts/subscripts to Unicode super/subscript characters, strike/underline to combining marks, and small caps to IPA-style small capitals. Links render as label plus URL, while images render as parenthesized label/title/URL text.

## Public API

Pure conversion functions include `pandocToUtext`, `inlinesToUtext`, `inlineToUtext`, `rawText2Utext`, `rawHtml2Utext`, and `rawMarkdown2Utext`.

The IO API, including `pandocToUtextIO`, `rawText2UtextIO`, `rawHtml2UtextIO`, and `rawMarkdown2UtextIO`, uses `latex2unicode.py` for math conversion before rendering.

## Configuration

Rendering style state and tests live in [Config.Utext](/backend/config-utext-hs). `Utext.hs` imports `Config.Utext.defaultStyle`, `Style`, and `traceLimit`.

## Limitations

The source documents several lossy cases: footnotes are dropped, tables are not fully rendered, ordered-list numbering style is ignored, arbitrary CSS in spans is ignored, and complex figures are best-effort.

