---
title: "Config.Utext"
description: "Configuration data and unit-test cases for Utext Unicode rendering."
---

# Config.Utext

Configuration data and unit-test cases for Utext Unicode rendering.

<div className="doc-meta">
  <div><strong>Path</strong><code>build/Config/Utext.hs</code></div>
  <div><strong>Language</strong>Haskell</div>
  <div><strong>Lines</strong>368</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/Config/Utext.hs">build/Config/Utext.hs</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the Haskell build pipeline, generators, metadata code, or backend utility behavior around Config.Utext.
</div>

## Overview

`Config.Utext` defines the small rendering style record used by `Utext.hs` and the regression test data for the Unicode text renderer.

The exported `Style` record tracks whether rendering is currently bold, italic, or ligature-enabled. `defaultStyle` disables all three flags, and `traceLimit` caps excerpts in trace/error output.

## Test Data

`utextTestSuite` accepts two render functions: one for default rendering and one for ligature-enabled rendering. It returns only failing triples of `(input, expected, actual)`, so an empty list means the configured tests pass.

The test groups cover identity text, italic, bold, bold-italic, monospace, superscript, subscript, strikethrough, underline, small caps, smart quotes, links, HTML-in-Markdown, blocks, non-ASCII pass-through, digits, edge cases, and opt-in ligatures.

