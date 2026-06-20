# Config.Utext

**Path:** `build/Config/Utext.hs` | **Language:** Haskell | **Lines:** 368

> Configuration data and unit-test cases for Utext Unicode rendering.

---

## Overview

`Config.Utext` defines the small rendering style record used by `Utext.hs` and the regression test data for the Unicode text renderer.

The exported `Style` record tracks whether rendering is currently bold, italic, or ligature-enabled. `defaultStyle` disables all three flags, and `traceLimit` caps excerpts in trace/error output.

## Test Data

`utextTestSuite` accepts two render functions: one for default rendering and one for ligature-enabled rendering. It returns only failing triples of `(input, expected, actual)`, so an empty list means the configured tests pass.

The test groups cover identity text, italic, bold, bold-italic, monospace, superscript, subscript, strikethrough, underline, small caps, smart quotes, links, HTML-in-Markdown, blocks, non-ASCII pass-through, digits, edge cases, and opt-in ligatures.

