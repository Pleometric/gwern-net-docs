---
title: "compressJPG"
description: "compressJPG optimizes.jpg /.jpeg files with a two-step pipeline:"
sidebar_position: 2
---

# compressJPG

compressJPG optimizes.jpg /.jpeg files with a two-step pipeline:

<div className="doc-meta">
  <div><strong>Path</strong><code>build/compressJPG</code></div>
  <div><strong>Language</strong>Bash</div>
  <div><strong>Lines</strong>83</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/compressJPG">build/compressJPG</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing shell automation, compression, upload/download helpers, linting, or preprocessing around compressJPG.
</div>

## Overview

`compressJPG` optimizes `.jpg` / `.jpeg` files with a two-step pipeline:

1. `jpegtran` for progressive/optimized re-encoding with metadata stripping
2. `mogrify -quality 60` for additional size reduction

It computes size ratio (`original / new`) and only replaces the original if ratio is greater than `1.2` (roughly >20% savings).

## Input Behavior

- If file arguments are provided, it processes those files.
- If no arguments are provided, it auto-discovers JPEG files in the current directory (non-recursive).
- If neither yields files, it exits with an error.

## Processing Steps

For each file:

1. Copy original to a temp file
2. Run `jpegtran -copy none -progressive -optimize`
3. Run `mogrify -quality 60` on the temp result
4. Compare byte sizes with `wc -c`
5. Replace original only if compression gain is sufficient

Temporary files are cleaned up at the end of each iteration.

## Dependencies

- `jpegtran`
- `mogrify`
- `bc`
- standard POSIX tooling (`find`, `wc`, `mktemp`, `cp`, `mv`, `rm`)

## Notes

- Uses `set -e` for fail-fast behavior.
- Color helper functions are inlined for portability (instead of sourcing `build/bash.sh`).
- Uses a conservative replacement threshold to avoid unnecessary churn on low-gain recompressions.

---

<details className="generated-section">
<summary>See Also</summary>

- [compress-gif](/shell/compress-gif) - GIF optimization companion
- [compress-png](/shell/compress-png) - PNG optimization companion
- [upload](/shell/upload) - Upload pipeline that may rely on pre-compressed assets
</details>
