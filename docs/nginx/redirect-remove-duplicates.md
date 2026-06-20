---
title: "remove-duplicates.py"
description: "Lints and optionally prunes duplicate nginx redirect map keys."
---

# remove-duplicates.py

Lints and optionally prunes duplicate nginx redirect map keys.

<div className="doc-meta">
  <div><strong>Path</strong><code>nginx/redirect/remove-duplicates.py</code></div>
  <div><strong>Language</strong>Python</div>
  <div><strong>Lines</strong>331</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/nginx/redirect/remove-duplicates.py">nginx/redirect/remove-duplicates.py</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing server routing, redirects, deployment configuration, or Nginx behavior around remove-duplicates.
</div>

## Overview

`remove-duplicates.py` checks redirect include files such as `nginx/redirect/move.conf` and `nginx/redirect/broken.conf` for exact duplicate nginx `map` keys. In nginx `map` evaluation, the first matching key wins, so later identical keys are shadowed dead code.

The tool is designed for redirect tables where `/404#...` suffixes are used for debuggability. Duplicate keys can silently defeat those tags by making later entries unreachable.

## Modes

- Default lint mode exits nonzero when duplicates are found.
- `--allow-redundant` ignores duplicates whose values are identical.
- `--prune-shadowed --in-place` removes shadowed duplicate lines while keeping the first occurrence.
- Backups are written by default in prune mode unless `--no-backup` is passed.

## Parsing Rules

The parser reads simple one-line entries of the form `"KEY" "VALUE";`, strips nginx comments only outside double quotes, and reports duplicate keys across files in the order files were passed on the command line. It intentionally does not try to prove semantic equivalence between different regexes.

