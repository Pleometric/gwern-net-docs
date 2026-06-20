# remove-duplicates.py

**Path:** `nginx/redirect/remove-duplicates.py` | **Language:** Python | **Lines:** 331

> Lints and optionally prunes duplicate nginx redirect `map` keys.

---

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

