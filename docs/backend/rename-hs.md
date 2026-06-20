---
title: "rename.hs"
description: "Generates shell commands for safely renaming pages across the gwern.net codebase"
---

# rename.hs

Generates shell commands for safely renaming pages across the gwern.net codebase

<div className="doc-meta">
  <div><strong>Path</strong><code>build/rename.hs</code></div>
  <div><strong>Language</strong>Haskell</div>
  <div><strong>Lines</strong>14</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/rename.hs">build/rename.hs</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the Haskell build pipeline, generators, metadata code, or backend utility behavior around rename.
</div>

## Overview

`rename.hs` is a tiny code generator that produces a multi-step shell script for renaming pages on gwern.net. Renaming a page is surprisingly complex because links can appear in many different syntactic forms across markdown files, HTML includes, and configuration files. Rather than attempt the transformation directly, this script outputs a sequence of commands that the user can review and execute.

The generated script handles: git file moves, multiple link syntax variants (markdown, HTML attributes with both single and escaped double quotes), and nginx redirect configuration updates. This defensive approach ensures no link references are silently broken by a rename operation.

---

## Public API

### `main :: IO ()`

Reads two command-line arguments (old path, new path) and prints a shell script to stdout.

**Called by:** Manual invocation from command line
**Calls:** `foo`

### `foo :: String -> String -> IO ()`

The actual script generator. Takes old and new paths (without `.md` extension or leading dot) and constructs a `&&`-chained shell command sequence.

**Called by:** `main`
**Calls:** `putStrLn`

---

## Internal Architecture

The script is minimal—just 15 lines. It builds a single string by concatenating six shell commands:

```
git mv '.{old}.md' '.{new}.md' && \
gwsed.sh ' {old}' ' {new}' && \
gwsed.sh '](old' '](new' && \
gwsed.sh 'href="{old}"' 'href="{new}"' && \
gwsed.sh 'href=\"{old}\"' 'href=\"{new}\"' && \
echo '"~^{old}$" "{new}";' >> ~/wiki/static/nginx/redirect/move.conf
```

Each step targets a different link format:
1. **git mv** — Move the actual file (note leading dot for hidden files)
2. **Space-prefixed** — Catches references like "see /foo" in prose
3. **Markdown links** — `](/old-path)` → `](/new-path)`
4. **HTML href (plain)** — `href="/old-path"` with regular quotes
5. **HTML href (escaped)** — `href=\"/old-path\"` with backslash-escaped quotes (common in JSON/JS)
6. **nginx redirect** — Appends a regex redirect rule so old URLs still work

---

<details className="generated-section">
<summary>Key Patterns</summary>

**Code generation over direct action**: Rather than executing the rename itself, the script outputs commands for the user to run. This allows inspection before execution—critical since a broken rename can corrupt many files.

**Pattern multiplication**: The same path must be searched in multiple syntactic contexts. This is a common theme in gwern.net's tooling: content appears in markdown, HTML, and escaped forms, requiring parallel handling.

**Hidden file convention**: The `.{path}.md` naming suggests gwern.net may use hidden files for certain content, or this is a quirk of the local directory structure.

---
</details>

<details className="generated-section">
<summary>Configuration</summary>

None. The script is hardcoded to:
- Assume files are `.md` with leading dots
- Use `gwsed.sh` for replacements
- Append redirects to `~/wiki/static/nginx/redirect/move.conf`

---
</details>

## Integration Points

### Dependencies

| Tool | Purpose |
|------|---------|
| `git` | Version-controlled file moves |
| `gwsed.sh` | Site-wide string replacement utility |
| `nginx.conf` | Redirect rules for URL preservation |

### Downstream

The generated nginx redirect rules are picked up by the web server configuration, ensuring old URLs continue to work after a rename.

---

## Usage Example

```bash
# Generate rename script for moving "/spaced-repetition" to "/srs"
$ runghc rename.hs /spaced-repetition /srs
git mv './spaced-repetition.md' './srs.md' && gwsed.sh ' /spaced-repetition' ' /srs' && gwsed.sh '](/spaced-repetition' '](/srs' && gwsed.sh 'href="/spaced-repetition"' 'href="/srs"' && gwsed.sh 'href=\"/spaced-repetition\"' 'href=\"/srs\"' && echo '"~^/spaced-repetition$" "/srs";' >> ~/wiki/static/nginx/redirect/move.conf

# Review output, then execute
$ runghc rename.hs /spaced-repetition /srs | bash
```

---

## Limitations

- Does not handle anchor/fragment references (`#section-id`)
- No validation that paths exist or are valid
- Assumes all files use `.md` extension
- The leading dot in git mv (`.{path}.md`) may not match all file patterns
- No rollback mechanism if partial execution fails

---

<details className="generated-section">
<summary>See Also</summary>

- [Utils.hs](/backend/utils-hs) - Core utilities for string manipulation and file operations
- [stringReplace.hs](/backend/string-replace-hs) - Parallel string replacement utility for batch processing
- [sync.sh](/backend/sync-sh) - Build orchestrator that may invoke page renames
- [redirectGuesser](/backend/nginx-redirect-guesser-hs) - Generates nginx redirect rules for broken URLs
- [gwern.net.conf](/nginx/gwern-net-conf) - Nginx configuration where redirects are stored
</details>
