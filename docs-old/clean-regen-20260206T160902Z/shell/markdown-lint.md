---
sidebar_position: 5
---

# markdown-lint.sh

**Path:** `build/markdown-lint.sh` | **Language:** Bash | **Lines:** ~134

Comprehensive linter for Markdown files, checking for common errors, style violations, and metadata quality.

---

## Overview

This script performs extensive quality control on gwern.net Markdown files, running dozens of checks for syntax errors, broken links, style violations, metadata completeness, and content quality. It's designed to catch problems before they make it to production, including malformed syntax, problematic URLs, weasel words, imperial units, missing metadata fields, and various Pandoc-specific issues.

The linter operates in multiple passes: first checking raw Markdown syntax with pattern matching, then validating compiled HTML output, and finally running specialized Haskell tools for deeper analysis. All warnings are highlighted in red for visibility, making it easy to spot issues that need attention.

Beyond simple syntax checking, it enforces gwern.net's house style: preferring metric units, avoiding statistics jargon abuse, ensuring proper metadata length, checking for collapsed section summaries, and validating footnote structure.

## Key Commands/Variables

- **`wrap()`**: Wrapper function that runs a check and highlights warnings in red
- **`fgp()`**: Fixed-string grep with context and color (`grep -F`)
- **`egp()`**: Regex grep with case-insensitivity and color (`grep -E`)
- **`HTML`**: Temporary file for compiled Pandoc output
- **`PAGE`**: Current Markdown file being checked

**Helper functions:**
- `λ()`: Lambda-style anonymous functions for each check category
- `grep -F --context=1 --line-number --color=always`: Standard search with context
- `elinks -dump --force-html`: Text extraction from HTML for final validation

## Usage

```bash
./markdown-lint.sh <file.md> [file2.md] [file3.md] ...
```

**Arguments:**
- One or more Markdown files to check

**Example:**
```bash
$ ./markdown-lint.sh ~/wiki/ai.md
⚠ find bad URLS, unacceptable/unreliable/risky domains, malformed syntax:
42: Check out this [article](https://medium.com/foo)

⚠ "description:" metadata too short.

$ ./markdown-lint.sh *.md
# Checks all Markdown files in current directory
```

## Check Categories

### 1. Bad URLs and Domains
Searches for problematic URL patterns:
- Paywalled sites: `jstor.org`, `springer.com`, `wiley.com/doi/abs/`, `tandfonline.com`
- Unreliable: `medium.com`, `academia.edu`, `researchgate.net`, `quora.com`
- Dead/deprecated: `plus.google.com`, `geocities.com`, `photobucket`, `tinypic.com`
- Sci-Hub/LibGen mirrors (should use stable versions)
- Malformed: `https://wwww.`, `http://wwww.`, `web.archive.org` without full path
- Social media redirects: `fbclid=`, `x.com/#!`
- Arxiv version-pinning: `arxiv.org/abs/XXXX.XXXXXvN` (versions shouldn't be linked)

### 2. Link Destination Issues
Checks for suboptimal link targets:
- Linking to Arxiv/BioRxiv PDFs instead of abstract pages
- NCBI/PubMed numeric IDs with `/pubmed/` prefix
- PNAS abstracts without full path

### 3. Markdown Syntax Errors
Pattern matching for broken syntax:
- Broken image syntax: `![](`, `]()`, `](//`
- Malformed links: `](wiki/`, `](/wiki/`, `(www`, `)www`
- Wrong dash types: `———`, `——–`, ` --- `
- Spacing errors: ` )`, ` +-`, ` -+`, `'s "`
- Broken blockquotes: `^ > `, `^  > `, `^   > `
- Code block errors: `~~~~`, `~~~{.collape}`, wrong syntax classes
- Broken footnotes: `]^[`, `#fn[0-9]` (unstable, should use span IDs)
- Smallcaps errors: `[.smallcaps}`, `{,smallcaps}`
- Math errors: `\Mathcal{` (should be `\mathcal{`)
- Smart quote issues: `''`, ` `` `, ` " `

### 4. Weasel Words and Statistics Abuse
Filters out blockquotes, then searches for:
- ` significant `, ` significantly ` (in non-quoted text)
- ` obvious`, `basically`
- `reproducibility crisis`, `replicability crisis`
- ` the the ` (doubled words)

### 5. Units and Dates
Reminds to use metric:
- ` feet`, ` foot `, ` pound `, ` mile `, ` inch`
- Month-first dates: `January 1 2020` (prefer ISO: `2020-01-01`)

### 6. LaTeX Math Issues
- Unescaped single dollar signs: `$X$` (use `\$` or `$$X$$`)
- Broken delimiters

### 7. Abbreviations
- Hyphenated shortcuts like `e-mail`, `e-book` (write out fully)

### 8. Metadata Validation
Enforces field requirements and length limits:

| Field | Min chars | Max chars | Required? |
|-------|-----------|-----------|-----------|
| `title:` | 10 | 60 | Yes |
| `description:` | 90 | 320 | Yes |
| `thumbnail:` | 20 | - | Yes |
| `modified:` | - | - | Yes (except newsletters) |
| `next:` | - | - | Yes |
| `previous:` | - | - | Yes |

### 9. Compiled HTML Checks
After Pandoc compilation, validates:
- Syntax leakage: `\frac`, `\times`, `$title$`, `$description$`
- Broken HTML: `<del>`, `<!--`, `-->`, `<q>`, `</q>`
- URL fragments: `(http`, `)http`, `[http`, `]http`
- Special syntax escaping: `(!Wikipedia`, `(!Hoogle`
- Self-references: `http://gwern.net`, `https://www.gwern.net`

### 10. Specialized Checks
- **Line length**: `markdown-length-checker.hs` - flags overly long lines
- **Footnote length**: `markdown-footnote-length.hs` - warns about excessive footnote sizes
- **Collapsed sections**: Validates that all `class="collapse"` headers have accompanying summaries
- **Duplicate links**: Extracts all links and counts duplicates (may indicate over-linking)

## Color Coding

- **⚠ WARNING**: Red background for all warnings
- Matched text: Color highlighting in grep output
- Context: ±1 line around matches with line numbers

## Dependencies

- `bash`: Shell interpreter
- `grep`: Pattern matching (both `-F` fixed-string and `-E` regex)
- `pandoc`: Markdown → HTML compilation
- `elinks`: Text extraction from HTML
- `markdown-length-checker.hs`: Custom Haskell line-length validator
- `markdown-footnote-length.hs`: Custom Haskell footnote analyzer
- `link-extractor.hs`: Custom Haskell link parser
- `runghc`: Haskell interpreter for inline scripts

## See Also

- [sync.sh](/backend/sync-sh) - Build process that runs linting
- [preprocess-markdown.hs](/backend/preprocess-markdown-hs) - Markdown preprocessing pipeline
- [Typography.hs](/backend/typography-hs) - Pandoc AST transformations
- [markdown-length-checker.hs](/backend/markdown-length-checker-hs) - Line length validator
- [markdown-footnote-length.hs](/backend/markdown-footnote-length-hs) - Footnote size analyzer
- [anchor-checker.php](/php/anchor-checker) - HTML anchor validation
