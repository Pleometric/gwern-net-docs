---
sidebar_position: 3
---

# preprocess-annotation.sh

**Path:** `build/preprocess-annotation.sh` | **Language:** Bash | **Lines:** ~7

Simple stdin preprocessor for annotation editing workflow.

---

## Overview

`preprocess-annotation.sh` is a minimal wrapper script that reads raw annotation content from stdin and pipes it through `preprocess-markdown` to prepare it for manual editing in Emacs. The preprocessed output is then stored in annotation database files (`full.gtx` or `half.gtx`).

This script is part of the manual annotation editing workflow on gwern.net. When adding or updating link annotations, the raw scraped or draft content is passed through this script to normalize formatting before being opened in an editor buffer. Historically, this also included Pandoc HTML conversion and HTML Tidy formatting (now commented out), but the current version only performs Markdown preprocessing.

The simplicity of this script reflects the design philosophy that most annotation processing should happen in dedicated tools (`preprocess-markdown`) rather than being scattered across multiple scripts.

## Key Commands/Variables

**Main pipeline:**
```bash
cat - | preprocess-markdown
```

**Components:**
- `cat -` - Read from stdin (explicit pipe-through idiom)
- `preprocess-markdown` (from `$PATH`) - Core Markdown normalizer

**Commented-out legacy pipeline:**
```bash
# pandoc --mathjax --metadata title='Annotation preview' --to=html5 --from=html
# tidy -quiet --show-warnings no --show-body-only auto -indent -wrap 0 \
#      --clean yes --merge-divs no --break-before-br yes --logical-emphasis yes \
#      --quote-nbsp no || true
```
- `pandoc` - Would convert Markdown → HTML5 with MathJax support
- `tidy` - Would format/clean HTML with specific options
- `|| true` - Suppress tidy's warning exit status (always exits 1)

**Purpose of legacy code:**
These were likely used when annotations were stored/edited as HTML rather than Markdown. The pipeline would render a preview of how the annotation would appear. Now annotations are kept in Markdown form, so HTML conversion is deferred to build time.

## Usage

**Standard invocation (from stdin):**
```bash
echo "Some *raw* annotation text" | ./preprocess-annotation.sh > processed.txt
```

**In Emacs workflow:**
```elisp
;; Hypothetical Emacs function
(defun edit-annotation (url)
  (let ((raw-content (fetch-annotation-draft url)))
    (shell-command-on-region
      (point-min) (point-max)
      "~/wiki/static/build/preprocess-annotation.sh"
      (get-buffer-create "*annotation*"))))
```

**Manual testing:**
```bash
# Test preprocessing on a sample annotation
cat <<EOF | ./preprocess-annotation.sh
A *neural* network paper about [transformers](https://example.com).

Contains **bold** and _italic_ text.
EOF
```

**What `preprocess-markdown` likely does:**
- Normalize whitespace and line breaks
- Standardize Markdown formatting (list indentation, emphasis markers)
- Convert HTML entities to Unicode
- Strip or normalize problematic characters
- Apply site-specific Markdown conventions

**No arguments:**
- Script takes no command-line arguments
- All input via stdin, output to stdout (Unix filter pattern)

**Exit status:**
- Returns exit status of `preprocess-markdown` (pipe propagates last command status)

## See Also

- [Annotation.hs](/backend/annotation-hs) - Annotation scraper dispatcher
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database manager (reads/writes .gtx files)
- [GTX.hs](/backend/gtx-hs) - Annotation data format parser (full.gtx / half.gtx)
- [openReviewAbstract.sh](/shell/openreviewabstract) - Example scraper that feeds into this workflow
- [Metadata/Format.hs](/backend/metadata-format-hs) - HTML cleanup for abstracts
- [paragraphizer.py](/python/paragraphizer) - LLM-based paragraph splitting
