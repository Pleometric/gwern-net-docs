---
title: "clean-pdf.py"
description: "When copying text from PDFs, the result is often malformatted with broken hyphens, ligature artifacts (like 'ﬄ'), spurious line breaks, and OCR errors."
sidebar_position: 1
---

# clean-pdf.py

When copying text from PDFs, the result is often malformatted with broken hyphens, ligature artifacts (like 'ﬄ'), spurious line breaks, and OCR errors.

<div className="doc-meta">
  <div><strong>Path</strong><code>build/clean-pdf.py</code></div>
  <div><strong>Language</strong>Python</div>
  <div><strong>Lines</strong>224</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/clean-pdf.py">build/clean-pdf.py</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing Python helper scripts for metadata cleanup, text processing, PDFs, dates, or generated content around clean-pdf.
</div>

## Overview

When copying text from PDFs, the result is often malformatted with broken hyphens, ligature artifacts (like 'ﬄ'), spurious line breaks, and OCR errors. This script uses OpenAI's GPT-4.1-mini model to intelligently fix these issues while preserving the original content and meaning.

The script is designed for interactive use via the clipboard: copy malformatted PDF text, pipe it through `clean-pdf.py`, and get clean, properly formatted text back. It's particularly useful for extracting academic papers, research documents, and other professional content where formatting errors would interfere with citation or quotation.

The cleaning process is conservative—it fixes only PDF/OCR artifacts and does not rewrite, paraphrase, or editorialize the content. It handles complex cases like removing author credentials, stripping footnote markers, fixing ALL-CAPS titles, and joining hyphenated words split across lines.

## Key Functions

- **Main processing loop**: Reads input from stdin or command-line argument, sends to GPT-4.1-mini via OpenAI API
- **Prompt engineering**: Provides extensive examples (100+ lines) demonstrating correct cleaning behavior
- **Post-processing**: Strips any XML-like `<text>` tags that might leak from the LLM response
- **Format preservation**: Maintains citations, links, formatting tags (like `<strong>`, `<a href>`), and special characters

## Command Line Usage

```bash
# From clipboard (using xclip on Linux)
xclip -o | OPENAI_API_KEY="sk-XXX" python clean-pdf.py

# From command-line argument
OPENAI_API_KEY="sk-XXX" python clean-pdf.py "Malfor-\nmatted text here"

# Typical workflow
xclip -o | OPENAI_API_KEY="sk-XXX" python clean-pdf.py | xclip -i
```

**Requirements:**
- Python 3
- `openai` Python package
- Valid OpenAI API key in `$OPENAI_API_KEY` environment variable

**Cost:** Uses GPT-4.1-mini model for cost-efficiency while maintaining high quality.

---

<details className="generated-section">
<summary>See Also</summary>

- [Annotation/PDF.hs](/backend/annotation-pdf-hs) - Backend PDF annotation processing in Haskell
- [title-cleaner.py](/python/title-cleaner) - Cleans webpage titles extracted from `<title>` tags
- [date-guesser.py](/python/date-guesser) - Extracts dates from natural language text
- [latex2unicode.py](/python/latex2unicode) - Converts LaTeX math to Unicode/HTML
- [upload.sh](/shell/upload) - File upload script that may process PDFs
- [LinkMetadata.hs](/backend/link-metadata-hs) - Metadata system that stores PDF annotations
</details>
