---
sidebar_position: 1
---

# clean-pdf.py

**Path:** `build/clean-pdf.py` | **Language:** Python | **Lines:** ~222

A utility for fixing formatting and spelling errors in text extracted from PDFs using GPT-4 language models.

---

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

## See Also

- [Annotation/PDF.hs](/backend/annotation-pdf-hs) - Backend PDF annotation processing in Haskell
- [title-cleaner.py](/python/title-cleaner) - Cleans webpage titles extracted from `<title>` tags
- [date-guesser.py](/python/date-guesser) - Extracts dates from natural language text
- [latex2unicode.py](/python/latex2unicode) - Converts LaTeX math to Unicode/HTML
- [upload.sh](/shell/upload) - File upload script that may process PDFs
- [LinkMetadata.hs](/backend/link-metadata-hs) - Metadata system that stores PDF annotations
