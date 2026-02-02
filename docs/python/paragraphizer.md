---
sidebar_position: 5
---

# paragraphizer.py

**Path:** `build/paragraphizer.py` | **Language:** Python | **Lines:** ~791

LLM-powered tool to split run-on research paper abstracts into multiple topical paragraphs with added hyperlinks.

---

## Overview

`paragraphizer.py` reformats dense single-paragraph abstracts into readable multi-paragraph summaries. Research paper abstracts often follow a predictable structure (Background → Question → Methods → Results → Conclusion) but are written as impenetrable blocks of jargon-heavy text. This script uses GPT-4o-mini to add logical paragraph breaks and relevant Wikipedia/documentation links.

The script enforces strict validation to prevent content corruption:
- **Minimum length**: 100 characters (shorter texts don't benefit from splitting)
- **Paragraph breaks required**: Output must contain at least one `\n\n`
- **Length constraints**: Output can't exceed 2× original length (allowing for HTML links)
- **Number preservation**: All decimal numbers from input must appear in output (prevents silent data corruption)

Originally, the script required byte-for-byte equality (except newlines) to prevent rewording. This was relaxed in 2023 as GPT-4 became reliable enough to preserve exact wording while adding links. The main remaining failure mode is with very long texts (podcast transcripts), not abstract-length inputs.

## Key Functions

- **`_numbers(s: str)`**: Extracts all decimal/percent-like numbers via regex for validation
- **`_looks_ok(original: str, candidate: str) -> bool`**: Validates output has breaks, reasonable length, preserved numbers
- **OpenAI API call**: Uses `gpt-4o-mini` with temperature=0 for deterministic breaks
- **Validation and output**: Returns reformatted text or empty string `""` if validation fails

## Command Line Usage

```bash
# From stdin
cat abstract.txt | OPENAI_API_KEY="sk-XXX" python paragraphizer.py

# Direct argument
OPENAI_API_KEY="sk-XXX" python paragraphizer.py "Long abstract text here..."

# Example input (single paragraph):
echo "Most deep reinforcement learning algorithms distill experience into parametric behavior policies via gradient updates. While effective, this approach has disadvantages: computationally expensive, slow integration, limited capacity. We train a network to map past experiences to optimal behavior..." | OPENAI_API_KEY="sk-XXX" python paragraphizer.py

# Example output (multi-paragraph with links):
# Most deep [reinforcement learning](https://en.wikipedia.org/wiki/Reinforcement_learning) algorithms distill experience into parametric behavior policies via gradient updates. While effective, this approach has disadvantages: computationally expensive, slow integration, limited capacity.
#
# We train a network to map past experiences to optimal behavior...
```

**Automatic handling:**
- Too short (< 100 chars): Returns `""`
- Invalid output (no breaks, corrupted numbers, too long): Returns `""`
- Success: Returns multi-paragraph Markdown with hyperlinks

**Configuration:**
- Model: `gpt-4o-mini` (fast, cheap, instruction-following)
- Temperature: 0 (deterministic paragraph breaks)
- System prompt: "You are a helpful research assistant"

## See Also

- [Paragraph.hs](/backend/paragraph-hs) - Haskell module that calls this script
- [Annotation/Arxiv.hs](/backend/annotation-arxiv-hs) - Scraper that uses paragraph splitting
- [Annotation/Biorxiv.hs](/backend/annotation-biorxiv-hs) - Scraper that uses paragraph splitting
- [Annotation/OpenReview.hs](/backend/annotation-openreview-hs) - Scraper that uses paragraph splitting
- [italicizer.py](/python/italicizer) - Companion LLM-based title formatting
- [title-cleaner.py](/python/title-cleaner) - Companion LLM-based title cleanup
- [LinkMetadata.hs](/backend/link-metadata-hs) - Stores reformatted abstracts
