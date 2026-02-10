---
sidebar_position: 1
---

# italicizer.py

**Path:** `build/italicizer.py` | **Language:** Python | **Lines:** ~1294

LLM-based script to add missing italics formatting to text using semantic understanding of English writing conventions.

---

## Overview

`italicizer.py` solves a common problem in web publishing: loss of italic formatting when extracting titles and text from PDFs, HTML pages, or APIs. The script uses GPT-4.1 to intelligently add `<em>` tags to text based on standard English typographic conventions, such as italicizing book titles, movie names, scientific species, foreign words, and ship names.

The tool is designed for automated metadata enhancement in the gwern.net build pipeline. It handles the long tail of edge cases that would be impractical to solve with rule-based systems, leveraging the LLM's knowledge of titles, proper nouns, and stylistic conventions. The script includes extensive few-shot examples (over 1,200 lines) covering books, films, periodicals, scientific names, foreign terms, musical terminology, and ambiguous cases.

When uncertain, the script returns an empty string `""` rather than risk incorrect formatting. This conservative approach prevents false positives while still catching most clear-cut cases.

## Key Functions

- **Main processing loop**: Reads input from stdin or command-line argument, sends to OpenAI API
- **Prompt construction**: Massive few-shot prompt with 1,200+ examples demonstrating when to italicize and when not to
- **Response handling**: Returns formatted text with `<em>` tags or empty string if no changes needed

## Command Line Usage

Set your OpenAI API key and pipe text to the script:

```bash
# From stdin
echo "Moby-Dick" | OPENAI_API_KEY="sk-XXX" python italicizer.py
# Output: <em>Moby-Dick</em>

# As argument
OPENAI_API_KEY="sk-XXX" python italicizer.py "The USS Enterprise"
# Output: The <em>USS Enterprise</em>

# No italics needed
echo "Herman Melville" | OPENAI_API_KEY="sk-XXX" python italicizer.py
# Output: ""
```

**Key rules encoded in the prompt:**
- **Italicize**: Book/movie/TV titles, periodicals, long poems, ship/plane/spaceship names, unfamiliar foreign words, scientific names, variables, musical terms
- **Don't italicize**: Personal/location names, software, short works, loanwords, brands, punctuation outside phrases, acronyms

## See Also

- [Annotation.hs](/backend/annotation-hs) - Calls processItalicizer during title processing
- [Metadata/Title.hs](/backend/metadata-title-hs) - Title extraction that may use italicization
- [title-cleaner.py](/python/title-cleaner) - Companion LLM-based title cleanup
- [paragraphizer.py](/python/paragraphizer) - Companion LLM-based text processing
- [date-guesser.py](/python/date-guesser) - Companion LLM-based date extraction
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database that stores formatted titles
