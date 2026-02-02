---
sidebar_position: 6
---

# title-cleaner.py

**Path:** `build/title-cleaner.py` | **Language:** Python | **Lines:** ~853

Removes cruft from webpage titles extracted from HTML `<title>` tags using GPT-4 language models.

---

## Overview

When scraping HTML pages, the `<title>` field often contains unwanted content: site names, domain suffixes, error messages, boilerplate text, or garbled Unicode. Manually writing rules to clean >20,000 URLs on gwern.net would be unmanageable due to the extreme long-tail of edge cases.

This script uses GPT-4.1-mini to intelligently clean titles based on "I know it when I see it" heuristics. It removes spam, cruft, and boilerplate while preserving meaningful content. The script can also convert inline Markdown (like `*emphasis*`) to HTML (`<em>emphasis</em>`).

The cleaning process is conservative: when uncertain, it returns the original title unchanged. Titles deemed completely useless (error pages, generic "Index", empty strings, site-wide constants) are replaced with empty strings for manual review.

**Note:** This script does *not* add italics for titles of works (books, movies, etc.) even when obviously appropriate, as English italicization rules are complex and context-dependent. That task is handled separately by `italicizer.py`. Both scripts accept stdin or arguments and can be chained: `echo "Title" | title-cleaner.py | italicizer.py`.

## Key Functions

- **Main processing**: Reads input from stdin or command-line argument, sends to GPT-4.1-mini
- **Extensive prompt examples**: 500+ examples covering common title problems
- **Output format**: Returns cleaned title or empty string (never adds commentary)
- **Markdown-to-HTML conversion**: Converts `*foo*` → `<em>foo</em>` and `Best-of-n` → `Best-of-<em>n</em>`

**Common title problems handled:**
- Site name suffixes: "Article Title - The New York Times" → "Article Title"
- Error messages: "404", "502 Bad Gateway", "Page not found" → `""`
- Boilerplate: "Gwern.net | Article" → "Article"
- Garbled Unicode: "miserâs heirs" → "miser's heirs"
- Useless constants: "index", "articles", "Reddit", "Home" → `""`
- Security checkpoints: "Vercel Security Checkpoint" → `""`
- Generic domains: "Notion – The all-in-one workspace..." → `""`

## Command Line Usage

```bash
# From stdin
echo "If I Sleep for an Hour, 30 People Will Die - The New York Times" | OPENAI_API_KEY="sk-XXX" python title-cleaner.py
# Output: If I Sleep for an Hour, 30 People Will Die

# From command-line argument
OPENAI_API_KEY="sk-XXX" python title-cleaner.py "404"
# Output: (empty string)

# Chain with italicizer
echo "Review of The Hobbit - Fantasy Books" | title-cleaner.py | italicizer.py
# Output: Review of <em>The Hobbit</em>

# Handle Unicode corruption
echo "After 92 years, millionaire miserâs heirs finally split $100M - TODAY.com" | OPENAI_API_KEY="sk-XXX" python title-cleaner.py
# Output: After 92 years, millionaire miser's heirs finally split $100M

# Handle inline Markdown
echo "Anton Seder's *The Animal in Decorative Art* (1896)" | OPENAI_API_KEY="sk-XXX" python title-cleaner.py
# Output: Anton Seder's <em>The Animal in Decorative Art</em> (1896)
```

**Requirements:**
- Python 3
- `openai` Python package
- Valid OpenAI API key in `$OPENAI_API_KEY` environment variable

**Edge cases:**
- Preserves important context: "It's (Still) Really Hard for Robots..."
- Removes author bylines: "by Robin Sloan The Message" → main title only
- Handles special characters: `&amp;` preserved, `\226\128\153` decoded
- Recognizes error pages: "Page not found : Stanford University" → `""`
- Domain-only titles: "Flashback Forum", "GoLocalPDX", "Notion" → `""`

## See Also

- [Metadata/Title.hs](/backend/metadata-title-hs) - Haskell module that calls this script
- [Config/Metadata/Title.hs](/backend/config-metadata-title-hs) - Rule-based title filtering configuration
- [italicizer.py](/python/italicizer) - Adds italics to book/movie/artwork titles
- [date-guesser.py](/python/date-guesser) - Companion LLM-based date extraction
- [paragraphizer.py](/python/paragraphizer) - Companion LLM-based paragraph splitting
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database manager
- [Annotation.hs](/backend/annotation-hs) - URL-to-scraper dispatcher
