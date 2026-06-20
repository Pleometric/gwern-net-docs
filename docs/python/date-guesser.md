---
title: "date-guesser.py"
description: "Many strings—page titles, abstracts, URLs, file names—contain dates in wildly varying formats."
sidebar_position: 2
---

# date-guesser.py

Many strings—page titles, abstracts, URLs, file names—contain dates in wildly varying formats.

<div className="doc-meta">
  <div><strong>Path</strong><code>build/date-guesser.py</code></div>
  <div><strong>Language</strong>Python</div>
  <div><strong>Lines</strong>858</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/date-guesser.py">build/date-guesser.py</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing Python helper scripts for metadata cleanup, text processing, PDFs, dates, or generated content around date-guesser.
</div>

## Overview

Many strings—page titles, abstracts, URLs, file names—contain dates in wildly varying formats. Writing regular expressions to cover all possible formats would be labor-intensive and error-prone. This script uses GPT-5-mini to intelligently extract dates using both syntactic and semantic understanding.

The script is conservative by design: when in doubt, it returns an empty string rather than risk guessing incorrectly. This makes it safe for automated processing pipelines where false positives would be costly. It understands complex edge cases like ArXiv IDs (YYMM.NNNNN format), leap years, month/day validity, Internet Archive timestamps vs. original publication dates, and more.

Date extraction returns the most precise format available: full dates when possible (YYYY-MM-DD), month precision when only year and month are clear (YYYY-MM), or just the year (YYYY) when that's all that's reliably available. The script validates all dates against calendar rules and filters out future dates and dates before 1000 AD.

## Key Functions

- **`validate_date_format(date_str)`**: Validates date string matches YYYY[-MM[-DD]] regex patterns
- **`validate_date_not_future(date_str)`**: Ensures date is not in the future and respects leap years and month lengths
- **`main()`**: Orchestrates the GPT-5-mini API call with extensive prompt examples covering edge cases
- **Prompt examples**: 300+ few-shot examples demonstrating proper date extraction behavior for URLs, metadata, ArXiv IDs, DOIs, timestamps, version numbers, and ambiguous formats

## Command Line Usage

```bash
# From stdin
echo 'https://erikbern.com/2016/04/04/nyc-subway-math' | OPENAI_API_KEY="sk-XXX" python date-guesser.py
# Output: 2016-04-04

# From command-line argument
OPENAI_API_KEY="sk-XXX" python date-guesser.py "Posted on April 4, 2016"
# Output: 2016-04-04

# Handle ArXiv IDs
echo "arXiv:2401.12345" | OPENAI_API_KEY="sk-XXX" python date-guesser.py
# Output: 2024-01

# Conservative on ambiguity
echo "Updated: 2 days ago" | OPENAI_API_KEY="sk-XXX" python date-guesser.py
# Output: (empty string)
```

**Requirements:**
- Python 3
- `openai` Python package
- Valid OpenAI API key in `$OPENAI_API_KEY` environment variable

**Edge cases handled:**
- ArXiv IDs (YYMM.NNNNN format where only first 4 digits are date)
- Leap year validation (e.g., 2024-02-29 valid, 2023-02-29 invalid)
- Month/day validity (e.g., rejects 2024-13-01, 2024-04-31)
- Date ranges (returns earliest date)
- Internet Archive vs. original publication dates
- Wikipedia articles (returns empty—no meaningful date)
- Product numbers, ISBNs, postal codes, room numbers (ignored)

<details className="generated-section">
<summary>See Also</summary>

- [Metadata/Date.hs](/backend/metadata-date-hs) - Haskell module that calls this script
- [Annotation.hs](/backend/annotation-hs) - Uses date guessing for new annotations
- [GTX.hs](/backend/gtx-hs) - Stores dates in annotation database
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database manager
- [title-cleaner.py](/python/title-cleaner) - Companion LLM-based title cleanup
- [paragraphizer.py](/python/paragraphizer) - Companion LLM-based paragraph splitting
</details>
