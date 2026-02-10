---
sidebar_position: 3
---

# daterange-checker.py

**Path:** `build/daterange-checker.py` | **Language:** Python | **Lines:** ~327

Validates that dates auto-annotated with CSS year-range markup are actual dates and not incorrectly formatted numbers.

---

## Overview

Gwern.net automatically annotates year strings (matching `[12][0-9]{3}`) with CSS markup showing how many years ago they were. For example, in 2024, "1975" becomes `<span class="date-range">1975<sub>49ya</sub></span>` to provide historical context for readers.

However, this pattern-based approach has a flaw: numbers that should be comma-separated (like "1,000 units") sometimes lack commas and get incorrectly annotated as dates (e.g., "1000 AD" vs "1000px"). This script uses GPT-4.1-mini to validate date annotations in context, identifying false positives where numbers were misclassified as years.

The script is conservative: for correct date annotations, it prints nothing. For incorrect ones (or uncertain cases), it prints the full input HTML, allowing manual review. This makes it suitable for automated quality control in the site build process.

## Key Functions

- **Main prompt**: Instructs GPT-4.1-mini to classify date annotations as correct or incorrect based on semantic context
- **Conservative defaults**: When uncertain, outputs the input for human review
- **Extensive examples**: 100+ training examples covering common error patterns (pixel dimensions, time notation, image resolutions, calculations, ratios)
- **Context-aware**: Understands domain-specific patterns (e.g., "1024×1024" is image dimensions, not years)

## Command Line Usage

```bash
# From stdin
echo '<text>1100px is close to my original A/B test indicating <span class="date-range">1000<sub>1,024ya</sub></span>px was the leading candidate</text>' | OPENAI_API_KEY="sk-XXX" python daterange-checker.py
# Output: <p>1100px is close to my original A/B test indicating <span class="date-range">1000<sub>1,024ya</sub></span>px...</p>
# (because 1000px is a CSS dimension, not a year)

# From command-line argument
OPENAI_API_KEY="sk-XXX" python daterange-checker.py '<text>Published in <span class="date-range">2008<sub>16ya</sub></span></text>'
# Output: (empty string, because 2008 is correctly a year)
```

**Requirements:**
- Python 3
- `openai` Python package
- Valid OpenAI API key in `$OPENAI_API_KEY` environment variable

**Common false positives detected:**
- Pixel dimensions: `1000px`, `1024×1024`, `1920×1080`
- Times: `1930 hours`, `2400hrs`
- Technical specifications: `1000 units`, `2000 days`
- Math expressions: `X/1000 mg`
- Image resolutions: `1024<sub>1,000ya</sub>×1024<sub>1,000ya</sub>`

**Correct date examples:**
- Publication years: `1975`, `2008`, `2023`
- Historical dates: `1886`, `1927`, `1957`
- Date ranges: `2006–2007`, `1961–1986`

---

## See Also

- [Metadata/Date.hs](/backend/metadata-date-hs) - Backend date parsing and validation in Haskell
- [date-guesser.py](/python/date-guesser) - Extracts dates from natural language
- [collapse-checker.py](/python/collapse-checker) - Validates collapsed HTML blocks
- [rewrite.js](/frontend/rewrite-js) - Frontend JavaScript that applies date-range CSS
- [sync.sh](/backend/sync-sh) - Build orchestrator that runs validation checks
- [Typography.hs](/backend/typography-hs) - Text processing that may generate date ranges
