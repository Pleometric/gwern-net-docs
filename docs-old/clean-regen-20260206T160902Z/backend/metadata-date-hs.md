
# Metadata.Date

**Path:** `build/Metadata/Date.hs` | **Language:** Haskell | **Lines:** ~159

> Date parsing, validation, and range annotation for annotations and typography

---

## Overview

This module handles all date-related operations in the gwern.net build system: validating date strings, guessing dates from filenames or free text, and annotating date ranges with durations. It's a critical piece of the annotation pipeline—every annotation's date field passes through this module's validation.

The module has two distinct responsibilities. First, it provides date validation and extraction for the annotation system: checking that dates conform to `YYYY[-MM[-DD]]` format and attempting to infer dates from local file paths or via an LLM-powered guesser. Second, it implements "date range duration" typography—a feature that automatically annotates date ranges like "1939–1945" with subscripts showing how long the range lasted and how long ago it ended.

The date-guessing logic is conservative by design. Rather than risk incorrect dates polluting the metadata, it returns empty strings when uncertain, leaving manual review as the fallback.

---

## Public API

### `isDate(String) → Bool`

Validates that a string is a valid date in `YYYY`, `YYYY-MM`, or `YYYY-MM-DD` format.

```haskell
isDate "" = True  -- empty is valid (makes lint checks easier)
isDate "2024" = True
isDate "2024-01" = True
isDate "2024-01-15" = True
isDate "2024-1-15" = False  -- must be zero-padded
```

**Called by:** `Test.hs`, annotation validation, `guessDateFromString`
**Calls:** `isYear`, `isValidDate`

### `dateTruncateBad(String) → String`

Strips fake precision from dates. Dates ending in `-01-01` or `-01` are often lies (meaning "sometime in YYYY"), so this truncates them to just the year.

```haskell
dateTruncateBad "2024-01-01" = "2024"
dateTruncateBad "2024-01" = "2024"
dateTruncateBad "2024-03-01" = "2024-03-01"  -- March 1st is probably real
```

**Called by:** Annotation processing
**Calls:** (pure)

### `guessDateFromLocalSchema(url, date) → String`

Extracts dates from local file paths following gwern.net's naming convention: `YYYY-surname-title.ext` or `YYYY-MM-DD-title.ext`.

```haskell
guessDateFromLocalSchema "/doc/ai/2020-10-10-barr.png" "" = "2020-10-10"
guessDateFromLocalSchema "/doc/ai/2020-barr.pdf" "" = "2020"
guessDateFromLocalSchema "http://cnn.com" "" = ""  -- external URLs unchanged
guessDateFromLocalSchema "/doc/foo.pdf" "2019" = "2019"  -- existing date preserved
```

**Called by:** `Annotation.linkDispatcher` (for local files without dates)
**Calls:** `sed` (regex extraction)

### `guessDateFromString(String) → IO String`

Invokes `date-guesser.py` (an LLM-powered script) to extract dates from arbitrary strings like titles, abstracts, or URLs. This is the slow path, used only when simpler methods fail.

```haskell
guessDateFromString "https://erikbern.com/2016/04/04/nyc-subway-math"
  -- → IO "2016-04-04"
```

**Called by:** `Annotation.linkDispatcher`, `GTX.fixDate`
**Calls:** `runShellCommand` → `date-guesser.py`, `isDate` (validation)

### `dateRangeDuration(todayYear, Inline) → Inline`

Transforms Pandoc `Str` or `Span.date-range` inlines containing date ranges into annotated spans with duration subscripts.

```haskell
-- "1939–1945" becomes:
-- <span class="date-range" title="...lasted 6 years...ending 80 years ago">
--   1939<sup>–</sup><sub>6</sub>1945<sub>80ya</sub>
-- </span>
```

**Called by:** `Typography.hs` (Pandoc AST transformation)
**Calls:** `dateRangeDurationRaw`, `dateDurationSingle`, `calculateDateSpan`, `formatDaysInLargestUnit`

---

## Internal Architecture

### Date Format Validation

All dates use ISO-like format with optional precision:

| Format | Example | Length |
|--------|---------|--------|
| Year only | `2024` | 4 |
| Year-month | `2024-01` | 7 |
| Full date | `2024-01-15` | 10 |

Validation uses `Data.Time.parseTimeM` with format strings `%Y`, `%Y-%m`, `%Y-%m-%d`.

### Date Range Regexes

Three compiled regexes (using `Text.Regex.TDFA`) detect date patterns:

| Regex | Matches | Example |
|-------|---------|---------|
| `dateRangeRegex` | Year ranges | `1939–1945`, `2020-2024` |
| `dateFullRangeRegex` | Full date ranges | `2020-09-27–2023-05-17` |
| `singleYearRegex` | Standalone years | `...in 1969...` |

The regexes are permissive (match any 4-digit year starting with 1 or 2). A separate guard filters `$`-prefixed matches, and year bounds are enforced later via `minDateFirst`/`maxDateSecond`.

### LLM Date Guesser

`date-guesser.py` is a Python script using OpenAI's API with extensive few-shot examples. It handles:
- Natural language dates ("Published: 02-29-2024")
- ArXiv IDs ("2401.12345" → "2024-01")
- Various international formats (DD/MM/YY vs MM/DD/YY)
- Edge cases (leap years, invalid days like Feb 30)

The script validates outputs against future dates and calendar correctness before returning.

---

## Key Patterns

### Conservative Date Guessing

Both `guessDateFromLocalSchema` and `guessDateFromString` preserve existing dates:

```haskell
guessDateFromLocalSchema url date =
  if head url /= '/' || date /= "" then date  -- only guess for local files without dates
  else ...
```

This prevents overwriting manually-curated dates with automated guesses.

### Subscript Duration Annotation

Date ranges get annotated with two subscripts:
1. **Range duration**: How long the period lasted (e.g., "6" for 1939–1945)
2. **Time since**: How long ago it ended (e.g., "80ya" for years ago)

The annotation includes a tooltip with the full description and handles edge cases like same-year ranges or dates too close to present.

---

## Configuration

Configuration lives in `Config.Typography`:

| Constant | Value | Purpose |
|----------|-------|---------|
| `minRange` | 4 | Minimum years for range subscript |
| `minDuration` | 11 | Minimum years ago for "ya" subscript |
| `minDateFirst` | 1501 | Earliest valid year (avoids matching ordinary numbers) |
| `maxDateSecond` | 2100 | Latest valid year |

These prevent annotating trivial ranges ("2023–2024") or recent events.

---

## Integration Points

### Events / State

- No events fired
- No shared state (pure functions except `guessDateFromString`)

### External Dependencies

- **`date-guesser.py`**: Called via shell for LLM-based date extraction. Requires `OPENAI_API_KEY` environment variable.
- **`Config.Typography.dateRangeDurationTestCases`**: Test cases for the duration annotation system.

### Module Interactions

```
Annotation.hs ──→ guessDateFromLocalSchema ──→ (pure regex)
             └──→ guessDateFromString ──→ date-guesser.py

Typography.hs ──→ dateRangeDuration ──→ (Pandoc Inline transforms)

Test.hs ──→ isDate, dateRangeDurationTestCasesTestsuite
```

---

## See Also

- [date-guesser.py](/python/date-guesser) - LLM-powered date extraction script
- [Annotation.hs](/backend/annotation-hs) - Uses date guessing for new annotations
- [GTX.hs](/backend/gtx-hs) - Calls date parsing during slow read
- [Metadata/Author.hs](/backend/metadata-author-hs) - Companion author processing module
- [Metadata/Format.hs](/backend/metadata-format-hs) - Companion string cleaning module
- [Metadata/Title.hs](/backend/metadata-title-hs) - Companion title processing module
- [LinkMetadata.hs](/backend/link-metadata-hs) - Stores processed dates in annotations

---
