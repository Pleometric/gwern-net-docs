
# Config.Typography

**Path:** `build/Config/Typography.hs` | **Language:** Haskell | **Lines:** ~234

> Configuration constants, test cases, and whitelists for typography transforms

---

## Overview

Config.Typography provides the configuration data that drives gwern.net's typography transformation system. Rather than containing transformation logic itself, this module exports test cases, validation whitelists, and numeric constants that the main [Typography](typography-hs) module consumes.

The module serves three primary purposes: (1) defining test cases for title-casing behavior, including edge cases around hyphens, apostrophes, and quotation marks; (2) maintaining a whitelist of words that look like surnames but shouldn't trigger citation formatting; and (3) providing comprehensive test cases for date range/duration annotation, which validates the complex logic that adds "X years ago" subscripts to historical dates.

This separation of configuration from logic follows a clean architecture pattern—the typography rules can be adjusted without touching the transformation code, and the extensive test suites serve as both documentation and regression protection for the nuanced typographic behaviors.

---

## Public API

### `cycleCount :: Int`

Number of visual variations for horizontal rule styling (value: 3).

**Used by:** [Typography](typography-hs) `rulersCycle` function to cycle through `horizontal-rule-nth-1`, `horizontal-rule-nth-2`, `horizontal-rule-nth-3` CSS classes.

---

### `titleCaseTestCases :: [(String, String)]`

Comprehensive test suite for title-casing behavior with 77 input/expected pairs.

**Used by:** [Typography](typography-hs) `titleCaseTest` function for regression testing.

**Coverage includes:**
- Hyphenated compounds (`"end-to-end testing"` → `"End-To-End Testing"`)
- Apostrophes in contractions vs possessives (`"Foo's bar"` → `"Foo's Bar"`)
- Quoted phrases within titles (`"'Two Truths and a Lie' As..."`)
- Mixed curly/straight quotes (`'` vs `'` vs `"` vs `"`)
- Parenthetical content (`"a janken (rock-paper-scissors) playing robot"`)
- Em-dashes with embedded clauses
- Preservation of ALL-CAPS (`"TEST-CASE"` stays `"TEST-CASE"`)

---

### `surnameFalsePositivesWhiteList :: [String]`

List of ~150 capitalized words that resemble surnames but shouldn't trigger citation formatting (e.g., "Et al." formatting for apparent author names).

**Used by:** [Typography](typography-hs) `citefyInline` to avoid false positives.

**Categories included:**
- Months and days (`"January"`, `"Monday"`, `"Fall"`, `"Winter"`)
- Geographic terms (`"America"`, `"Africa"`, `"Japan"`, `"London"`)
- Academic/temporal markers (`"Et"`, `"Al"`, `"Circa"`, `"Survey"`)
- Ordinals and descriptors (`"First"`, `"Early"`, `"Late"`, `"Original"`)
- Event names (`"Otakon"`, `"Fanime"`, `"Expo"`, `"Fair"`)
- Miscellaneous (`"LessWrong"`, `"Codex"`, `"Atari"`, `"Republicans"`)

---

### Date Range Bounds

```haskell
minRange      :: Int  -- 4: minimum year gap for range annotation
minDuration   :: Int  -- 11: minimum years ago threshold
maxDateSecond :: Int  -- 2100: latest plausible future date
minDateFirst  :: Int  -- 1501: earliest date to process (avoids false positives)
```

**Used by:** [Metadata.Date](metadata-date-hs) `dateRangeDuration` function.

**Rationale:** Dates before 1501 are too often ordinary numbers (page counts, measurements). Dates after 2100 are rare enough to ignore. These bounds prevent false positives like treating "1,850" as a year or "2561 AD" from science fiction as a real date range endpoint.

---

### `dateRangeDurationTestCases :: [(Int, Inline, Inline)]`

Test suite of 35+ cases for date range annotation, each containing:
- Current year context (Int)
- Input Pandoc Inline
- Expected transformed Inline

**Used by:** [Typography](typography-hs) test harness for date annotation validation.

**Test case categories:**

| Range | Example | Tests |
|-------|---------|-------|
| Simple year range | `1984–1987` | Subscript generation, duration calculation |
| Full date range | `1939-09-01--1945-09-02` | Day-level precision, "6y" duration |
| Single date | `1980` | Standalone "Xya" subscript |
| Too recent | `2020` (when year=2020) | No annotation (within threshold) |
| Non-dates | `$1541`, `#1029`, `1600px` | Currency, issue numbers, dimensions skipped |
| Comma-separated | `1,850`, `1,900-2,000` | Numbers with thousands separators skipped |
| Decade references | `1920s`, `1920's` | Handled as fuzzy dates |
| Malformed ranges | `1998–1998` | Same-year ranges still annotated |

---

## Internal Architecture

The module contains no functions—only data declarations. All values are pure constants evaluated at compile time.

**Data flow:**
```
Config.Typography exports
        ↓
Typography.hs imports qualified as C
        ↓
C.cycleCount → rulersCycle
C.titleCaseTestCases → titleCaseTest
C.surnameFalsePositivesWhiteList → citefyInline surname detection
```

The test case format for `dateRangeDurationTestCases` uses Pandoc's `Inline` type directly, making expected outputs unambiguous but verbose. The Unicode character `\8211` represents en-dash (–), appearing frequently in date ranges.

---

## Key Patterns

**Exhaustive edge case coverage:** The title case tests demonstrate gwern.net's attention to typographic detail—handling not just basic capitalization but the full complexity of English title conventions across different quote styles, hyphenation patterns, and punctuation contexts.

**Whitelist-based filtering:** Rather than trying to detect surnames algorithmically (which would require NLP), the false positive whitelist provides a pragmatic solution. Words are added as they're encountered, making the list a living document of edge cases.

**Year-parameterized tests:** The date range tests accept the "current year" as a parameter, allowing the test suite to validate behavior at different points in time. This catches subtle bugs where relative calculations ("37 years ago") would otherwise be tested against stale expected values.

---

## Configuration

All configuration is static—there are no runtime settings. To adjust typography behavior:

| Change | Edit |
|--------|------|
| Add HR style variations | Increase `cycleCount` |
| Fix title case bug | Add case to `titleCaseTestCases` |
| Stop word getting citation-formatted | Add to `surnameFalsePositivesWhiteList` |
| Adjust date annotation thresholds | Modify `minRange`, `minDuration`, etc. |

---

## Integration Points

**Consumers:**
- [Typography](typography-hs) — Primary consumer of all exports
- [Metadata.Date](metadata-date-hs) — Uses date range bounds

**No external dependencies:** This module imports only `Text.Pandoc` for the `Inline` type used in test cases.

**Build impact:** Changes to test cases don't affect site output, but changes to constants like `cycleCount` or the date thresholds will change rendered HTML.

---

## See Also

- [Typography.hs](/backend/typography-hs) - The transformation logic that consumes these configs
- [typography.js](/frontend/typography-js) - Client-side typography transforms (complementary to server-side)
- [Config.Misc](/backend/config-misc-hs) - Related configuration including `currentYear`
- [Config.Inflation](/backend/config-inflation-hs) - Similar data-heavy config module pattern
- [hakyll.hs](/backend/hakyll-hs) - Build pipeline that uses typography transforms
- [Metadata.Date](/backend/metadata-date-hs) - Date range duration annotation implementation
