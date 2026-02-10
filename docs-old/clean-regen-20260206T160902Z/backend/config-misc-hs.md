
# Config.Misc

**Path:** `build/Config/Misc.hs` | **Language:** Haskell | **Lines:** ~283

> Global constants, date utilities, and miscellaneous configuration values used throughout the build system

---

## Overview

Config.Misc is the catch-all configuration module for gwern.net, containing global constants, date/time utilities, and various lookup tables that don't fit elsewhere. It provides the site author identity, filesystem paths, date arithmetic, file extension mappings, and threshold values used across the build pipeline.

The module makes extensive use of `unsafePerformIO` to provide pure-looking constants that are actually computed at program startup (current year, today's date, home directory). This is a pragmatic design choice: these values don't change during a build run, so computing them once and treating them as constants simplifies the API for downstream consumers.

Key categories of functionality: author/path configuration, date utilities for "new" content detection, backlink filtering rules, Arxiv abstract cleanup patterns, file extension human-readable names, and test case data for other modules.

---

## Public API

### Constants

#### `author`, `authorL :: String`

Site author name ("Gwern") and lowercase variant.

**Called by:** Metadata generation, RSS feeds, copyright notices

#### `root :: FilePath`

Absolute path to the wiki directory (`~/wiki/`). Computed once via `unsafePerformIO`.

**Called by:** Most file I/O operations throughout the build system

#### `cd :: IO ()`

Changes current directory to `root`. Convenience for REPL usage.

#### `userAgent :: String`

Firefox user agent string for HTTP requests. Used when fetching external resources.

**Called by:** Link archiving, metadata scraping

### Date Utilities

#### `currentYear`, `lastYear :: Int`

Current and previous year as integers. Computed at startup.

**Called by:** Year range validation, copyright dates

#### `currentYearS`, `lastYearS :: String`

String versions of the above.

#### `todayDay :: IO Integer`

Returns today's Modified Julian Day number.

#### `todayDayString :: IO String`

Returns today's date as "YYYY-MM-DD".

#### `todayDayStringUnsafe :: String`

Pure version of `todayDayString` via `unsafePerformIO`.

#### `yesterdayDayString :: IO String`

Returns yesterday's date as "YYYY-MM-DD".

#### `dayStringFromToday :: Integer -> IO String`

Generic date string computation: adds offset days to today.

#### `currentMonthAgo :: String`

Date string for `isNewWithinNDays` days ago. Used for "new content" highlighting.

#### `toDay :: String -> Day`

Parses flexible date formats (YYYY, YYYY-MM, YYYY-MM-DD) into a `Day`. Missing components default to first of period.

**Called by:** `isOlderThan`, date comparisons

#### `isOlderThan :: Integer -> String -> String -> Bool`

Returns `True` if the difference between two dates exceeds threshold days.

```haskell
isOlderThan 90 "2025-01-01" "2025-06-02"  -- True (>90 days apart)
```

**Called by:** Staleness detection, cache invalidation

#### `lateNight :: IO Bool`

Returns `True` if current hour is before 8 AM. Used to detect overnight work sessions where dates might need adjustment.

### Threshold Constants

#### `isNewWithinNDays :: Integer`

Number of days (62) content is considered "new". Controls the new-content highlighting.

#### `listLengthMaxN :: Int`

Maximum list length (75) before triggering special handling in Columns.hs.

#### `sectionizeMinN :: Int`

Minimum backlinks (3) before sectionization kicks in.

#### `mininumLinkBibliographyFragment :: Int`

Minimum links (3) before generating a link bibliography.

#### `minFileSizeWarning :: Int`

Megabyte threshold (17) for file size warnings. Below this, no warning shown.

#### `minimumAnnotationLength :: Int`

Character threshold (250) for an annotation to be considered "substantial".

### Filtering Functions

#### `backlinkBlackList :: T.Text -> Bool`

Returns `True` if a URL should be excluded from backlink generation.

Filters out:
- Internal meta-pages: `/backlink/`, `/link-bibliography/`, `/similar/`, `/private`
- Special prefixes: `$`, `#`, `!`, `mailto:`, `irc://`, bitcoin symbol, `/doc/www/`, `https://example.com`

**Called by:** generateBacklinks.hs

### Lookup Tables

#### `sectionizeWhiteList :: [T.Text]`

URLs exempt from normal sectionization rules (Danbooru dataset pages).

#### `fileExtensionToEnglish :: String -> String`

Maps file extensions to human-readable names:
- `.pdf` → "PDF"
- `.hs` → "Haskell"
- `.webm` → "WebM"
- `.xlsx` → "spreadsheet"

Returns empty string for unknown extensions.

**Called by:** File link tooltips, download descriptions

### Arxiv Abstract Cleanup

#### `cleanArxivAbstracts :: [(String, String)]`

Find/replace pairs for removing puffery from Arxiv abstracts. Targets overused "significant/significantly" with contextual replacements.

#### `arxivAbstractRegexps :: [(String, String)]`

Regex patterns for cleaning Arxiv LaTeX artifacts (citation commands, lambda symbols).

#### `arxivAbstractFixedRewrites :: [(String, String)]`

Fixed string replacements for common Arxiv formatting issues.

**Called by:** Arxiv annotation processing

### GTX Configuration

#### `gtxKeyValueKeyNames :: [String]`

Whitelist of valid keys for GTX file metadata fields:
`id`, `doi`, `title`, `description`, `created`, `modified`, `status`, `importance`, `confidence`, `css-extension`, `invert`, `backlink`, `placeholder`, `index`, `thumbnail`, `thumbnail-text`

**Called by:** GTX parser validation

### Test Data

#### `tooltipToMetadataTestcases :: [((String,String),(String,String,String))]`

Test cases for tooltip-to-metadata parsing. Each entry maps (URL, tooltip) → (title, author, date).

#### `cycleTestCases :: [([(Int, Int)], Bool)]`

Test cases for cycle detection in directed graphs. Used to validate redirect chain detection.

---

## Internal Architecture

The module has no complex internal state—it's essentially a flat collection of constants and pure functions. The only stateful aspect is the startup-time computation of date values via `unsafePerformIO`.

### Date Computation Pattern

```haskell
currentYear = unsafePerformIO $ do
    now <- getCurrentTime
    tz <- getCurrentTimeZone
    let localTime = utcToLocalTime tz now
    return $ (\(year,_,_) -> fromInteger year) . toGregorian . localDay $ localTime
```

This pattern repeats: get UTC time, convert to local timezone, extract needed component. The timezone conversion ensures dates match the author's perspective, not UTC.

### Extension Mapping Structure

`fileExtensionToEnglish` uses a simple association list lookup. Keys include the dot (e.g., ".pdf"), so callers must pass the extension with the dot; there is no dot-normalization.

---

## Key Patterns

**Pragmatic `unsafePerformIO`:** The module accepts the tradeoff of impure-under-the-hood constants for API ergonomics. Build runs are short enough that "current year" genuinely won't change mid-execution.

**Flexible Date Parsing:** The `toDay` function handles three date granularities by length-based dispatch, padding incomplete dates to YYYY-MM-DD format. This accommodates varied annotation date precision.

**Anchor Stripping:** `backlinkBlackList` strips URL fragments before blacklist checking, preventing anchor IDs from accidentally bypassing filters.

**Domain-Specific Cleanup:** The Arxiv abstract cleaning shows deep domain knowledge—"significant" is overused academic puffery, but "significant margin" should become "large margin" not just disappear.

---

## Configuration

Most values are hardcoded constants. Key tuning points:

| Constant | Value | Purpose |
|----------|-------|---------|
| `isNewWithinNDays` | 62 | "New" content window |
| `listLengthMaxN` | 75 | Column list threshold |
| `minFileSizeWarning` | 17 MB | Download warning threshold |
| `minimumAnnotationLength` | 250 chars | "Has annotation" threshold |

The `minFileSizeWarning` comment notes it should be bumped ~10%/year as bandwidth improves.

---

## Integration Points

**Imports from:** `Utils` (for `anyInfixT`, `anyPrefixT`)

**Used by virtually everything:** This module is a dependency of most other build modules due to its fundamental constants (`root`, `author`, date functions).

Key consumers:
- **generateBacklinks.hs** - Uses `backlinkBlackList`, `sectionizeWhiteList`
- **Columns.hs** - Uses `listLengthMaxN`
- **GTX.hs** - Uses `gtxKeyValueKeyNames` for validation
- **Arxiv annotation processing** - Uses abstract cleanup patterns
- **All file I/O** - Uses `root` path constant

---

## See Also

- [Hakyll.hs](/backend/hakyll-hs) - Site generator using `root`, `author`, and date constants
- [Typography.hs](/backend/typography-hs) - Uses date constants for formatting
- [Annotation.hs](/backend/annotation-hs) - Uses Arxiv cleanup patterns and `minimumAnnotationLength`
- [LinkMetadata.hs](/backend/link-metadata-hs) - Uses `userAgent`, annotation thresholds
- [Config.Tags](/backend/config-tags-hs) - Related configuration module for the tag system
- [Config.Paragraph](/backend/config-paragraph-hs) - Related configuration for paragraph splitting
- [Utils.hs](/backend/utils-hs) - Core utilities that consume these configuration values
