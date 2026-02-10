
# Test.hs

**Path:** `build/Test.hs` | **Language:** Haskell | **Lines:** ~235

> Centralized test suite validating configuration files, metadata databases, and module unit tests

---

## Overview

Test.hs is the unified test harness for the gwern.net Haskell backend. Rather than scattering tests across individual modules (which would incur per-call overhead and risk quadratic behavior when validating lists), this module consolidates all validation into a single `testAll` entry point that runs during the build process.

The test suite validates three categories of concerns: (1) configuration file integrity—checking for duplicates, valid URLs, proper formats, and absence of cycles in rewrite databases; (2) regex pattern validity—ensuring all regex strings compile without errors; and (3) module-level unit tests—verifying that individual functions like `cleanAbstractsHTML`, `titleCase`, and `tooltipToMetadata` produce expected outputs. The design philosophy prioritizes catching configuration errors early, since many configs are hand-edited and prone to typos, duplicates, or format mistakes.

Test.hs imports from virtually every other module in the codebase, making it both a validation layer and an implicit documentation of cross-module dependencies. When tests fail, they print red error messages identifying exactly which config or test case failed; when they pass, they print green status messages with counts of verified items.

---

## Public API

### `testAll :: IO ()`

Main entry point that runs all tests sequentially. Prints colored output indicating pass/fail status.

**Called by:** `hakyll.hs` (build system), manual invocation
**Calls:** Every test function in the module, plus functions from 20+ other modules

### `testConfigs :: Int`

Pure function that validates all configuration lists for uniqueness and format correctness. Returns count of errors (0 = all pass).

**Called by:** `testAll`
**Calls:** `isUniqueList`, `isUniqueKeys`, `isUniqueAll`, `isCycleLess`, `ensure`, `isDomainT`, `isURL*` family

### `testXOTD :: IO Int`

Validates the "X of the Day" quote and site databases for format correctness and uniqueness.

**Called by:** `testAll`
**Calls:** `XOTD.readTTDB`, `isUniqueKeys3`, `ensure`

### `testRegexPatterns :: [String] -> IO ()`

Attempts to compile each regex pattern, printing errors for any that fail.

**Called by:** `testAll`
**Calls:** `makeRegexM`

---

## Internal Architecture

### Test Organization

```
testAll
├── Link Icon Tests (linkIconTest)
├── Interwiki Tests (interwikiTestSuite, interwikiCycleTestSuite)
├── Config Validation (testConfigs)
│   ├── Uniqueness checks (isUniqueList, isUniqueKeys, isUniqueAll)
│   ├── Format validation (ensure with predicates)
│   └── Cycle detection (isCycleLess)
├── Regex Validation (testRegexPatterns)
├── Metadata Database Tests
│   ├── readLinkMetadata
│   ├── readArchiveMetadataAndCheck
│   └── URL hash uniqueness
├── Unit Test Suites
│   ├── cleanAbstractsHTML
│   ├── titleCase
│   ├── authorCollapse
│   ├── tooltipToMetadata
│   └── many more...
└── Live Tests (Wikipedia API calls)
```

### Validation Helpers from `Unique` Module

- **`isUniqueList`**: Checks a list has no duplicate elements
- **`isUniqueKeys`**: Checks a list of tuples has unique first elements
- **`isUniqueAll`**: Checks both keys and values are unique
- **`isUniqueKeys3`/`isUniqueKeys4`**: For 3-tuples and 4-tuples

### The `ensure` Pattern

```haskell
ensure :: String -> String -> (a -> Bool) -> [a] -> [a]
```

Filters a list to items failing a predicate, returning failures. Used extensively:

```haskell
ensure "Test.linkIDOverrides" "isURLAny" isURLAny Config.LinkID.linkIDOverrides
```

Returns empty list if all items pass; returns failing items otherwise.

---

## Key Patterns

### Centralized Config Testing

The design deliberately avoids testing configs at point-of-use:

```haskell
-- we prefer to test configs in a single centralized place, as inconvenient
-- as that is, because if we simply test inside the function itself on every
-- call, we incur overhead and we risk accidentally-quadratic behavior
```

This means adding a new config requires manually adding its test to `testConfigs`.

### Colored Output Protocol

```haskell
printGreen ("Testing link icon matches…" :: String)
unless (null linkIconTest) $ printRed ("Link icon rules have errors in: " ++ show linkIconTest)
```

Green messages indicate test categories being run; red indicates failures. Success is silent beyond the category announcement.

### Cycle Detection in Rewrite Databases

Several configs (author canonicalization, tag rewrites, interwiki redirects) could create infinite loops if entries reference each other cyclically:

```haskell
length $ isCycleLess (M.toList Config.Metadata.Author.canonicals)
length $ isCycleLess Config.Tags.tagsShort2LongRewrites
```

### Live API Tests

The Wikipedia tests actually hit the Wikipedia API:

```haskell
a <- Interwiki.isWPArticle False "https://en.wikipedia.org/wiki/George_Washington_XYZ"
b <- Interwiki.isWPArticle False "https://en.wikipedia.org/wiki/George_Washington"
c <- Interwiki.isWPDisambig "Mercury"
```

These verify that article-existence and disambiguation checking works correctly.

---

## Configuration

Test.hs itself has no configuration. It tests configs from these modules:

| Config Module | What's Tested |
|--------------|---------------|
| `Config.GenerateSimilar` | URL blacklists are valid URLs |
| `Config.Interwiki` | Rewrites, redirects, quote overrides |
| `Config.LinkArchive` | Whitelist matches, localization test cases |
| `Config.LinkIcon` | Icon rules, blacklists |
| `Config.LinkLive` | Domain whitelists/blacklists |
| `Config.LinkSuggester` | Bad anchor strings, whitelists |
| `Config.Tags` | Tag mappings, rewrites |
| `Config.Typography` | Title case rules, date ranges |
| `Config.XOfTheDay` | Site/quote databases |
| `Config.Inflation` | Bitcoin exchange rates, test cases |
| `Config.LinkAuto` | Custom link rewrites |
| `Config.LinkID` | ID overrides, affiliation anchors |
| `Config.Metadata.*` | Author, Format, Title configs |
| `Config.Misc` | Tooltips, cycles, arxiv abstracts |
| `Config.Paragraph` | Paragraph whitelist |

---

## Integration Points

### Imports (Tests From)

Test.hs imports test functions from these modules:

- `Annotation.tooltipToMetadata`, `testGuessAuthorDate`
- `Cycle.testCycleDetection`
- `Inflation.inflationDollarTestSuite`
- `Interwiki.interwikiTestSuite`, `interwikiCycleTestSuite`
- `LinkArchive.testLinkRewrites`
- `LinkAuto.linkAutoTest`
- `LinkIcon.linkIconTest`
- `LinkLive.linkLiveTest`, `linkLivePrioritize`
- `Tags.testTags`
- `Typography.titleCaseTest`
- `LinkMetadata.fileTranscludesTest`
- `Metadata.Author.authorCollapseTest`, `cleanAuthorsTest`
- `Metadata.Format.printDoubleTestSuite`, `cleanAbstractsHTMLTest`
- `Metadata.Date.dateRangeDurationTestCasesTestsuite`

### Working Directory

```haskell
testAll = do Config.Misc.cd  -- Changes to project root
```

Tests assume they run from the project root directory.

### Exit Behavior

Tests don't exit on failure—they print errors and continue. The build system must check output or return codes separately.

---

## What Is Tested

### Configuration Integrity

1. **Uniqueness**: No duplicate entries in lists, no duplicate keys in maps
2. **URL Validity**: URLs are actually URLs (`isURL`, `isURLT`, `isURLAny`)
3. **Domain Validity**: Domain strings are valid domains (`isDomainT`)
4. **Cycle Freedom**: Rewrite chains don't loop infinitely
5. **Format Correctness**: HTML identifiers start with letters, dates are valid dates

### Regex Validity

All regex patterns from configs are compiled to catch syntax errors:
- `footnoteRegex`, `sectionAnonymousRegex`, `badUrlRegex`
- Tag rewrite regexes
- Author cleanup regexes
- HTML rewrite regexes
- Arxiv abstract regexes
- LinkAuto custom patterns

### Unit Test Suites

| Function | Config Source |
|----------|--------------|
| `balanced` | `balancedBracketTestCases` |
| `cleanAbstractsHTML` | `htmlRewriteTestCases` |
| `extractTwitterUsername` | `extractTwitterUsernameTestSuite` |
| `tooltipToMetadata` | `tooltipToMetadataTestcases` |
| `titleCase` | `titleCaseTestCases` |
| `authorCollapse` | `authorCollapseTestCases` |
| `fileTranscludes` | Generated from metadata |

### Database Integrity

- **Link Metadata**: URL hashes are unique, IDs are valid
- **Archive Metadata**: Checked via `readArchiveMetadataAndCheck`
- **X-of-the-Day**: Quote/site DBs have valid format

---

## See Also

- [hakyll.hs](/backend/hakyll-hs) - Build system that invokes tests during SLOW builds
- [Utils.hs](/backend/utils-hs) - Utility functions with unit tests
- [sync.sh](/backend/sync-sh) - Build orchestrator controlling test execution
- [LinkMetadata.hs](/backend/link-metadata-hs) - Metadata database being validated
- [Typography.hs](/backend/typography-hs) - Typography functions with unit tests
- [Cycle.hs](/backend/cycle-hs) - Cycle detection tested here
