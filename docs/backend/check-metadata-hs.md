
# checkMetadata.hs

**Path:** `build/checkMetadata.hs` | **Language:** Haskell | **Lines:** ~6

> Standalone executable for validating annotation metadata databases

---

## Overview

`checkMetadata.hs` is a minimal entry point that invokes the comprehensive metadata validation system. It exists as a standalone executable so the build system can validate annotation databases (`*.gtx` files) as an independent step, catching errors before expensive build operations proceed.

The actual validation logic lives entirely in `LinkMetadata.readLinkMetadataAndCheck`. This module is purely a thin wrapper that calls that function and discards its return value (the validated `Metadata` map), caring only about whether it completes without error.

This separation allows the same validation to be run: (1) as a standalone `checkMetadata` executable during CI/development, (2) implicitly during `walkAndUpdateLinkMetadata` operations, and (3) after batch update operations like `updateGwernEntries`.

---

## Public API

### `main :: IO ()`

Entry point that runs all metadata validation checks.

```haskell
main :: IO ()
main = readLinkMetadataAndCheck >> return ()
```

**Called by:** Build system, CI, manual invocation
**Calls:** `LinkMetadata.readLinkMetadataAndCheck`

---

## Internal Architecture

The module has no internal architecture—it's a single-line wrapper. All complexity lives in `LinkMetadata.readLinkMetadataAndCheck`.

### Validation Performed by `readLinkMetadataAndCheck`

The delegated function performs extensive checks across four GTX databases (`me.gtx`, `full.gtx`, `half.gtx`, `auto.gtx`):

**URL/Path Validation:**
- URLs must be non-empty and start with `h` (http/https), `/` (local), `mailto:`, `irc://`, or `rsync://`
- No double slashes in local paths (`//` disallowed)
- No spaces or em-dashes in URLs
- No trailing `#`, `?`, or `&` (except local pages with range includes)
- Local file references must exist on disk (checked for `me.gtx`/`full.gtx`/`half.gtx`)
- Normalized URLs (http/https stripped) must be unique in `me.gtx`/`full.gtx`

**Required Field Validation:**
- Title, author, and abstract must be non-empty in `me.gtx`/`full.gtx`
- Title must not begin/end with suspicious characters (`<\;,_~=-({}:`)
- Abstracts must be unique (no copy-paste duplicates)

**DOI Validation:**
- Must contain exactly one `/` (DOI spec requirement)
- Must contain at least one digit
- No invalid punctuation (en-dash, em-dash, space, braces, etc.)
- Cannot be a date, tag, or URL

**Date Validation:**
- Format must be `YYYY`, `YYYY-MM`, or `YYYY-MM-DD`
- Year cannot be more than 2 years in the future

**Author Validation:**
- Cannot start with digit or punctuation (unless whitelisted)
- No suspicious characters (`;`, `&`, `?`, `!`, `>`, `<`)
- Cannot end in punctuation (except `.`)

**Tag Validation:**
- Every tag must correspond to an existing `doc/$TAG/` directory
- Tags should not be more specific than the file's path

**Abstract/HTML Validation:**
- Double quotes must be balanced (even count)
- Brackets must be balanced (`()`, `[]`, `{}`, `<>`)
- See-Also sections with 1-2 items shouldn't use `.columns` class

**Structural Validation:**
- No redundant entries between `half.gtx` and `me.gtx`/`full.gtx`
- Key-value pairs must use whitelisted keys from `Config.Misc.gtxKeyValueKeyNames`
- Manual IDs cannot start with underscore (reserved for hash-IDs)
- Link IDs must be unique (reports disambiguation needs)

---

## Key Patterns

### Fail-Fast Error Reporting

Hard errors use `error` to halt immediately:
```haskell
when (condition) $ error $ "Description: " ++ show badData
```

Soft warnings use colored output but allow continuation:
```haskell
unless (null warnings) $ printRed "Warning:" >> printGreen (show warnings)
```

### Database Hierarchy

Validation respects the merge hierarchy: `me.gtx > full.gtx > half.gtx > auto.gtx`. Stricter checks apply to hand-curated databases (`me.gtx`, `full.gtx`) than auto-generated ones.

---

## Configuration

- **`Config.Misc.gtxKeyValueKeyNames`**: Whitelist of valid key names in key-value pairs
- **`Config.Metadata.Author.authorWhitelist`**: Author names that bypass odd-character checks
- **`Config.currentYear`**: Used to validate dates aren't too far in the future

---

## Integration Points

**Build System:**
- Called as standalone executable during build validation phase
- Exit code 0 = all checks pass, non-zero = validation failure

**Related Operations:**
- `walkAndUpdateLinkMetadata`: Calls `readLinkMetadataAndCheck` after bulk updates
- `updateGwernEntries`: Calls validation after re-scraping gwern.net entries
- `rescrapeGTX`: Individual rescrape operations

**File Dependencies:**
- `metadata/me.gtx`
- `metadata/full.gtx`
- `metadata/half.gtx`
- `metadata/auto.gtx`
- `doc/*/` directories (for tag validation)

---

## See Also

- [LinkMetadata.hs](/backend/link-metadata-hs) - Contains the `readLinkMetadataAndCheck` implementation
- [GTX.hs](/backend/gtx-hs) - GTX file format parser for annotation databases
- [Annotation.hs](/backend/annotation-hs) - Creates annotation entries that this module validates
- [sync.sh](/backend/sync-sh) - Build orchestrator that runs metadata validation
- [Test.hs](/backend/test-hs) - Test suite that includes metadata validation
