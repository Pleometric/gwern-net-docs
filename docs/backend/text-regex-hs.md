# Text.Regex

**Path:** `build/Text/Regex.hs` | **Language:** Haskell | **Lines:** ~203

Vendored regex compatibility module providing POSIX-style regular expressions via TDFA backend.

---

## Overview

Text.Regex is a vendored copy of the `regex-compat-tdfa` package, included directly in the gwern.net codebase to avoid Cabal dependency versioning conflicts. It provides a simple, high-level interface for regular expression matching and substitution using the TDFA (Tagged Deterministic Finite Automaton) regex engine.

The module offers POSIX "extended" regular expression syntax (similar to `egrep`) with configurable options for case sensitivity and multiline matching. It wraps the lower-level `Text.Regex.TDFA` and `Text.Regex.Base` modules into a convenient API used throughout the build system.

This vendored approach ensures stable regex behavior regardless of external package updates or version conflicts in the Haskell ecosystem.

---

## Public API

### mkRegex :: String -> Regex

Creates a compiled regular expression with default options (multiline mode enabled, case-sensitive).

```haskell
let pattern = mkRegex "^[A-Z][a-z]+ [0-9]{4}$"
-- Matches lines like "Smith 2020"
```

**Default options:**
- `newSyntax = True` (extended regex syntax)
- `multiline = True` (`^`/`$` match line boundaries)
- Case-sensitive matching

### mkRegexWithOpts :: String -> Bool -> Bool -> Regex

Creates a compiled regex with explicit multiline and case-sensitivity options.

```haskell
mkRegexWithOpts pattern singleLine caseSensitive
```

**Parameters:**
- `singleLine`: When `True`, `^` and `$` match individual line boundaries; `.` does not match newlines
- `caseSensitive`: When `True`, matching is case-sensitive

```haskell
-- Case-insensitive, single-line mode
let pattern = mkRegexWithOpts "[a-z]+" True False
```

### matchRegex :: Regex -> String -> Maybe [String]

Matches a regex against a string, returning captured subgroups.

```haskell
matchRegex (mkRegex "([A-Z]+) ([0-9]+)") "ABC 123"
-- → Just ["ABC", "123"]

matchRegex (mkRegex "foo") "bar"
-- → Nothing
```

### matchRegexAll :: Regex -> String -> Maybe (String, String, String, [String])

Full match information including context around the match.

```haskell
matchRegexAll (mkRegex "[0-9]+") "foo 123 bar"
-- → Just ("foo ", "123", " bar", [])
--         ^before ^match ^after ^subgroups
```

**Returns:** `Just (beforeMatch, matched, afterMatch, subgroupMatches)` or `Nothing`

### subRegex :: Regex -> String -> String -> String

Replaces all occurrences of a pattern with a replacement string.

```haskell
subRegex (mkRegex "[0-9]+") "a1b2c3" "X"
-- → "aXbXcX"
```

**Replacement syntax:**
- `\0` - entire match
- `\1`, `\2`, ... - captured subgroups
- `\\` - literal backslash

```haskell
subRegex (mkRegex "([a-z]+)([0-9]+)") "foo123" "\\2-\\1"
-- → "123-foo"
```

**Note:** Does not advance on empty matches (matches original `Text.Regex` behavior).

### splitRegex :: Regex -> String -> [String]

Splits a string on a delimiter pattern.

```haskell
splitRegex (mkRegex ",\\s*") "a, b,  c"
-- → ["a", "b", "c"]

splitRegex (mkRegex "::") "a::b::c"
-- → ["a", "b", "c"]
```

**Warning:** Produces infinite list if the regex matches empty strings.

---

## Why Vendored

The comment at the top of the file explains:

> GWERN.NET: VENDORED FROM regex-compat-tdfa-0.95.1.4 DUE TO CABAL DEPENDENCY VERSIONING PROBLEMS

Haskell's package ecosystem can have version conflicts when multiple packages require different versions of the same dependency. By vendoring this module, gwern.net:

1. Avoids dependency resolution failures during builds
2. Ensures consistent regex behavior across all build environments
3. Eliminates external package update surprises

---

## Usage in Codebase

This module is used throughout the Haskell build tools for:

- **Typography.hs**: Citation pattern matching (`Smith 2020`, `Smith & Jones 2020`)
- **LinkMetadata.hs**: URL and metadata parsing
- **Annotation modules**: Extracting structured data from scraped content
- **Various utilities**: String manipulation and validation

---

## Configuration

Listed in `gwernnet.cabal` as an exposed library module:

```cabal
exposed-modules:
    ...
    Text.Regex
    ...
```

Depends on:
- `regex-base` - Abstract regex interface
- `regex-tdfa` - TDFA implementation

---

## See Also

- [Typography.hs](/backend/typography-hs) - Uses regex for citation formatting
- [LinkMetadata.hs](/backend/link-metadata-hs) - URL pattern matching
- [gwernnet.cabal](/backend/gwernnet-cabal) - Build configuration listing this module
- [Utils.hs](/backend/utils-hs) - Other utility functions
