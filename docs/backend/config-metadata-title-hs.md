
# Config.Metadata.Title

**Path:** `build/Config/Metadata/Title.hs` | **Language:** Haskell | **Lines:** ~186

> Configuration lists for filtering and cleaning HTML page titles

---

## Overview

This module contains configuration data for the title-cleaning pipeline. When gwern.net scrapes metadata from external URLs (for annotations, link previews, etc.), the raw `<title>` elements often contain junk: navigation boilerplate, error messages, site branding suffixes, or malformed Unicode. This module defines the filter lists that identify and remove such noise.

The data here feeds into the broader title-cleaning system, which includes both Haskell processing (via `Utils.deleteMixedMany` and similar) and Python machine-learning cleanup (`/static/build/title-cleaner.py`). The configuration is intentionally kept in a pure-data Haskell module so it can be easily extended without touching processing logic.

Design philosophy: be aggressive about false positives. A slightly over-cleaned title (losing a suffix like "- YouTube") is preferable to displaying "404 Not Found" or "CAPTCHA" as an annotation title.

---

## Public API

### `separators :: String`

Characters used as title/site-name delimiters: `"—·|"`

Used to split titles at brand suffixes (e.g., "Article Title | Site Name").

**Called by:** Title processing in [LinkMetadata](link-metadata-hs)
**Calls:** None (pure data)

---

### `badStringPatterns :: [String]`

Substrings that indicate an unusable title when present anywhere:

```haskell
["Redirecting to ", "404 ", "Page Unavailable", "Page not found",
 "Page Not Found", "CAPTCHA", "s shortform feed", "Item not found"]
```

Titles containing any of these are rejected entirely.

**Called by:** Title validation logic
**Calls:** None

---

### `badStrings :: [String]`

Exact-match blocklist (~290 entries). Titles matching any string exactly are rejected.

Categories include:
- **Error pages:** "404 Not Found", "403 Forbidden", "500 Internal Server Error"
- **Generic/placeholder:** "Home", "Search", "Untitled", "Welcome"
- **Paywalls/challenges:** "CAPTCHA", "Are you a robot?", "Access Denied"
- **Site boilerplate:** Long navigation dumps from Quanta, Slate, Archive.org
- **Domain names used as titles:** "nytimes.com", "wsj.com", "x.com"
- **Personal sites with uninformative titles:** "niplav", "ribbonfarm", "Dario Amodei"

**Called by:** Title validation (exact match)
**Calls:** None

---

### `stringReplace :: [(String, String)]`

Character encoding fixes (6 entries). Repairs common mojibake:

```haskell
[("  ", " "),           -- double space → single
 (" � ", " - "),        -- replacement char → dash
 ("Â°", "°"),           -- UTF-8/Latin-1 collision
 ("Â ", " "),           -- ditto
 (" â\200\224 ", "—"),  -- em-dash encoding
 ("\128\200\231", "'")  -- curly quote encoding
]
```

**Called by:** Title normalization
**Calls:** None

---

### `stringDelete :: [String]`

Suffix/prefix patterns to strip (~180 entries). Uses leading/trailing space to determine deletion direction via `Utils.deleteMixedMany`:

- Trailing space → prefix deletion: `"The Technium: "`, `"GitHub - "`
- Leading space → suffix deletion: `" - YouTube"`, `" - Wikipedia"`

Categories include:
- **Site branding:** " - LessWrong", " - Marginal REVOLUTION", " - Astral Codex Ten"
- **Navigation junk:** Long Archive.org/Stripe/DEV Community boilerplate
- **Publisher suffixes:** " - The New York Times", " - IEEE Spectrum"
- **Blog prefixes:** "Abandoned Footnotes: ", "The Splintered Mind: "

**Called by:** `Utils.deleteMixedMany`
**Calls:** None

---

## Internal Architecture

This module is pure configuration—no functions, just data declarations. The structure is:

```
separators      : String      (4 chars)
badStringPatterns: [String]   (8 patterns)
badStrings      : [String]    (~290 exact matches)
stringReplace   : [(String,String)] (6 pairs)
stringDelete    : [String]    (~180 patterns)
```

All lists are tested for uniqueness per the inline comments. The `badStrings` list is not unit-tested because entries are often used as few-shot examples in the Python ML cleaner.

---

## Key Patterns

### Encoding-aware cleanup

The `stringReplace` list handles common UTF-8/Latin-1 collision artifacts. These appear when pages serve UTF-8 content but declare Latin-1, or vice versa. The patterns like `"Â°"` → `"°"` fix degree symbols that were double-encoded.

### Directional deletion via whitespace

The `stringDelete` entries cleverly encode deletion direction:
- `" - YouTube"` (leading space) → suffix, delete from end
- `"GitHub - "` (trailing space) → prefix, delete from start

This is processed by `Utils.deleteMixedMany` which inspects whitespace position.

### Navigation dump detection

Several entries are massive multi-line strings containing entire navigation menus scraped as "titles". For example, the Quanta Magazine entry is ~400 characters of "Homepage\nSaved articles\nLogin\nSearch...". These represent pathological scraping failures.

---

## Configuration

This *is* the configuration. To add new filters:

1. **Exact bad titles:** Add to `badStrings`
2. **Partial patterns (any position):** Add to `badStringPatterns`
3. **Site branding to strip:** Add to `stringDelete` with appropriate leading/trailing space
4. **Encoding fixes:** Add to `stringReplace`

After changes, rebuild and optionally re-run the annotation scraper on affected URLs.

---

## Integration Points

### Upstream consumers

- **[LinkMetadata](link-metadata-hs):** Main consumer, applies filters during annotation creation
- **`/static/build/title-cleaner.py`:** Python ML model uses `badStrings` as few-shot examples

### Utils dependency

Relies on `Utils.deleteMixedMany` for the directional deletion logic in `stringDelete`.

---

## See Also

- [Metadata/Title.hs](/backend/metadata-title-hs) - Processing logic that consumes this config
- [title-cleaner.py](/python/title-cleaner) - LLM-based title cleanup (uses badStrings as examples)
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database manager
- [Annotation.hs](/backend/annotation-hs) - URL-to-scraper dispatcher
- [Config/Metadata/Author.hs](/backend/config-metadata-author-hs) - Companion author configuration
- [Config/Metadata/Format.hs](/backend/config-metadata-format-hs) - Companion format configuration
