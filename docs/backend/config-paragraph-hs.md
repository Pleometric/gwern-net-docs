
# Config.Paragraph

**Path:** `build/Config/Paragraph.hs` | **Language:** Haskell | **Lines:** ~29

> Configuration constants for GPT-4o paragraph-splitting behavior

---

## Overview

Config.Paragraph is a minimal configuration module that controls how the build system decides which documents are eligible for GPT-4o–assisted paragraph splitting. The paragraph splitter uses a language model to improve text readability by inserting appropriate line breaks.

The module exports two values: a minimum character length threshold (`minLength`) that documents must exceed to be considered for splitting, and a `whitelist` of specific URLs that should always be processed regardless of other criteria. This separation of configuration from logic follows the gwern.net pattern of isolating magic numbers and test fixtures into dedicated Config modules.

---

## Public API

### `minLength :: Int`

The minimum character count a document must have to be considered for paragraph splitting. Set to **768 characters**.

**Called by:** Paragraph splitting logic in the build pipeline
**Calls:** (pure constant)

### `whitelist :: [String]`

A curated list of URLs (both local paths and external URLs) that should always be processed by the paragraph splitter. Contains approximately 40 entries covering PDFs, arxiv papers, NCBI articles, and web pages.

**Called by:** Paragraph splitting logic for whitelist checks
**Calls:** (pure constant)

---

## Internal Architecture

This module has no internal architecture—it consists entirely of two exported constants. The `whitelist` is a flat list of strings with no categorization or structure.

---

## Key Patterns

**Test fixture as configuration:** The whitelist serves dual purposes: it ensures specific important documents always get paragraph splitting applied, and it provides a diverse test corpus for the GPT-4o splitting feature. The comment "testing: unique list, all URLs" explicitly notes this.

**Mixed URL formats:** The whitelist includes both local paths (starting with `/doc/`) and external URLs (https://). This suggests the paragraph splitter operates on content fetched from either location.

**Deliberately heterogeneous corpus:** The whitelist spans many domains and content types—academic PDFs, blog posts, Reddit, government docs, arxiv preprints—likely to ensure the paragraph splitter handles diverse formatting gracefully.

---

## Configuration

| Constant | Value | Purpose |
|----------|-------|---------|
| `minLength` | 768 | Skip short documents that don't benefit from splitting |
| `whitelist` | ~40 URLs | Always process these regardless of length |

The values are hardcoded. To change them, edit this file directly.

---

## Integration Points

- **Paragraph.hs:** The main paragraph-splitting module imports this config to decide what to process
- **Build pipeline:** Indirectly affects which documents get GPT-4o API calls during builds, impacting both build time and API costs

---

## See Also

- [Paragraph.hs](/backend/paragraph-hs) - Main paragraph-splitting module that consumes this configuration
- [Typography.hs](/backend/typography-hs) - Related text transformation module
- [Config.Misc](/backend/config-misc-hs) - Related configuration module with global constants
- [Hakyll.hs](/backend/hakyll-hs) - Site generator integrating paragraph processing
- [sync.sh](/backend/sync-sh) - Build orchestration that invokes paragraph splitting
- [Utils.hs](/backend/utils-hs) - Core utilities used alongside paragraph processing
