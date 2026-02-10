---
sidebar_position: 8
---

# font_spec.php

**Path:** `font/font_spec.php` | **Language:** PHP | **Lines:** ~168

Font specification file for generating CSS @font-face declarations.

## Overview

`font_spec.php` is a hybrid PHP/data file that defines the web font stack for gwern.net. It uses a custom Domain-Specific Language (DSL) to declare font families, variants, and loading parameters, which are then parsed by `build_font_css.php` to generate the actual CSS @font-face rules.

The file serves as the single source of truth for all fonts used across the site, including body text fonts (Source Serif 4), UI fonts (Source Sans 3), code fonts (IBM Plex Mono), poetry fonts (Nimbus Mono), decorative dropcap fonts (5 varieties), and symbol fonts (Quivira, Noto Emoji). The specification uses a concise DSL that allows font declarations with minimal boilerplate while maintaining precise control over unicode-range subsetting, weight/style variants, and loading strategies.

The inline PHP code within the file (the `all_the_letters()` function) generates per-letter subsets for dropcap fonts, enabling the site to load only the single initial letter needed (8-16KB) rather than the entire font file (200-700KB), a significant performance optimization.

## Key Font Families

### Body Text
- **Source Serif 4**: Primary workhorse font for body text, with 6 weights (200-900) and italic variants
  - Uses `font-display: swap` for immediate text visibility
  - Subsetted to BASIC unicode range covering Latin, Greek, and common symbols
  - Separate inline variants for critical rendering path

### UI Text
- **Source Sans 3**: Used for Table of Contents and UI elements, primarily for Mac/iOS users
  - 7 weights (200-900) with italic variants
  - Extended unicode-range including UI symbols (⌘, ⌥, etc.)

### Code
- **IBM Plex Mono**: Modern monospace for code blocks
  - Regular and Bold weights with italics
- **Nimbus Mono**: Monospace for poetry (enables enjambment alignment)
  - Includes custom private-use-area glyphs (U+EA11-EA6A)

### Dropcaps (Initial Capitals)
Five decorative fonts, each subset into 26 individual files (A-Z):
- **Deutsche Zierschrift**: General pages (default)
- **Yinit**: Technical/scientific pages
- **Goudy Initialen**: Humanities/literature/history pages
- **Cheshire Initials**: Alternate decorative style
- **Kanzlei Initialen**: Gothic/blackletter style

### Symbols
- **Quivira**: Specific Unicode symbols (subsetted)
- **Noto Emoji**: Emoji and Unicode linkicons (subsetted)
- **Great Primer Uncials**: Additional blackletter characters

## DSL Syntax

The custom DSL uses a line-based format parsed by `build_font_css.php`:

```
Font Family Name
/path/to/font/files-
extension
property    value
`weight     filename-component
```

### Key Directives

- **`!inline`**: Marks font as critical for inline inclusion in HTML `<head>`
- **`italic /$/`**: Pattern replacement for italic variant filenames (replaces `$` with "Italic")
- **`unicode-range`**: CSS unicode-range descriptor for subsetting
- **`font-display`**: CSS font-display descriptor (always set to `swap`)
- **Backtick prefix (`` ` ``)**: Indicates a font weight/style variant
  - `` `400 Regular`` → font-weight: 400, filename includes "Regular"
  - `` `bold Bold`` → font-weight: bold, filename includes "Bold"

### Example Entry

```
Source Serif 4
/static/font/ssf4/SourceSerif4-Text-BASIC-
otf
font-display    swap
italic          /$/     Italic
unicode-range   U+0020-007E, U+00A0-00FF, ...
`400    Regular
`600    Semibold
`700    Bold
```

This generates @font-face rules for:
- SourceSerif4-Text-BASIC-Regular.otf (weight 400, normal)
- SourceSerif4-Text-BASIC-Italic.otf (weight 400, italic)
- SourceSerif4-Text-BASIC-Semibold.otf (weight 600, normal)
- etc.

## PHP Helper Functions

### all_the_letters()

```php
function all_the_letters() {
    for ($c = 0x41; $c <= 0x5A; $c++) {
        echo "`  ". chr($c) . "\n";
        echo "  unicode-range  U+00" . strtoupper(dechex($c)) . "\n";
    }
}
```

Generates 26 font variant declarations for dropcap fonts, one per letter A-Z. Each variant includes a `unicode-range` limited to that single character, enabling per-letter loading.

**Output example:**
```
`   A
    unicode-range   U+0041
`   B
    unicode-range   U+0042
```

This allows the CSS to load only the dropcap font for the actual initial letter present on the page, rather than all 26 letters.

## Font Loading Strategy

The specification implements a performance-optimized font loading strategy:

1. **Critical fonts marked `!inline`**: Included directly in HTML for fastest rendering
2. **`font-display: swap`**: Shows system fallback immediately, swaps to webfont when loaded
3. **Unicode-range subsetting**: Browser loads only needed character ranges
4. **Per-letter dropcaps**: Loads only the single initial letter needed (8-16KB vs 200-700KB)

### Fallback Fonts

System fallback fonts are defined in `/static/css/initial.css` (referenced in comments):
- Body text fallback: Baskerville (common system serif)
- These display instantly while webfonts load

## Usage

This file is consumed by:
- **`/static/build/build_font_css.php`**: Parses the DSL and generates CSS
- **`/static/build/build_versioned_font_css.php`**: Creates versioned/cached font CSS

The build process:
1. Parse `font_spec.php` DSL
2. Execute embedded PHP (`all_the_letters()`)
3. Generate @font-face declarations
4. Separate inline-critical fonts from async-loaded fonts
5. Output CSS with content-addressed hashes for cache busting

---

## See Also

- [build_font_css.php](/php/build-font-css) - DSL parser and CSS generator that consumes this file
- [build_versioned_font_css.php](/php/build-versioned-font-css) - Versioned font CSS builder
- [build_unified_assets.php](/php/build_unified_assets) - Bundles generated font CSS into stylesheets
- [build_paths.php](/php/build_paths) - Directory path constants for font file locations
- [initial.css](/css/initial) - Defines system font fallbacks
- [sync.sh](/backend/sync-sh) - Build orchestrator that triggers font CSS generation
