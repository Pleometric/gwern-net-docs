
# typography.js

**Path:** `js/typography.js` | **Language:** JavaScript | **Lines:** ~285

> Client-side text processing for smart quotes, dashes, ellipses, and word-break insertion

---

## Overview

Typography.js provides regex-based text transformations to improve typographic quality of rendered content. It converts ASCII approximations (straight quotes, hyphens, triple dots) into proper Unicode characters (curly quotes, em-dashes, ellipses) and inserts zero-width word-break opportunities after slashes in long URLs.

The module operates on both raw strings and DOM elements. When processing DOM elements, it decomposes them into text nodes, applies transformations while tracking segment boundaries, then reconstructs the nodes. This preserves element structure while allowing replacements to span across text node boundaries.

Based on Kelly Martin's smartquotes.js and kronusaturn's lw2-viewer, the implementation uses bitmask flags to select which replacement categories to apply. A special "CLEAN" mode strips invisible characters (soft hyphens, joiners, zero-width spaces) that may have been inserted during server-side processing.

---

## Public API

### `Typography.processString(str, replacementTypes, segments) → string`

Applies selected replacements to a plain string.

**Parameters:**
- `str` - Input text
- `replacementTypes` - Bitmask of `Typography.replacementTypes.*` flags (default: `NONE`)
- `segments` - Optional array of segment lengths for tracking offset changes

**Returns:** Transformed string

**Called by:** `rewrite.js` (image alt text), `transclude.js` (include link cleanup)
**Calls:** `Typography.replacements()`

---

### `Typography.processElement(element, replacementTypes, rectifyWordBreaks) → string|null`

Applies replacements to all text content within a DOM element.

**Parameters:**
- `element` - DOM element to process
- `replacementTypes` - Bitmask of replacement types (default: `NONE`)
- `rectifyWordBreaks` - Whether to convert zero-width spaces to `<wbr>` tags (default: `true`)

**Returns:** Processed text, or `null` if element is excluded

**Called by:** `rewrite.js` (content processing, TOC, code blocks), `image-focus.js` (image captions), `misc.js` (changelog entries)
**Calls:** `Typography.processString()`, `Typography.rectifyWordBreaks()`

---

### `Typography.replacements(types) → Array<[RegExp, string]>`

Returns the list of replacement definitions for the specified type bitmask.

**Parameters:**
- `types` - Bitmask combining `Typography.replacementTypes.*` constants

**Returns:** Array of `[pattern, replacement]` pairs

---

### `Typography.rectifyWordBreaks(element)`

Converts zero-width space characters (U+200B) and hair spaces (U+200A) in text nodes into proper `<wbr>` elements for browser-controlled line breaking.

**Called by:** `Typography.processElement()`

---

## Internal Architecture

### Replacement Type Flags

Bitmask constants for selecting transformation categories:

```javascript
Typography.replacementTypes = {
    NONE:        0x0000,  // No transformations
    QUOTES:      0x0001,  // Smart quotes and primes
    HYPHENS:     0x0002,  // Em/en dashes
    ELLIPSES:    0x0004,  // ... → …
    ARROWS:      0x0008,  // -> → →, => → ⇒
    WORDBREAKS:  0x0010,  // Insert zero-width spaces after /
    MISC:        0x0020,  // nbsp→space, double-space removal, minus sign
    SOFTHYPHENS: 0x0040,  // Strip soft hyphens
    JOINERS:     0x0080,  // Strip word joiners
    SEPARATORS:  0x0100,  // Strip zero-width/hair spaces
    SYMBOLS:     0x0200,  // Heavy asterisk → *
    CLEAN:       0x01C0   // SOFTHYPHENS | JOINERS | SEPARATORS
}
```

### Replacement Definition Groups

Each category maps to an array of `[RegExp, template]` pairs:

| Group | Example Transformation |
|-------|----------------------|
| `quotes` | `"hello"` → `"hello"`, `it's` → `it's` |
| `hyphens` | `word - word` → `word—word`, `1 - 2` → `1–2` |
| `ellipses` | `...` → `…` |
| `arrows` | `->` → `→`, `<=>` → `⇔` |
| `wordbreaks` | `/path` → `/​path` (zero-width space after slash) |
| `misc` | `- 5` → `−5` (minus sign) |
| `softHyphens` | Strip U+00AD |
| `joiners` | Strip U+2060 |
| `separators` | Strip U+200B, U+200A |
| `symbols` | `✱` → `*` |

### DOM Decomposition

`processElement()` walks the DOM tree, collecting all text nodes and their lengths:

```
Element
├── TextNode("Hello ")     → segments[0] = 6
├── <em>
│   └── TextNode("world")  → segments[1] = 5
└── TextNode("!")          → segments[2] = 1

Combined: "Hello world!" with segments = [6, 5, 1]
```

After processing the combined string, segment lengths are used to split it back into the original text nodes. This allows quote matching to work across element boundaries (e.g., `"<em>quoted</em> text"`).

---

## Key Patterns

### Lookbehind-Heavy Quote Detection

Smart quote insertion uses extensive lookbehind assertions to detect context:

```javascript
// Beginning " - after whitespace/punctuation, before non-punctuation
[ /(?<=[\s([]|^)"(?=[^\s?!.,;\/)])/, '\u201c' ]

// Ending " - after opening ", before end or next opening
[ /(?<=\u201c[^"]*)"(?=[^"]*$|[^\u201c"]*(?=\u201c))/, '\u201d' ]
```

This handles nested quotes, apostrophes in contractions (`it's`), and abbreviated years (`'93`).

### Segment Tracking

When replacements change string length, segment offsets must be updated:

```javascript
let segmentAtMatchStart = segmentIndexAtOffset(segments, match.index);
let segmentAtMatchEnd = segmentIndexAtOffset(segments, match.index + match[0].length - 1);
if (segmentAtMatchStart == segmentAtMatchEnd) {
    segments[segmentAtMatchStart] += lengthChange;
} else {
    //  TODO: THIS! (cross-segment replacements not yet handled)
}
```

### WBR Deduplication

After converting zero-width spaces to `<wbr>` tags, consecutive `<wbr>` elements are collapsed to prevent redundant break opportunities:

```javascript
// Remove all but one of each set of consecutive <wbr> tags
let prevNodeIsWBR = false;
for (let i = 0; i < element.childNodes.length; i++) {
    // ... removes consecutive WBRs and intervening empty text nodes
}
```

---

## Configuration

### Excluded Tags

Elements that are never processed:

```javascript
Typography.excludedTags = [ 'PRE', 'SCRIPT', 'STYLE', 'NOSCRIPT' ]
```

### Unmodified Tags

Elements where text content is preserved but word-break rectification still applies:

```javascript
Typography.unmodifiedTags = [ 'CODE ' ]  // Note: trailing space (likely bug)
```

---

## Integration Points

### Usage in rewrite.js

The primary consumer, applying typography fixes during content rewriting:

```javascript
// Main content: quotes, word breaks, ellipses
Typography.processElement(eventInfo.container,
    Typography.replacementTypes.QUOTES
    | Typography.replacementTypes.WORDBREAKS
    | Typography.replacementTypes.ELLIPSES);

// Image alt text: just quotes
image.alt = Typography.processString(image.alt, Typography.replacementTypes.QUOTES);

// Code blocks: just word-break rectification
Typography.processElement(block, Typography.replacementTypes.NONE, true);
```

### Usage in transclude.js

Cleans transclusion markers:

```javascript
let cleanedNodeContents = Typography.processString(
    includeLink.nextSibling.textContent,
    Typography.replacementTypes.CLEAN
);
```

### Usage in image-focus.js

Processes image captions in the lightbox:

```javascript
Typography.processElement(element,
    Typography.replacementTypes.CLEAN | Typography.replacementTypes.QUOTES);
```

### Global Object

The `Typography` object is defined at global scope (no namespace wrapper), making it available to all other scripts.

---

## See Also

- [Typography.hs](/backend/typography-hs) - Server-side Haskell typography (Pandoc AST transforms)
- [Config.Typography](/backend/config-typography-hs) - Server-side typography configuration
- [rewrite.js](/frontend/rewrite-js) - Primary consumer for content transformations
- [initial.js](/frontend/initial-js) - Core framework that loads typography module
- [transclude.js](/frontend/transclude-js) - Uses CLEAN mode for include markers
- [image-focus.js](/frontend/image-focus-js) - Uses typography for caption processing
- [sidenotes.js](/frontend/sidenotes-js) - Related text layout system
