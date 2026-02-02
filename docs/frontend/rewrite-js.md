
# rewrite.js

**Path:** `js/rewrite.js` | **Language:** JavaScript | **Lines:** ~4,461

> DOM transformation pipeline—registers dozens of handlers that run on content load/inject to enhance raw HTML

---

## Overview

`rewrite.js` is the largest JavaScript file in gwern.net and serves as the central DOM transformation pipeline. It contains approximately 80+ handlers that register themselves via `addContentLoadHandler()` and `addContentInjectHandler()` from initial.js. When content is loaded (from the server, via transclusion, or from annotations), these handlers fire in sequence to transform raw HTML into the enhanced gwern.net reading experience.

The handlers are organized around content categories: images and figures, tables, code blocks, footnotes, links, lists, blockquotes, poetry, annotations, typography, and more. Each handler typically queries for specific selectors within the injected container, then applies transformations—wrapping elements, adding classes, setting CSS properties, binding event listeners, or restructuring the DOM.

This architecture separates concerns cleanly: content.js and transclude.js handle fetching and templating content, while rewrite.js focuses purely on transformation. The notification center's phase system ensures handlers run in the correct order (e.g., structural changes before event listener attachment).

---

## Handler Registration System

Handlers are registered via two convenience functions defined in initial.js:

### `addContentLoadHandler(handler, phase, condition, once)`

Registers a handler for `GW.contentDidLoad` events. These fire when content is loaded from any source but before injection into the document.

### `addContentInjectHandler(handler, phase, condition, once)`

Registers a handler for `GW.contentDidInject` events. These fire after content has been injected into a document (main page or pop-frame).

**Parameters:**
- `handler` — Function receiving an `eventInfo` object with:
  - `container` — The DOM element containing the loaded/injected content
  - `document` — The document (main or pop-frame) the content belongs to
  - `source` — String identifying the content origin (e.g., "transclude", "Annotation.load")
  - `contentType` — String like "annotation", "localPage", "tweet", etc.
  - `loadLocation` — URL of the loaded content
  - `fullWidthPossible` — Boolean flag for layout context
- `phase` — Execution order hint (see Phase System below)
- `condition` — Optional predicate function for selective execution
- `once` — If true, handler auto-removes after first invocation

---

## Phase System

Handlers run in a controlled order based on their declared phase. The phases for each event type are:

**`GW.contentDidLoad`:** `"transclude"` → `"rewrite"`

**`GW.contentDidInject`:** `"rewrite"` → `"eventListeners"`

Within a phase, you can fine-tune order with prefixes:
- `"<rewrite"` — Run before main rewrite phase
- `">rewrite"` — Run after main rewrite phase

```javascript
// Runs before most rewrite handlers
addContentLoadHandler(GW.contentLoadHandlers.myEarlyHandler = (eventInfo) => {
    // ...
}, "<rewrite");

// Runs after most rewrite handlers
addContentInjectHandler(GW.contentInjectHandlers.myLateHandler = (eventInfo) => {
    // ...
}, ">rewrite");
```

The `"eventListeners"` phase is used for binding mouse/keyboard events—always done last since DOM structure must be finalized.

---

## Handler Categories

### Images & Figures (~500 lines)

- **`wrapImages`** — Wraps bare `<img>` elements in `<figure>` tags
- **`wrapFigures`** — Adds inner structure: `.figure-outer-wrapper`, `.image-wrapper`, `.caption-wrapper`
- **`setMediaElementDimensions`** — Applies width/height from HTML attributes to CSS
- **`applyImageInversionAndOutliningJudgments`** — Applies dark-mode inversion decisions from invertOrNot API
- **`prepareFullWidthFigures`** — Handles `width-full` class figures, constrains captions
- **`rectifyImageAuxText`** — Ensures figcaption, alt, and title don't duplicate each other
- **`addSwapOutThumbnailEvents`** — Lazy-swaps thumbnails for full images when viewport requires

### Tables (~100 lines)

- **`deleteColgroups`** — Removes Pandoc-inserted `<colgroup>` elements
- **`wrapTables`** — Wraps tables in `.table-wrapper` and `.table-scroll-wrapper`
- **`makeTablesSortable`** — Imports tablesorter.js and initializes sortable tables
- **`rectifyFullWidthTableWrapperStructure`** — Fixes wrapper structure for full-width tables

### Links (~400 lines)

- **`reverseArchivedLinkPolarity`** — Swaps `href` with `data-url-original` for archived links
- **`qualifyAnchorLinks`** — Rewrites anchor link pathnames for transcluded content
- **`addSpecialLinkClasses`** — Adds `.link-self` and `.link-page` classes
- **`designateLocalNavigationLinkIcons`** — Assigns directional arrows (↑/↓) or pilcrow (¶) to self-links
- **`setLinkIconStates`** — Enables/disables link icon display via CSS custom properties
- **`enableLinkIcon(link)` / `disableLinkIcon(link)`** — Utility functions for link icon management

### Code Blocks (~100 lines)

- **`wrapPreBlocks`** — Wraps `<pre>` in `.sourceCode` div
- **`addCodeBlockLineClasses`** — Adds `.line` class to each line span for hover highlighting
- **`rectifyCodeBlockClasses`** — Moves float classes from `<pre>` to wrapper
- **`wrapFullWidthPreBlocks`** — Wraps full-width code blocks appropriately

### Footnotes & Sidenotes (~200 lines)

- **`addFootnoteClassToFootnotes`** — Adds `.footnote` class to footnote list items
- **`injectFootnoteSelfLinks`** — Adds self-link anchors to footnotes
- **`rewriteFootnoteBackLinks`** — Replaces Pandoc back-link text with SVG arrow
- **`markTargetedFootnote`** — Adds `.targeted` class to URL-hash-targeted footnote
- **`bindNoteHighlightEventsToCitations`** — Highlights note on citation hover

### Lists & Blockquotes (~150 lines)

- **`designateListTypes`** — Sets ordered list type classes based on nesting (decimal → roman → alpha)
- **`designateBlockquoteLevels`** — Sets blockquote level classes (cycles 1-6)
- **`disableSingleItemColumnBlocks`** — Removes `.columns` class from single-item lists

### Typography (~200 lines)

- **`rectifyTypographyInContentTransforms`** — Applies smart quotes, word breaks, ellipses
- **`hyphenate`** — Runs Hyphenopoly on eligible paragraphs
- **`iconifyUnicodeIconGlyphs`** — Converts Unicode glyphs (e.g., ☞) to icon spans
- Copy processors for proper clipboard handling of citations, soft hyphens, etc.

### Annotations (~300 lines)

- **`rewriteTruncatedAnnotations`** — Makes partial annotations link to full
- **`rectifyFileAppendClasses`** — Fixes file-include collapse styling
- **`rewriteAuxLinksLinksInTranscludedAnnotations`** — Makes aux-links scroll to appended blocks
- **`bindSectionHighlightEventsToAnnotatedLinks`** — Highlights corresponding annotation on hover

### Special Content Types

- **Poetry** (`processPoems`, `processPreformattedPoems`) — Stanza/line structure, enjambment
- **Epigraphs** (`designateEpigraphAttributions`) — Attribution styling
- **Interviews** (`rewriteInterviews`) — Speaker/utterance structure
- **Dropcaps** (`rewriteDropcaps`, `linkifyDropcaps`) — Graphical and textual dropcaps
- **Math** (`unwrapMathBlocks`, `addBlockButtonsToMathBlocks`) — LaTeX block handling

---

## Representative Handler Examples

### Image Wrapping (Load Handler)

```javascript
addContentLoadHandler(GW.contentLoadHandlers.wrapImages = (eventInfo) => {
    GWLog("wrapImages", "rewrite.js", 1);

    // Unwrap single-child images from paragraphs
    eventInfo.container.querySelectorAll("p > img:only-child").forEach(image => {
        unwrap(image.parentElement);
    });

    // Wrap all qualifying images in <figure>
    let exclusionSelector = ["td", "th", ".footnote-back"].join(", ");
    wrapAll("img", (image) => {
        if (image.classList.contains("figure-not")
            || image.closest(exclusionSelector) != null
            || image.closest("figure") != null)
            return;
        wrapElement(image, "figure");
    }, { root: eventInfo.container });
}, "rewrite");
```

### Directional Link Icons (Inject Handler)

```javascript
addContentInjectHandler(GW.contentInjectHandlers.designateLocalNavigationLinkIcons = (eventInfo) => {
    GWLog("designateLocalNavigationLinkIcons", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".link-self").forEach(link => {
        if (link.closest(".icon-not, .icon-special, #sidebar"))
            return;

        // Find target and determine direction
        let target = eventInfo.document.querySelector(selectorFromHash(link.hash));
        if (target == null) return;

        link.dataset.linkIconType = "text";
        link.dataset.linkIcon =
            (link.compareDocumentPosition(target) & Node.DOCUMENT_POSITION_FOLLOWING)
            ? "\u{2193}"  // ↓ target is after
            : "\u{2191}"; // ↑ target is before
    });
}, "rewrite");
```

### Citation Highlight Events (Inject Handler, eventListeners Phase)

```javascript
addContentInjectHandler(GW.contentInjectHandlers.bindNoteHighlightEventsToCitations = (eventInfo) => {
    GWLog("bindNoteHighlightEventsToCitations", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".footnote-ref").forEach(citation => {
        let notesForCitation = Notes.allNotesForCitation(citation);
        if (notesForCitation == null || notesForCitation.length == 0)
            return;

        citation.addEventListener("mouseenter", () => {
            notesForCitation.forEach(note => note.classList.toggle("highlighted", true));
        });
        citation.addEventListener("mouseleave", () => {
            notesForCitation.forEach(note => note.classList.toggle("highlighted", false));
        });
    });
}, "eventListeners");
```

---

## Key Utility Functions

These are defined in utility.js and used throughout rewrite.js:

| Function | Purpose |
|----------|---------|
| `wrapElement(el, spec, opts)` | Wraps element in new parent (`spec` = "div.class1.class2") |
| `wrapAll(selector, spec, opts)` | Wraps all matching elements |
| `unwrap(wrapper, opts)` | Replaces wrapper with its children |
| `unwrapAll(selector, opts)` | Unwraps all matching elements |
| `newElement(tag, attrs, props)` | Creates element with attributes and properties |
| `elementFromHTML(html)` | Parses HTML string into element |
| `transferClasses(from, to, classes)` | Moves classes between elements |

---

## Configuration

**Hyphenopoly** configuration (lines ~2197-2206):
```javascript
Hyphenopoly.config({
    require: { "en-us": "FORCEHYPHENOPOLY" },
    setup: { hide: "none", keepAlive: true, safeCopy: false }
});
```

**Full-width block layout** (`GW.fullWidthBlockLayout`):
- `sideMargin: 25` — Pixels of margin on viewport edges
- Dynamically updated on window resize

**Link prefetching** (instant.page integration, ~lines 4359-4461):
- `delayOnHover: 1600` — ms before prefetch on hover
- Exclusions via `.prefetch-not`, `.has-content`

---

## Integration Points

**Events Listened:**
- `GW.contentDidLoad` — Primary hook for content transformations
- `GW.contentDidInject` — Post-injection DOM work and event binding
- `GW.hashDidChange` — Updates targeted footnote styling
- `DarkMode.didSetMode` — Updates mode-dependent styling
- `DarkMode.computedModeDidChange` — Swaps graphical dropcap images
- `GW.imageInversionJudgmentsAvailable` — Applies inversion when API responds

**Events Fired:**
- `Rewrite.contentDidChange` — When content modification completes
- `Rewrite.fullWidthMediaDidLoad` — When full-width image/video loads

**Module Dependencies:**
- `GW.notificationCenter` (initial.js)
- `Transclude` (transclude.js) — For triggering transcludes
- `Annotations` (annotations.js) — For annotation link detection
- `Notes` (notes.js) — For footnote/sidenote management
- `Extracts` (extracts.js) — For pop-frame context detection
- `DarkMode` (darkmode.js) — For theme-aware styling
- `Typography` (typography.js) — For text processing
- `Content` (content.js) — For reference data

---

## See Also

- [initial.js](/frontend/initial-js) - Notification center and handler registration that rewrite.js depends on
- [rewrite-initial.js](/frontend/rewrite-initial-js) - Fast non-block-layout processors that run before rewrite.js
- [content.js](/frontend/content-js) - Content loading and data providers consumed by rewrite handlers
- [transclude.js](/frontend/transclude-js) - Transclusion pipeline that triggers rewrite handlers
- [typography.js](/frontend/typography-js) - Typography processing called by rewrite handlers
- [utility.js](/frontend/utility-js) - DOM manipulation utilities (wrapElement, unwrap, newElement)
- [extracts.js](/frontend/extracts-js) - Pop-frame extraction system that uses rewrite handlers
