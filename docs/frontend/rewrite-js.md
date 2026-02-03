
# rewrite.js

**Path:** `js/rewrite.js` | **Language:** JavaScript | **Lines:** ~4,536

> DOM transformation pipeline—registers dozens of handlers that run on content load/inject to enhance raw HTML

---

## Overview

`rewrite.js` is the largest JavaScript file in gwern.net and serves as the central DOM transformation pipeline. It contains approximately 90+ handlers that register themselves via `addContentLoadHandler()` and `addContentInjectHandler()` from initial.js. When content is loaded (from the server, via transclusion, or from annotations), these handlers fire in sequence to transform raw HTML into the enhanced gwern.net reading experience.

The handlers are organized around content categories: images and figures, tables, code blocks, footnotes, links, lists, blockquotes, poetry, annotations, typography, search, ID-based loading (/ref/ system), aux-links/backlinks, and more. Each handler typically queries for specific selectors within the injected container, then applies transformations—wrapping elements, adding classes, setting CSS properties, binding event listeners, or restructuring the DOM.

This architecture separates concerns cleanly: content.js and transclude.js handle fetching and templating content, while rewrite.js focuses purely on transformation. The notification center's phase system ensures handlers run in the correct order (e.g., structural changes before event listener attachment).

---

## Handler Registration System

Handlers are registered via two convenience functions defined in initial.js:

### `addContentLoadHandler(name, handler, phase, condition)`

Registers a handler for `GW.contentDidLoad` events. These fire when content is loaded from any source but before injection into the document.

### `addContentInjectHandler(name, handler, phase, condition)`

Registers a handler for `GW.contentDidInject` events. These fire after content has been injected into a document (main page or pop-frame).

**Parameters:**
- `name` — String identifier for the handler (e.g., `"wrapImages"`)
- `handler` — Function receiving an `eventInfo` object with:
  - `container` — The DOM element containing the loaded/injected content
  - `document` — The document (main or pop-frame) the content belongs to
  - `source` — String identifying the content origin (e.g., "transclude", "Annotation.load")
  - `contentType` — String like "annotation", "localPage", "tweet", etc.
  - `loadLocation` — URL of the loaded content
  - `fullWidthPossible` — Boolean flag for layout context
  - `includeLink` — The include-link element that triggered transclusion (if applicable)
- `phase` — Execution order hint (see Phase System below)
- `condition` — Optional predicate function for selective execution

---

## Phase System

Handlers run in a controlled order based on their declared phase. The phase orders are defined by `GW.notificationCenter.handlerPhaseOrders`:

**`GW.contentDidLoad`:** `transclude` → `rewrite`

**`GW.contentDidInject`:** `rewrite` → `eventListeners`

`"<phase"` and `">phase"` are before/after pseudo-phases for each named phase.

Within a phase, you can fine-tune order with prefixes:
- `"<rewrite"` — Run before main rewrite phase
- `">rewrite"` — Run after main rewrite phase

```javascript
// Runs before most rewrite handlers
addContentLoadHandler("myEarlyHandler", (eventInfo) => {
    // ...
}, "<rewrite");

// Runs after most rewrite handlers
addContentInjectHandler("myLateHandler", (eventInfo) => {
    // ...
}, ">rewrite");
```

The `"eventListeners"` phase is used for binding mouse/keyboard events—always done last since DOM structure must be finalized.

---

## Handler Categories

### Clipboard & Copy Processors (~100 lines)

- **`registerCopyProcessorsForDocument()`** — Sets up copy processors in main document
- **Copy processors** for:
  - Soft hyphen removal (`Typography.processElement`)
  - Citation joiner text display (`.cite-joiner`)
  - Symbol normalization
  - Date range metadata stripping
  - Inflation adjuster formatting
  - Math LaTeX source copying
  - Mode selector button text representation

### Search (~50 lines)

- **`setUpSearchIframe`** — Configures search iframe with dark mode support, "search where" functionality, and form submit override

### ID-Based Loading / /ref/ System (~300 lines)

- **`loadReferencedIdentifier`** — Loads content based on URL path in `/ref/` namespace
  - Handles URL lookups via `id-to-url` mapping files
  - Supports prefix matching for both IDs and URLs
  - Normalizes manual IDs (lowercase, reversed date fix)
  - Injects helpful error messages and suggestions

### Aux-Links & Backlinks (~400 lines)

- **`anonymizeLinksInBacklinkContextBlocks`** — Strips IDs from links in backlink contexts
- **`getBacklinksBlockForSectionOrFootnote()`** — Creates/retrieves backlinks block for sections or footnotes
- **`updateBacklinksCountDisplay()`** — Updates backlink count in display
- **`addWithinPageBacklinksToSectionBacklinksBlocks`** — Adds within-page section backlinks
- **`rectifyLocalizedBacklinkContextLinks`** — Converts "full context" to "context" for local backlinks
- **`injectBacklinksLinkIntoLocalSectionPopFrame`** — Adds backlinks link to section popups
- **`removeAuxLinksListLabelsInAuxLinksSections`** — Removes redundant labels in aux-links sections

### Lists (~150 lines)

- **`designateListTypes`** — Sets ordered list type classes (decimal → upper-roman → lower-alpha cycle)
- **`orderedListType()` / `setOrderedListType()`** — Get/set OL type
- **`unorderedListLevel()` / `setUnorderedListLevel()`** — Get/set UL nesting level (cycles 1-3)
- **`paragraphizeListTextNodes`** — Wraps text nodes in list items in `<p>` tags
- **`rectifyListHeadings`** — Fixes styling of `<p><strong>:</strong></p>` patterns

### Blockquotes (~50 lines)

- **`designateBlockquoteLevels`** — Sets blockquote level classes (cycles 1-6)
- **`blockquoteLevel()` / `setBlockquoteLevel()`** — Get/set blockquote nesting level

### Tables (~100 lines)

- **`deleteColgroups`** — Removes Pandoc-inserted `<colgroup>` elements
- **`wrapTables`** — Wraps tables in `.table-wrapper` and `.table-scroll-wrapper`
- **`makeTablesSortable`** — Imports tablesorter.js and initializes sortable tables
- **`rectifyFullWidthTableWrapperStructure`** — Fixes wrapper structure for full-width tables

### Figures & Images (~600 lines)

- **`addSwapOutThumbnailEvents`** — Lazy-swaps thumbnails for full images when viewport requires
- **`requestImageInversionJudgments`** — Requests inversion/outlining judgments from APIs
- **`applyImageInversionAndOutliningJudgments`** — Applies inversion/outlining based on API responses
- **`applyImageInversionJudgmentNowOrLater()` / `applyImageOutliningJudgmentNowOrLater()`** — Applies judgments immediately or when available
- **`paragraphizeFigcaptionTextNodes`** — Wraps figcaption text nodes in `<p>` tags
- **`rectifyImageAuxText`** — Ensures figcaption, alt, and title don't duplicate
- **`wrapImages`** — Wraps bare images in `<figure>` tags
- **`injectThumbnailIntoPopFramePageAbstract`** — Injects page thumbnail into full-page pop-frame abstracts
- **`setMediaElementDimensions()`** — Sets CSS dimensions from HTML attributes
- **`updateMediaElementDimensions`** — Updates dimensions for pop-frame media
- **`setImageDimensionsFromImageData`** — Sets dimensions from data: URIs
- **`addOrientationChangeMediaElementDimensionUpdateEvents`** — Updates on device rotation
- **`wrapFigures`** — Adds inner structure: `.figure-outer-wrapper`, `.image-wrapper`, `.caption-wrapper`
- **`addMediaLinkWrappers`** — Wraps annotated images/videos in link wrappers for popup integration
- **`disableAnnotatedMediaLinkWrapperClickEvents`** — Disables click on media wrapper links (desktop)
- **`designateImageBackdropInversionStatus`** — Sets dark mode inversion classes on image wrappers
- **`removeEmptyFigureCaptions`** — Removes empty figcaptions
- **`rectifyFigureClasses`** — Moves float/outline classes from media to figure
- **`deFloatSolitaryFigures`** — Removes float from figures that are only children
- **`prepareFullWidthFigures`** — Sets up full-width figures with caption width constraints

### Video (~100 lines)

- **`videoPosterURL()`** — Returns video poster URL
- **`setVideoPosters`** — Auto-sets poster URL for local videos
- **`lazyLoadVideoPosters`** — Implements lazy loading for video posters
- **`enableVideoClickToPlay`** — Enables click-anywhere to play/pause videos

### Poetry (~300 lines)

- **`processPreformattedPoems`** — Processes HTML poems using whitespace for layout (enjambment)
- **`processPoems`** — Divides poems into stanzas with each line as `<p>`
- **`wrapSlashesInPoems`** — Wraps line-break-indicator slashes in `span.slash`
- **`wrapCaesuraMarksInPoems`** — Wraps `||` caesura marks
- **`rewriteCenteredPoemThingies`** — Special layout for centered stanzas with caesura marks
- **`designateFirstAndLastLinesInPoemStanzas`** — Adds `.first-line` / `.last-line` classes

### Epigraphs (~100 lines)

- **`designatePoemEpigraphsInPoems`** — Marks epigraphs in poems
- **`designateEpigraphAttributions`** — Converts em-dash-prefixed paragraphs to `.attribution`
- **`designateNarrowEpigraphs`** — Marks epigraphs squeezed by floats

### Code Blocks (~100 lines)

- **`wrapPreBlocks`** — Wraps `<pre>` in `.sourceCode` div
- **`addCodeBlockLineClasses`** — Adds `.line` class to each line span
- **`rectifyCodeBlockClasses`** — Moves float classes from `<pre>` to wrapper
- **`wrapFullWidthPreBlocks`** — Wraps full-width code blocks

### Embeds (~50 lines)

- **`markLoadedEmbeds`** — Adds load tracking for iframes
- **`applyIframeScrollFix`** — Workaround for Chrome anchor scrolling bug

### Headings (~50 lines)

- **`injectCopySectionLinkButtons`** — Adds copy-link buttons to section headings

### Columns (~25 lines)

- **`disableSingleItemColumnBlocks`** — Removes `.columns` class from single-item lists

### Interviews (~75 lines)

- **`rewriteInterviews`** — Restructures interview HTML with speaker/utterance classes

### Margin Notes (~75 lines)

- **`wrapMarginNotes`** — Wraps margin note contents in inner wrapper
- **`aggregateMarginNotes`** — Aggregates margin notes via `aggregateMarginNotesInDocument()`

### Typography (~200 lines)

- **`rectifyTypographyInContentTransforms`** — Applies smart quotes, word breaks, ellipses for Wikipedia/tweet content
- **`rectifyTypographyInBodyText`** — Adds word breaks after slashes in body text
- **`removeExtraneousWhitespaceFromCitations`** — Cleans up `.cite` elements
- **`iconifyUnicodeIconGlyphs`** — Converts Unicode glyphs (e.g., ☞) to icon spans
- **`hyphenate`** — Runs Hyphenopoly on eligible paragraphs

### Full-Width Blocks (~100 lines)

- **`createFullWidthBlockLayoutStyles()`** — Creates CSS variables for full-width layout
- **`setMarginsOnFullWidthBlocks`** — Sets margins for full-width blocks

### Annotations (~200 lines)

- **`rewriteTruncatedAnnotations`** — Makes partial annotations link to full
- **`designateBlogPosts`** — Marks blog post annotations
- **`rectifyBlogPosts`** — Removes title from blog post annotations on their own pages
- **`rewriteAnnotationTitleLinksInPopFrames`** — Strips quotes from title-links in pop-frames
- **`rectifyFileAppendClasses`** — Fixes file-include collapse styling
- **`rectifyInlineAnnotationTitleClasses`** — Treats un-annotated include links as title-links
- **`handleFileIncludeUncollapseInAnnotations`** — Handles file include uncollapse events
- **`rewriteAuxLinksLinksInTranscludedAnnotations`** — Makes aux-links scroll to appended blocks
- **`bindSectionHighlightEventsToAnnotatedLinks`** — Highlights annotations on link hover

### Directory Indexes (~50 lines)

- **`stripInvalidFileAppends`** — Removes invalid file embed links

### Link Bibliography (~50 lines)

- **`applyLinkBibliographyCompactStylingClass`** — Applies compact styling to link-bibs
- **`rectifyLinkBibliographyContextLinks`** — Injects context links into annotation title lines

### Table of Contents (~150 lines)

- **`setTOCCollapseState()`** — Sets TOC collapse state and button
- **`injectTOCCollapseToggleButton`** — Adds collapse button to main TOC
- **`stripTOCLinkSpans`** — Removes spurious spans from TOC links
- **`updateMainPageTOC`** — Updates TOC with new sections
- **`rectifyTypographyInTOC`** — Applies word breaks to TOC
- **`disableTOCLinkDecoration`** — Disables link decoration on TOC links
- **`rewriteDirectoryIndexTOC`** — Relocates and cleans up TOC on tag index pages
- **`addRecentlyModifiedDecorationsToPageTOC`** — Adds star icons to recently modified TOC entries
- **`updateTOCVisibility`** — Hides TOC if ≤1 entry

### Footnotes (~200 lines)

- **`rectifyFootnoteSectionTagName`** — Ensures footnotes section is a `<section>`
- **`injectFootnoteSectionSelfLink`** — Adds self-link to footnotes section
- **`addFootnoteClassToFootnotes`** — Adds `.footnote` class to footnote list items
- **`markTargetedFootnote`** — Marks hash-targeted footnote with `.targeted`
- **`injectFootnoteSelfLinks`** — Adds self-link anchors to footnotes
- **`rewriteFootnoteBackLinks`** — Replaces Pandoc back-link text with SVG arrow
- **`invalidateCachedNotesIfNeeded`** — Invalidates cached notes when content has footnotes
- **`bindNoteHighlightEventsToCitations`** — Highlights notes on citation hover
- **`bindHighlightEventsToFootnoteSelfLinks`** — Highlights footnotes on self-link hover

### Links (~400 lines)

- **`reverseArchivedLinkPolarity`** — Swaps `href` with `data-url-original`
- **`qualifyAnchorLinks`** — Rewrites anchor link pathnames for transcluded content
- **`addSpecialLinkClasses`** — Adds `.link-self` and `.link-page` classes
- **`identifyAnchorLinks`** — Adds IDs to within-page links
- **`designateLocalNavigationLinkIcons`** — Assigns icons (↑/↓/¶/gwern logo) to links
- **`cleanSpuriousLinkIcons`** — Removes link icons from excluded contexts
- **`renderQuadLinkIcon()`** — Renders SVG quad-letter link icon
- **`enableLinkIcon()` / `disableLinkIcon()`** — Enable/disable link icon display
- **`setLinkIconStates`** — Updates link icon display states
- **`enableLinkIconColor()` / `disableLinkIconColor()`** — Enable/disable link hover colorization
- **`setLinkHoverColors`** — Sets hover colors for links with `data-link-icon-color`

### Date Ranges & Inflation (~100 lines)

- **`prettifyCurrencyString()`** — Formats currency amounts
- **`rewriteInflationAdjusters`** — Rewrites inflation-adjustment elements
- **`addDoubleClickListenersToInflationAdjusters`** — Enables double-click selection

### Miscellaneous (~200 lines)

- **`resolveRandomElementSelectors`** — Handles `display-random-N` containers
- **`regeneratePlaceholderIds`** — Regenerates placeholder IDs
- **`removeNoscriptTags`** — Removes `<noscript>` tags
- **`cleanUpImageAltText`** — Sets default alt text, URL-encodes % signs
- **`noBreakForCitations`** — Prevents line breaks around citations
- **`designateColorInvertedContainers`** — Marks containers with dark backgrounds
- **`paragraphizeAdmonitionTextNodes`** — Wraps admonition text in `<p>` tags
- **`rectifySpecialTextBlockTagTypes`** — Fixes incorrect `<div>` vs `<p>` usage
- **`designateOrdinals`** — Marks ordinal superscripts (1st, 2nd, etc.)
- **`rectifyPageMetadataFieldLinkAppearance`** — Fixes colon appearance in page metadata
- **`rectifyTOCAdjacentBlockLayout`** — Handles TOC-adjacent block clearing

### Dropcaps (~250 lines)

- **`rewriteDropcaps`** — Creates graphical or textual dropcaps
- **`activateDynamicGraphicalDropcaps`** — Swaps dropcap images on dark mode change
- **`linkifyDropcaps`** — Wraps dropcaps in links to `/dropcap#type`
- **`preventDropcapsOverlap`** — Prevents dropcap blocks from overlapping

### Math (~150 lines)

- **`unwrapMathBlocks`** — Unwraps `<p>` wrappers of math blocks
- **`addDoubleClickListenersToMathBlocks`** — Enables double-click to select equations
- **`addBlockButtonsToMathBlocks`** — Adds copy button to block math
- **`activateMathBlockButtons`** — Activates copy functionality

### Printing (~50 lines)

- **`beforeprint` / `afterprint` handlers** — Triggers transcludes and expands collapses for printing

### Broken HTML/Anchor Checking (~50 lines)

- **`reportBrokenAnchorLink()`** — Reports broken anchor links to server
- **Broken anchor check** — Validates location hash on load and hash change

### Link Prefetching (instant.page) (~100 lines)

- **instant.page integration** — Prefetches links on hover (1600ms delay)
- Configurable via `delayOnHover`, `linkPrefetchExclusionSelector`

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
| `atomicDOMUpdate(element, callback)` | Performs DOM update atomically |
| `paragraphizeTextNodesOfElementRetainingMetadata()` | Wraps text nodes in `<p>` tags |

---

## Configuration

**Hyphenopoly** configuration (lines ~2368-2377):
```javascript
Hyphenopoly.config({
    require: { "en-us": "FORCEHYPHENOPOLY" },
    setup: { hide: "none", keepAlive: true, safeCopy: false }
});
```

**Full-width block layout** (`GW.fullWidthBlockLayout`):
- `sideMargin: 25` — Pixels of margin on viewport edges
- Dynamically updated on window resize

**Ordered list types** (`GW.layout.orderedListTypes`):
- `decimal`, `lower-alpha`, `upper-alpha`, `lower-roman`, `upper-roman`, `lower-greek`

**Link prefetching** (instant.page integration):
- `delayOnHover: 1600` — ms before prefetch on hover
- `linkPrefetchExclusionSelector`: `.prefetch-not`, `.has-content`

---

## Integration Points

**Events Listened:**
- `GW.contentDidLoad` — Primary hook for content transformations
- `GW.contentDidInject` — Post-injection DOM work and event binding
- `GW.hashDidChange` — Updates targeted footnote styling, broken anchor checking
- `DarkMode.didSetMode` — Updates mode-dependent styling
- `DarkMode.computedModeDidChange` — Swaps graphical dropcap images
- `GW.imageInversionJudgmentsAvailable` — Applies inversion when API responds
- `GW.imageOutliningJudgmentsAvailable` — Applies outlining when API responds
- `Layout.layoutProcessorDidComplete` — TOC-adjacent block layout rectification
- `Collapse.collapseStateDidChange` — Backlinks link uncollapse handling
- `beforeprint` / `afterprint` — Print preparation

**Events Fired:**
- `Rewrite.contentDidChange` — When content modification completes
- `Rewrite.fullWidthMediaDidLoad` — When full-width image/video loads

**Module Dependencies:**
- `GW.notificationCenter` (initial.js)
- `Transclude` (transclude.js) — For triggering transcludes
- `Annotations` (annotations.js) — For annotation link detection
- `Notes` (notes.js) — For footnote/sidenote management
- `Extracts` (extracts.js) — For pop-frame context detection
- `DarkMode` (dark-mode.js) — For theme-aware styling
- `Typography` (typography.js) — For text processing
- `Content` (content.js) — For reference data
- `Images` (misc.js) — For thumbnail/image utilities
- `AuxLinks` (misc.js) — For aux-links type detection
- `Sidenotes` (sidenotes.js) — For viewport width breakpoint media query
- `Color` (color.js) — For link icon colorization
- `ImageFocus` (image-focus.js) — For popup/image-focus coordination
- `Popups` (popups.js) — For popup coordination

---

## See Also

- [initial.js](/frontend/initial-js) - Notification center and handler registration that rewrite.js depends on
- [rewrite-initial.js](/frontend/rewrite-initial-js) - Fast non-block-layout processors that run before rewrite.js
- [content.js](/frontend/content-js) - Content loading and data providers consumed by rewrite handlers
- [transclude.js](/frontend/transclude-js) - Transclusion pipeline that triggers rewrite handlers
- [typography.js](/frontend/typography-js) - Typography processing called by rewrite handlers
- [utility.js](/frontend/utility-js) - DOM manipulation utilities (wrapElement, unwrap, newElement)
- [extracts.js](/frontend/extracts-js) - Pop-frame extraction system that uses rewrite handlers
- [dark-mode.js](/frontend/dark-mode-js) - Dark mode system for theme-aware styling
- [misc.js](/frontend/misc-js) - Image utilities and aux-links helpers
