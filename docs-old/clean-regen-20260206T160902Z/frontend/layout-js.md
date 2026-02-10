
# layout.js

**Path:** `js/layout.js` | **Language:** JavaScript | **Lines:** ~1,584

> Block layout system for computing vertical spacing and structural classes

---

## Overview

layout.js implements a CSS-independent block layout system that computes vertical spacing between elements and applies structural classes (first-block, last-block, intro-graf, etc.) to enable sophisticated typographic styling. Unlike traditional CSS margin collapsing, this system uses a "block spacing multiplier" (BSM) approach where each block gets a `--bsm` CSS custom property that controls its top margin via `calc()` in the stylesheet.

The system operates through **layout processors** that run whenever DOM mutations occur within "block containers" (`.markdownBody`, `section`, `blockquote`, etc.). A `MutationObserver` watches for changes and queues affected containers for reprocessing on the next animation frame. This reactive architecture ensures layout stays correct as content is dynamically loaded, transcluded, or transformed.

A key design decision is the concept of **wrappers**—elements like `<div>`, `<span>`, and `<li>` that are "transparent" to block flow. The system looks through wrappers to find the actual blocks inside them, enabling correct spacing even when blocks are nested in structural containers. There are also **half-wrappers** (like `section`) that are transparent at the bottom but not the top.

---

## Public API

### `addLayoutProcessor(name, processor, options)`

Registers a function to run during layout passes.

```javascript
addLayoutProcessor("applyBlockLayoutClassesInContainer", (blockContainer) => {
    // Process blocks within container
}, { blockLayout: true });
```

**Called by:** Module initialization
**Calls:** Registered processor functions during layout

**Options:**
- `blockLayout` (boolean, default true): If false, runs even in block-layout-excluded zones
- `condition` (function): Predicate receiving `{document, container, processorName, blockContainer}`; skip if returns false

---

### `processContainerNowAndAfterBlockLayout(container, callback)`

Runs callback immediately and again whenever `applyBlockLayoutClassesInContainer` completes on that container.

**Called by:** External modules needing layout-dependent updates
**Calls:** `callback`, subscribes to `Layout.layoutProcessorDidComplete`

---

### `doWhenPageLayoutComplete(f)`

Executes function immediately if initial layout is done, otherwise queues for `Layout.initialPageLayoutDidComplete` event.

**Called by:** Any code needing to wait for first-pass layout
**Calls:** Provided function `f`

---

### Block Query Functions

All use `useLayoutCache()` for memoization within a layout pass:

| Function | Returns |
|----------|---------|
| `isBlock(element, options)` | true if element matches `blockElementsSelector` |
| `isSkipped(element, options)` | true if element matches `skipElementsSelector` |
| `isEmpty(element, options)` | true if element has no meaningful content |
| `isWrapper(element, wrapperType, options)` | true if element is transparent wrapper (types: upOut, downOut, upIn, downIn) |
| `blockContainerOf(element, options)` | Nearest ancestor block container |
| `previousBlockOf(element, options)` | Previous block in flow (skipping floats, empties) |
| `nextBlockOf(element, options)` | Next block in flow |
| `firstBlockOf(element, options, strictDescent)` | First block within element |
| `lastBlockOf(element, options, strictDescent)` | Last block within element |
| `childBlocksOf(element, options)` | Array of direct child blocks (looking through wrappers) |

---

### `getBlockSpacingMultiplier(block, debug = false)`

Computes BSM for a block by matching against `GW.layout.blockSpacing` rules, then applying adjustments from `GW.layout.blockSpacingAdjustments`.

**Returns:** Integer 0-16, or `undefined` if no rule matches

---

## Internal Architecture

### Configuration Arrays

```javascript
GW.layout = {
    blockContainers: [".markdownBody", "section", ".collapse-block", ...],
    blockElements: ["section", "p", "figure", "hr", ...],
    skipElements: [".empty", ".hidden", ".float", ...],
    wrapperElements: ["div", "span", ".list", "li", ...],
    halfWrapperElements: ["section"],
    // ...
};
```

### Block Spacing Rules

Two-tier system:

1. **`blockSpacing`**: Array of `[selector, bsm, adjustable?]` tuples, checked in order. First match wins.

```javascript
[ "section.level1",  15 ],      // Level 1 sections get BSM 15
[ "p.first-graf",    10 ],      // First paragraphs get BSM 10
[ "p",                0 ],      // Other paragraphs get BSM 0
```

2. **`blockSpacingAdjustments`**: Array of `[selector, transformFn]` applied to adjustable matches.

```javascript
[ "p + p",           (bsm, block) => bsm - 6 ],  // Consecutive paragraphs: tighter
[ "figcaption *",    (bsm, block) => bsm - 2 ],  // Inside figcaptions: tighter
```

### Sibling Combinator Handling

The `+` combinator in selectors is preprocessed into a custom matcher that handles floats specially—floats match if they themselves satisfy the preceding-block selector, but are otherwise "skipped over" to find the actual previous block.

### Layout Cache

Each element gets a `layoutCache` Map storing computed results. Cache is invalidated at the start of each layout pass via timestamp comparison with `GW.layout.currentPassBegin`.

---

## Key Patterns

### Wrapper Transparency

Wrappers have directional transparency. A wrapper is "transparent" for a given traversal direction if it doesn't constitute a layout block itself:

- **downIn / upOut**: Look through `div`, `span`, `li`, `figcaption` to find nested blocks
- **downOut / upIn**: Also look through `section` (half-wrapper)

This enables correct handling of structures like `<li><div><p>text</p></div></li>` where the `<p>` should be treated as if directly in the `<li>`.

### Dynamic Layout via MutationObserver

```javascript
function startDynamicLayoutInContainer(container) {
    let observer = new MutationObserver((mutationsList) => {
        // Find affected block containers
        // Queue them in blockContainersNeedingLayout
        requestAnimationFrame(() => {
            // Process all queued containers
        });
    });
    observer.observe(container, { subtree: true, childList: true });
}
```

### Layout Isolation Zones

Elements matching `layoutIsolationSelector` (like `.sidenote-column`) create boundaries—mutations inside don't trigger layout in the outer container, only in isolated sub-containers.

---

## Configuration

### `GW.layout.blockSpacing`

Controls vertical spacing. Format: `[selector, bsm, adjustable]`
- `selector`: CSS selector (supports custom `+` combinator handling)
- `bsm`: Integer 0-16 (maps to CSS margin)
- `adjustable`: If false, skip adjustments (default true)

### `GW.layout.blockSpacingAdjustments`

Fine-tunes spacing. Format: `[selector|selectorArray, (bsm, block) => newBsm]`

### Layout Exclusion/Isolation

- `blockLayoutExclusionSelector`: Skip block classes entirely (e.g., `#page-metadata`, `.popframe`)
- `layoutIsolationSelector`: Isolate layout recalculation (e.g., `.sidenote-column`)

---

## Integration Points

### Events Fired

| Event | When | Payload |
|-------|------|---------|
| `Layout.layoutProcessorDidComplete` | After each processor runs on a block container | `{document, container, processorName, blockContainer}` |
| `Layout.initialPageLayoutDidComplete` | After first full page layout | (none) |

### Events Listened

| Event | Response |
|-------|----------|
| `Collapse.collapseStateDidChange` | Re-run `applyBlockSpacingInContainer` on the collapse block |

### Content Load Handlers

- `applyBlockLayoutClassesInMainDocument`: Runs `<rewrite` phase on `document.main`
- `applyBlockLayoutClassesInDocumentFragment`: Runs `<rewrite` on `DocumentFragment` containers
- `completePageLayout`: Fires initial-complete event `<eventListeners` phase

### CSS Custom Properties Set

- `--bsm`: Block spacing multiplier (integer), consumed by stylesheet for `margin-top` calculation

### Classes Applied

| Class | Meaning |
|-------|---------|
| `.block` | Element participates in block spacing |
| `.first-block` | First block in container |
| `.last-block` | Last block in container |
| `.first-graf` | Paragraph not preceded by another paragraph |
| `.intro-graf` | Opening paragraph (may get dropcap) |
| `.list-heading` | Paragraph ending in `:` before a list |
| `.empty-graf` | Empty paragraph (hidden) |
| `.in-list` | Block inside a list item |
| `.heading` | H1-H6 element |
| `.float` | Floating element (non-mobile only) |
| `.has-floats` | List containing floats |
| `.big-list` | List with multi-block items |
| `.list` | UL or OL element |
| `.dropcap-{type}` | Dropcap style (goudy, kanzlei, etc.) |

---

## See Also

- [sidenotes.js](/frontend/sidenotes-js) - Uses layout classes for margin note positioning
- [rewrite.js](/frontend/rewrite-js) - Transforms that may trigger layout recalculation
- [initial.js](/frontend/initial-js) - Core framework that loads layout module
- [collapse.js](/frontend/collapse-js) - Fires `collapseStateDidChange` for re-layout
- [transclude.js](/frontend/transclude-js) - Content injection triggers layout updates
- [Columns.hs](/backend/columns-hs) - Server-side multi-column layout detection
- [Typography.hs](/backend/typography-hs) - Server-side transforms affecting layout
