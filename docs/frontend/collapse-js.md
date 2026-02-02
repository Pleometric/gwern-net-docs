
# collapse.js

**Path:** `js/collapse.js` | **Language:** JavaScript | **Lines:** ~1,130

> Collapsible content sections with disclosure buttons and auto-expansion

---

## Overview

collapse.js implements a collapsible section system that hides long content behind disclosure buttons. It supports two collapse types: **block collapses** (full-width sections with chevron-based disclosure buttons) and **inline collapses** (in-text collapsed spans with bracket-style buttons at both ends).

The module handles three core concerns: (1) preparing raw `.collapse` elements into fully functional collapse blocks with appropriate wrappers, buttons, and visual indicators; (2) managing expand/collapse state transitions including nested collapse handling; (3) auto-expanding collapse blocks when the user navigates to contained elements via URL hash or browser find (Ctrl+F).

A notable feature is the "iceberg indicator"—a visual progress bar showing how much content remains hidden. This is calculated lazily (only when scrolled into view) based on either pixel heights (block collapses) or text length (inline collapses). The module also implements hover-to-expand behavior on desktop, with safeguards to prevent accidental expansion during scrolling.

---

## Public API

### `expandCollapseBlocksToReveal(node, options) → boolean`

Recursively expands all collapse blocks containing the given node. Returns `true` if any expansion occurred.

```javascript
expandCollapseBlocksToReveal(document.getElementById("footnote-5"), {
    fireStateChangedEvent: true  // default
});
```

**Called by:** `revealElement()`, `GW.selectionChangedRevealElement`
**Calls:** `isWithinCollapsedBlock()`, `isCollapsed()`, `toggleCollapseBlockState()`

---

### `collapseCollapseBlock(collapseBlock, options)`

Collapses the specified block and all nested collapse blocks within it.

**Called by:** `GW.contentInjectHandlers.collapseExpandedCollapseBlocks`
**Calls:** `isCollapsed()`, `toggleCollapseBlockState()`

---

### `isCollapsed(collapseBlock) → boolean|undefined`

Returns `true` if collapsed, `false` if expanded, `undefined` if not yet initialized.

```javascript
// State is tracked via classes:
collapseBlock.classList.contains("expanded")     // → false
collapseBlock.classList.contains("expanded-not") // → true
```

**Called by:** Most functions in this module
**Calls:** None

---

### `isWithinCollapsedBlock(element) → boolean`

Returns `true` if the element is inside any currently-collapsed block (checking ancestors recursively).

**Called by:** `expandCollapseBlocksToReveal()`, `GW.selectionChangedRevealElement`
**Calls:** `isCollapsed()`

---

### `revealElement(element, options) → boolean`

Expands collapse blocks to reveal an element and optionally scrolls it into view.

```javascript
revealElement(targetElement, {
    scrollIntoView: true,  // default
    offset: 0              // scroll offset
});
```

**Called by:** `revealTarget()`, `GW.selectionChangedRevealElement`
**Calls:** `expandCollapseBlocksToReveal()`, `scrollElementIntoView()`

---

### `revealTarget(options)`

Expands collapses to reveal the element targeted by the current URL hash.

**Called by:** `GW.revealTargetOnPageLayoutComplete`, `GW.revealTargetOnHashChange`
**Calls:** `getHashTargetedElement()`, `revealElement()`

---

### `expandLockCollapseBlock(collapseBlock)`

Permanently expands a collapse block, removes its disclosure button, and strips all collapse-related classes/wrappers. Used when stripping collapses (e.g., for popups that shouldn't have collapsible content).

**Called by:** `GW.contentInjectHandlers.expandLockCollapseBlocks`
**Calls:** None

---

## Internal Architecture

### State Model

Collapse state is managed via CSS classes:

| Class | Meaning |
|-------|---------|
| `.expanded` | Block is expanded |
| `.expanded-not` | Block is collapsed |
| `.collapse-block` | Block-level collapse |
| `.collapse-inline` | Inline collapse |
| `.has-abstract` | Has preview content |
| `.no-abstract` | No preview content |
| `.expand-on-hover` | Desktop hover behavior enabled |

### DOM Structure After Preparation

**Block collapse:**
```html
<div class="collapse collapse-block expanded-not has-abstract">
  <div class="abstract-collapse">Preview content...</div>
  <button class="disclosure-button">
    <span class="part top"><span class="label">Click to expand</span><span class="icon">...</span></span>
    <span class="part bottom"><span class="label"></span><span class="icon">...</span></span>
    <span class="collapse-iceberg-indicator">...</span>
  </button>
  <div class="collapse-content-wrapper">Hidden content...</div>
</div>
```

**Inline collapse:**
```html
<span class="collapse collapse-inline expanded-not">
  <span class="abstract-collapse-only">…</span>
  <span class="collapse-content-outer-wrapper">
    <button class="disclosure-button start">[</button>
    <span class="collapse-content-wrapper">Hidden text</span>
    <button class="disclosure-button end">→]</button>
  </span>
</span>
```

### Content Load Handlers

The module registers several content handlers at different phases:

| Handler | Phase | Purpose |
|---------|-------|---------|
| `preprocessMismatchedCollapseHTML` | rewrite | Fix malformed abstract/collapse nesting |
| `prepareCollapseBlocks` | rewrite | Build complete collapse DOM structure |
| `rectifySectionCollapseLayout` | \>rewrite | Adjust section heading heights |
| `collapseExpandedCollapseBlocks` | \<eventListeners | Re-collapse blocks when content moves to new context |
| `activateCollapseBlockDisclosureButtons` | eventListeners | Add click/hover handlers |
| `expandLockCollapseBlocks` | \<rewrite | Remove collapses when `stripCollapses` is set |

---

## Key Patterns

### Lazy Iceberg Indicator Calculation

The "iceberg indicator" shows what percentage of content is visible. It's expensive to calculate, so the module uses an IntersectionObserver to defer calculation until the collapse is scrolled into view:

```javascript
function setCollapseBlockIcebergIndicatorUpdateWhenNeeded(collapseBlock) {
    lazyLoadObserver(() => {
        updateCollapseBlockIcebergIndicatorIfNeeded(collapseBlock);
    }, collapseBlock, {
        root: scrollContainerOf(collapseBlock),
        rootMargin: "100%"
    });
}
```

The calculation differs by collapse type:
- **Block with abstract:** `abstractHeight / (abstractHeight + contentHeight)`
- **Block without abstract:** `visibleHeight / totalContentHeight`
- **Inline:** `abstractLength / (abstractLength + contentLength)` (character count)

### Hover Events with Scroll Guard

Desktop collapses expand on hover, but this would be annoying during scrolling. The module disables hover events during scroll, re-enabling on mouse movement:

```javascript
// Disable during scroll
addScrollListener((event) => {
    GW.collapse.hoverEventsActive = false;
});

// Re-enable on mouse move
addMousemoveListener((event) => {
    GW.collapse.hoverEventsActive = true;
});
```

Hover expansion has a 1-second delay and can be cancelled by `mouseleave` or `mousedown`.

### Click Counter for UI Hints

The module tracks how many times users manually expand collapse blocks. After a threshold (3 on desktop, 6 on mobile), disclosure button labels are hidden:

```javascript
GW.collapse = {
    alwaysShowCollapseInteractionHints: (getSavedCount("clicked-to-expand-collapse-block-count") < (GW.isMobile() ? 6 : 3)),
    showCollapseInteractionHintsOnHover: (getSavedCount(...) < 6)
};
```

### XOR State Coupling

Collapse blocks can be linked so that expanding one collapses another:

```javascript
if (collapseBlock.dataset.collapseXorStateWithSelector > "") {
    let otherCollapseElement = collapseBlock.getRootNode()
        .querySelector(collapseBlock.dataset.collapseXorStateWithSelector);
    toggleCollapseBlockState(otherCollapseElement, expanding ? false : true);
}
```

---

## Configuration

### `GW.collapse` Namespace

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `alwaysShowCollapseInteractionHints` | boolean | varies | Always show "Click to expand" labels |
| `showCollapseInteractionHintsOnHover` | boolean | varies | Show labels on hover |
| `hoverEventsEnabled` | boolean | `!GW.isMobile()` | Master hover toggle |
| `hoverEventsActive` | boolean | `!GW.isMobile()` | Current hover state (toggled by scroll/mousemove) |

### Collapse Classes (Author-Applied)

| Class | Effect |
|-------|--------|
| `.collapse` | Marks element as collapsible |
| `.start-expanded` | Start in expanded state |
| `.collapse-small` | Minimal inline collapse (no abstract) |
| `.bare-content-not` | Prevent "bare content" styling |
| `data-collapse-xor-state-with-selector` | CSS selector for XOR-linked collapse |

---

## Integration Points

### Events Fired

| Event | Source | Payload |
|-------|--------|---------|
| `Collapse.collapseStateDidChange` | State toggle, reveal, expand-lock | `{ source, collapseBlock }` |
| `Collapse.targetDidReveal` | Hash target revealed | (none) |

### Events Listened

| Event | Handler | Purpose |
|-------|---------|---------|
| `Popups.popupDidSpawn` | Add scroll listener to popup | Disable hover in popup scroll |
| `GW.hashHandlingSetupDidComplete` | `revealTargetOnPageLayoutComplete` | Reveal hash target on load |
| `GW.hashDidChange` | `revealTargetOnHashChange` | Reveal hash target on navigation |
| `Rewrite.contentDidChange` | `updateIcebergIndicatorsOnContentChangeWithinCollapseBlocks` | Recalc iceberg on content mutation |
| `selectionchange` (document) | `selectionChangedRevealElement` | Expand for Ctrl+F hits |

### Shared State

- **`GW.collapse`**: Module configuration namespace
- **`getSavedCount` / `incrementSavedCount`**: Persistent click counter (localStorage)
- **`GW.TOC.getMainTOC()`**: Used for layout compensation when TOC floats

### Transclusion Integration

The module handles collapse-inducing include-links specially:

```javascript
if (collapseBlock.tagName == "A")
    collapseBlock = wrapElement(wrapElement(collapseBlock, "p", wrapOptions), "div", wrapOptions);
```

It also checks `Transclude.isIncludeLink()` when determining block-level children.

---

## See Also

- [rewrite.js](/frontend/rewrite-js) - DOM transformation pipeline that processes collapses
- [initial.js](/frontend/initial-js) - Core framework that loads collapse module
- [layout.js](/frontend/layout-js) - Block layout system that responds to collapse state changes
- [sidenotes.js](/frontend/sidenotes-js) - Sidenotes reposition when collapses expand/contract
- [content.js](/frontend/content-js) - Content loading phases that trigger collapse preparation
- [transclude.js](/frontend/transclude-js) - Include-links that may create collapses
- [popups.js](/frontend/popups-js) - Popups interact with collapse hover events
