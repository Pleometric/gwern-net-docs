
# sidenotes.js

**Path:** `js/sidenotes.js` | **Language:** JavaScript | **Lines:** ~1,350 | **Author:** Said Achmiz (2019)

> Dynamically positions footnotes as margin sidenotes with collision avoidance

---

## Overview

sidenotes.js transforms Pandoc-style footnotes into margin sidenotes when the viewport is wide enough (≥1761px). Unlike Tufte-CSS which requires static inline sidenotes, this system dynamically repositions notes to avoid overlaps, responds to window resizes, and gracefully degrades to traditional footnotes on narrow viewports.

The core challenge is layout: sidenotes want to align with their citations, but when citations cluster densely, notes would overlap. The module solves this with a "layout cell" algorithm that divides the margin into vertical regions, assigns sidenotes to cells, then pushes overlapping notes apart while keeping them as close to their citations as possible.

The system also handles "slidenotes"—temporarily sliding offscreen sidenotes into view when hovering over a citation or the sidenote itself, then returning them to their calculated positions afterward.

---

## Public API

### `Sidenotes.setup()`

Main entry point. Registers media query listeners, content inject handlers, and event listeners. Called automatically on script load.

**Called by:** Self-invoked at end of file
**Calls:** `constructSidenotes`, `updateSidenotePositions`, various event handlers

### `Sidenotes.cleanup()`

Tears down all sidenote infrastructure, removes HTML, deactivates media queries. Used when navigating away or resetting state.

### `Sidenotes.counterpart(element) → Element|null`

Given a sidenote, returns its citation; given a citation, returns its sidenote.

```javascript
let citation = Sidenotes.counterpart(sidenote);  // a.footnote-ref
let sidenote = Sidenotes.counterpart(citation);  // div.sidenote
```

**Called by:** Position calculations, highlight handlers, slide logic

### `Sidenotes.sidenoteOfNumber(number) → Element|null`

Returns the sidenote with the given number.

### `Sidenotes.citationOfNumber(number) → Element|null`

Returns the citation with the given number.

### `Sidenotes.updateSidenotePositions()`

Recalculates and applies positions for all sidenotes. The heavy lifter—runs the layout algorithm, detects cut-off notes, updates back-arrow orientations.

**Called by:** Window resize, content injection, collapse state changes
**Calls:** `updateSidenoteDispositions`, `updateFootnoteBackArrowOrientationForSidenote`

### `Sidenotes.updateSidenotePositionsIfNeeded()`

Queues a position update on the next available animation frame if one isn't already queued. Uses `requestIdleCallback` for debouncing.

### `Sidenotes.slideSidenoteIntoView(sidenote, toCitation)`

Temporarily translates a sidenote vertically to bring it on-screen. If `toCitation` is true, prioritizes aligning with the citation's vertical position.

**Called by:** Mouse enter handlers on citations and sidenotes

### `Sidenotes.putSidenoteBack(sidenote)` / `Sidenotes.putAllSidenotesBack(exceptOne?)`

Reverses the slide transform, returning sidenotes to their calculated positions.

### `Sidenotes.updateStateAfterHashChange()`

Responds to URL hash changes targeting sidenotes or citations. Handles:
- Highlighting the targeted sidenote/citation pair
- Expanding collapse blocks containing the citation
- Scrolling the sidenote into view (with slide-lock to prevent jitter)
- Hiding UI elements that would overlap sidenotes

**Called by:** `GW.hashDidChange` event, self-link clicks

### `Sidenotes.updateTargetCounterpart()`

Updates the `.targeted` class on citations and sidenotes when the URL hash changes to target a note.

### `Sidenotes.hideInterferingUIElements()`

Hides mode selectors and other UI elements that would overlap with a sidenote at the top-right of the page.

---

## Internal Architecture

### State

```javascript
Sidenotes = {
  sidenotes: Element[],           // All sidenote divs, ordered by number
  citations: Element[],           // All citation links (a.footnote-ref)
  sidenoteColumnLeft: Element,    // #sidenote-column-left container
  sidenoteColumnRight: Element,   // #sidenote-column-right container
  displacedSidenotes: Element[],  // Currently slid sidenotes
  positionUpdateQueued: boolean,  // Debounce flag for position updates
  sidenotesNeedConstructing: boolean  // Flag for pending construction
}
```

### Layout Algorithm

The positioning algorithm in `updateSidenotePositions()` works in phases:

1. **Disposition**: Place sidenotes in appropriate columns; hide sidenotes whose citations are in collapsed blocks.

2. **Proscribed Ranges**: Scan for elements that intrude into sidenote columns (full-width images, tables, margin notes). Convert to vertical ranges where sidenotes cannot be placed.

3. **Layout Cells**: Divide each column into vertical "cells"—the gaps between proscribed ranges. Each cell tracks remaining room.

4. **Assignment**: For each sidenote, find fitting cells (enough room for sidenote height). Score cells by:
   - Vertical distance from citation
   - Horizontal distance (for two-column layouts)
   - "Crowdedness"—overlap with sidenotes already assigned to that cell

5. **Positioning within Cells**: Set default position (aligned with citation, or cell top). Sort by position. For overlapping pairs, split the overlap: push upper notes up, shift lower note down.

6. **Bottom Overflow**: If the lowest sidenote extends past cell bottom, push the whole stack upward.

```
┌─────────────────────────────────────────────────────────────────────┐
│  Citation 1 ────────────────────────────────── [Sidenote 1]        │
│                                                                     │
│  Citation 2 ────────────────────────────────── [Sidenote 2]        │
│  Citation 3 ─────────────────────────┐         (pushed down)       │
│                                      └──────── [Sidenote 3]        │
│                                                                     │
│  ═══════════ FULL-WIDTH IMAGE (proscribed) ════════════            │
│                                                                     │
│  Citation 4 ────────────────────────────────── [Sidenote 4]        │
└─────────────────────────────────────────────────────────────────────┘
```

### Sidenote Construction

`constructSidenotes()` creates sidenote elements by:

1. Finding all `a.footnote-ref` citations in injected content
2. Creating wrapper divs with structure:
   ```html
   <div class="sidenote" id="sn42">
     <div class="sidenote-outer-wrapper">
       <div class="sidenote-inner-wrapper">
         <!-- content transcluded from footnote -->
       </div>
     </div>
     <a class="sidenote-self-link" href="#sn42">42</a>
   </div>
   ```
3. Triggering transclusion to pull footnote content into the wrapper

### Mode Switching

Two modes based on viewport width:
- **Sidenote mode** (≥1761px): Sidenotes visible in margins
- **Footnote mode** (&lt;1761px): Traditional footnotes at page bottom

The `mediaQueries.viewportWidthBreakpoint` media query triggers:
- Rewriting citation hrefs (`#fn5` ↔ `#sn5`)
- Rewriting URL hash if it targets a note
- Adding/removing event handlers

---

## Key Patterns

### Collision Avoidance via Push-Up Recursion

When notes overlap, `pushNotesUp()` recursively shifts notes upward:

```javascript
let pushNotesUp = (pushUpWhich, pushUpForce, bruteStrength = false) => {
  let roomToPush = pushUpWhich.first == 0
                   ? cell.sidenotes[pushUpWhich.first].posInCell
                   : Math.max(0, getDistance(cell.sidenotes[pushUpWhich.first - 1],
                                             cell.sidenotes[pushUpWhich.first]));
  // ...
  if (pushUpDistance <= roomToPush) {
    shiftNotesUp(pushUpWhich, pushUpDistance);
    return (pushUpForce - pushUpDistance);
  } else {
    // Recursively add the note above to the push set
    pushUpWhich.splice(0, 0, pushUpWhich.first - 1);
    return pushNotesUp(pushUpWhich, (pushUpForce - roomToPush), bruteStrength);
  }
};
```

This ensures that when one sidenote is pushed up, it can cascade the push to notes above it if needed.

### Slidenotes (Temporary Repositioning)

When a sidenote is offscreen, hovering over its citation or the sidenote itself temporarily slides it into view. This is distinct from the permanent layout positioning.

```javascript
// Slide logic considers both viewport bounds and citation position
if (toCitation) {
  let citationRect = Sidenotes.counterpart(sidenote).getBoundingClientRect();
  newSidenoteTop = Math.max(sidenoteRect.top, minDistanceFromScreenEdge, citationRect.top);
  // ...clamp to viewport bottom and citation position
} else {
  // Just clamp to viewport bounds
  newSidenoteTop = Math.max(sidenoteRect.top, minDistanceFromScreenEdge);
}
sidenote.style.transform = `translateY(${delta}px)`;
sidenote.classList.toggle("displaced", true);
```

Key behaviors:
- **Citation hover**: Slides sidenote to align with citation (within viewport bounds)
- **Sidenote hover**: Slides sidenote just enough to be fully visible
- **Scroll**: All displaced sidenotes return to calculated positions
- **Displaced tracking**: `Sidenotes.displacedSidenotes` array tracks which are currently slid

### Cut-off Detection

Sidenotes exceeding max-height get a scrollable container. The module detects this:

```javascript
sidenote.classList.toggle("cut-off",
  (sidenoteOuterWrapper.scrollHeight > sidenoteOuterWrapper.offsetHeight + 2));
```

When the user scrolls to the bottom of a cut-off sidenote, an ellipsis indicator hides:

```javascript
sidenote.classList.toggle("hide-more-indicator",
  sidenote.outerWrapper.scrollTop + sidenote.outerWrapper.clientHeight == sidenote.outerWrapper.scrollHeight);
```

CSS styling for cut-off sidenotes (in `default.css`) makes the outer wrapper scrollable with `overflow-y: auto` and shows an ellipsis indicator via `::after` pseudo-element.

### Back-Arrow Orientation

The ↩ back-arrow in each sidenote rotates to point at its citation:

```javascript
let x = (citationRect.x + citationRect.width * 0.5) - (sidenoteRect.x + sidenoteRect.width * 0.5);
let y = (sidenoteRect.y + sidenoteRect.height * 0.5) + offset - (citationRect.y + citationRect.height * 0.5);
let rotationAngle = -1 * (modulo(Math.atan2(y, x) * (180 / Math.PI), 360) - 90);
arrow.style.transform = `rotate(${rotationAngle}deg)`;
```

### Debounced Position Updates

Position recalculation is expensive. `updateSidenotePositionsIfNeeded()` uses `requestIdleCallback` to batch updates:

```javascript
if (Sidenotes.positionUpdateQueued) return;
Sidenotes.positionUpdateQueued = true;
requestIdleCallback(() => {
  Sidenotes.positionUpdateQueued = false;
  if (Sidenotes.sidenotesNeedConstructing) return;
  Sidenotes.updateSidenotePositions();
});
```

---

## Configuration

Defined at the top of the `Sidenotes` object:

| Property | Default | Description |
|----------|---------|-------------|
| `sidenoteSpacing` | `60.0` | Minimum vertical gap between adjacent sidenotes (px) |
| `sidenotePadding` | `13.0` | Padding including border (px) |
| `minimumViewportWidthForSidenotes` | `"1761px"` | Below this, sidenotes disabled |
| `minimumViewportWidthForSidenoteMarginNotes` | `"1497px"` | Below this, margin notes inline |
| `potentiallyOverlappingElementsSelectors` | `[".width-full img", ".width-full video", ...]` | Elements that create proscribed zones |
| `constrainMarginNotesWithinSelectors` | `[".backlink-context", ".margin-notes-block", ...]` | Contexts where margin notes stay inline |
| `useLeftColumn` | `() => false` | Whether to use left margin |
| `useRightColumn` | `() => true` | Whether to use right margin |

### Media Queries

```javascript
Sidenotes.mediaQueries = {
  viewportWidthBreakpoint: matchMedia(`(min-width: 1761px)`),
  marginNoteViewportWidthBreakpoint: matchMedia(`(min-width: 1497px)`)
}
```

---

## Integration Points

### Events Fired

| Event | When |
|-------|------|
| `Sidenotes.didLoad` | Script loaded |
| `Sidenotes.setupDidComplete` | Setup finished |
| `Sidenotes.sidenotesDidConstruct` | Sidenotes built/rebuilt |
| `Sidenotes.sidenotePositionsDidUpdate` | Positions recalculated |
| `Sidenotes.cleanupDidComplete` | Teardown finished |

### Events Listened

| Event | Response |
|-------|----------|
| `GW.hashDidChange` | Update targeting, scroll sidenote/citation into view |
| `Rewrite.fullWidthMediaDidLoad` | Recalculate positions (new proscribed zones) |
| `Collapse.collapseStateDidChange` | Recalculate positions (citations revealed/hidden) |
| `Rewrite.contentDidChange` | Recalculate positions (new content may affect layout) |
| `Layout.layoutProcessorDidComplete` | Recalculate positions after block spacing |

### Content Inject Handlers

- `constructSidenotesWhenMainPageContentDidInject` — Build sidenotes when page content loads
- `setMarginNoteStyle` — Set margin notes to inline or sidenote style
- `addFauxHashChangeEventsToNoteMetaLinks` — Handle same-hash clicks
- `addMediaElementLoadEventsInSidenotes` — Reposition after images load inside sidenotes
- `bindAdditionalSidenoteSlideEvents` — Slide events for citations in popups

### Shared State

- Uses `Notes.noteNumber()`, `Notes.footnoteIdForNumber()`, `Notes.sidenoteIdForNumber()` from notes system
- Reads collapse state via `isWithinCollapsedBlock()`
- Triggers transclusion via `Transclude.triggerTransclude()`
- Uses `revealElement()` from utility.js for expanding collapsed blocks

---

## See Also

- [layout.js](/frontend/layout-js) - Block layout system that triggers sidenote repositioning
- [rewrite.js](/frontend/rewrite-js) - DOM transforms including citation highlight bindings
- [initial.js](/frontend/initial-js) - Core framework that loads sidenotes module
- [collapse.js](/frontend/collapse-js) - Collapse blocks that affect sidenote visibility
- [transclude.js](/frontend/transclude-js) - Content transclusion for sidenote content
- [extracts.js](/frontend/extracts-js) - Popup system (alternative to sidenotes)
- [typography.js](/frontend/typography-js) - Text processing used in sidenote captions
