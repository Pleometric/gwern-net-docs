
# extracts-annotations.js

**Path:** `js/extracts-annotations.js` | **Language:** JavaScript | **Lines:** ~296

> Annotation-specific content type definitions and popup handling for the extract system

---

## Overview

This module registers annotation content types with the extract system, enabling popups and popins to display bibliographic annotation data. It defines two related types: `ANNOTATION` for fully-annotated links (marked with `link-annotated`) and `ANNOTATION_PARTIAL` for partially-annotated links (`link-annotated-partial`).

The core pattern mirrors other extract content types: each type is a five-element array specifying a name, predicate, CSS classes, fill function, and pop-frame styling. The fill functions synthesize include-links that trigger the transclusion system to fetch and render annotation HTML from `/metadata/annotation/`.

A key innovation is the `injectPartialAnnotationMetadata` rewrite, which appends partial annotation metadata to *any* popup whose spawning target has partial annotation data, even if the popup itself is a different content type (e.g., a local page). This ensures citation metadata surfaces consistently across popup types.

---

## Public API

### Type Definitions

Two types are registered into `Extracts.targetTypeDefinitions`:

| Type Name | Predicate | Fill Function | CSS Classes |
|-----------|-----------|---------------|-------------|
| `ANNOTATION` | `isAnnotatedLink` | `annotationForTarget` | `has-annotation` / `annotation` |
| `ANNOTATION_PARTIAL` | `isPartialAnnotationLink` | `partialAnnotationForTarget` | `has-annotation-partial` / `annotation annotation-partial` |

---

### `Extracts.isAnnotatedLink(target) -> boolean`

Tests whether `target` is a fully-annotated link.

**Called by:** `extracts.js` (predicate dispatch)
**Calls:** `Annotations.isAnnotatedLinkFull()`

---

### `Extracts.isPartialAnnotationLink(target) -> boolean`

Tests whether `target` is a partially-annotated link.

**Called by:** `extracts.js` (predicate dispatch)
**Calls:** `Annotations.isAnnotatedLinkPartial()`

---

### `Extracts.annotationForTarget(target) -> DocumentFragment`

Creates a DocumentFragment containing a synthesized include-link for full annotation transclusion.

**Called by:** `extracts.js` (as fill function)
**Calls:** `synthesizeIncludeLink()`, `newDocument()`

---

### `Extracts.partialAnnotationForTarget(target) -> DocumentFragment`

Creates a DocumentFragment for partial annotation transclusion.

**Called by:** `extracts.js` (as fill function)
**Calls:** `synthesizeIncludeLink()`, `newDocument()`

---

### `Extracts.setUpAnnotationLoadEventsWithin(container)`

Sets up preloading for annotated links within `container`. On desktop (Popups mode), adds `mouseenter` listeners with a 25ms delay to pre-fetch annotation data. On mobile (Popins mode), adds `click` listeners instead.

**Called by:** `extracts.js` during setup
**Calls:** `Annotations.allAnnotatedLinksInContainer()`, `Annotations.load()`, `Annotations.cachedDataExists()`

---

## Internal Architecture

### Type Registration Pattern

Types are pushed directly onto `Extracts.targetTypeDefinitions`:

```javascript
Extracts.targetTypeDefinitions.push([
    "ANNOTATION",               // Type name
    "isAnnotatedLink",          // Predicate function name
    "has-annotation",           // Target classes to add
    "annotationForTarget",      // Fill function name
    "annotation"                // Pop-frame classes
]);
```

### Fill Function Pattern

Both fill functions synthesize include-links with specific class markers:

```javascript
annotationForTarget: (target) => {
    return newDocument(synthesizeIncludeLink(target, {
        "class": "link-annotated include-annotation include-strict include-spinner-not",
        "data-include-template": "$popFrameTemplate"
    }));
}
```

The `$popFrameTemplate` placeholder is resolved by the transclusion system based on context.

### Lifecycle Hooks

For each type, the module defines these hooks:

| Hook | Purpose |
|------|---------|
| `testTarget_ANNOTATION` | Excludes TOC and sidebar links in popin mode |
| `titleForPopFrame_ANNOTATION` | Renders title from annotation reference data |
| `preparePopFrame_ANNOTATION` | Sets base location to annotation source URL |
| `preparePopup_ANNOTATION` | Suppresses popup if annotation already visible |
| `updatePopFrame_ANNOTATION` | Updates title after content loads |
| `rewritePopFrameContent_ANNOTATION` | Rearranges media annotations |

The `ANNOTATION_PARTIAL` hooks delegate to their `ANNOTATION` counterparts.

---

## Key Patterns

### Popup Suppression Logic

`preparePopup_ANNOTATION` implements duplicate-suppression by checking two conditions:

1. **Link bibliography visibility:** If the same URL appears in a visible link-bibliography section with a blockquote, the popup is suppressed:

```javascript
let escapedLinkURL = CSS.escape(decodeURIComponent(popup.spawningTarget.href));
let targetAnalogueInLinkBibliography = document.querySelector(
    `a[id^='link-bibliography'][href='${escapedLinkURL}']`
);
if (targetAnalogueInLinkBibliography) {
    let containingSection = targetAnalogueInLinkBibliography.closest("section");
    if (containingSection?.querySelector("blockquote") && Popups.isVisible(containingSection))
        return null;
}
```

2. **Blog page match:** If the current page is `/blog/X` and the popup target ID is `gwern-X`, the popup is suppressed (the annotation is already the main content).

### Media Annotation Rearrangement

`rewritePopFrameContent_ANNOTATION` special-cases media annotations (images, videos, audio). It moves the media include-link from `.file-includes` into `.annotation-abstract`, positioning it before `.aux-links-append` and suppressing duplicate captions:

```javascript
if ([ "remoteImage", "remoteVideo", "localImage", "localVideo", "localAudio" ]
    .findIndex(x => Content.contentTypes[x].matches(popFrame.spawningTarget)) !== -1) {
    let annotationAbstract = contentContainer.querySelector(".annotation-abstract");
    let fileIncludes = contentContainer.querySelector(".file-includes");
    let includeLink = fileIncludes.querySelector("a");
    includeLink.classList.add("include-caption-not");
    annotationAbstract.insertBefore(includeLink, annotationAbstract.querySelector(".aux-links-append"));
    fileIncludes.remove();
}
```

### Partial Annotation Injection

The `injectPartialAnnotationMetadata` rewrite (added to `Extracts.additionalRewrites`) appends partial annotation metadata to *any* popup for a target with partial annotations, not just `ANNOTATION_PARTIAL` popups. This creates a footer section:

```javascript
let partialAnnotationAppendContainer = newElement("DIV", {
    "class": [ "partial-annotation-append-container", "markdownBody",
               "popframe-body", "popframe-footer", "popup-body" ].join(" ")
});
partialAnnotationAppendContainer.appendChild(synthesizeIncludeLink(target.href, {
    "class": "link-annotated-partial include-annotation-partial include-strict",
    "data-include-template": "annotation-blockquote-inside"
}));
Extracts.popFrameProvider.addPartToPopFrame(popFrame, partialAnnotationAppendContainer);
```

---

## Configuration

### Constants

```javascript
Extracts.annotationLoadHoverDelay = 25;  // ms before annotation preload on hover
```

### Include Link Classes

| Class | Meaning |
|-------|---------|
| `include-annotation` | Full annotation transclusion |
| `include-annotation-partial` | Partial annotation transclusion |
| `include-strict` | Force immediate transclusion at load time (not lazy); implies `include-even-when-collapsed` |
| `include-spinner-not` | Don't show loading spinner |
| `include-caption-not` | Suppress caption rendering |

### Template References

- `$popFrameTemplate` — Resolved dynamically based on popup/popin context
- `annotation-blockquote-inside` — Template for inline partial annotation display

---

## Integration Points

### Dependencies

- **Annotations.isAnnotatedLinkFull()** / **Annotations.isAnnotatedLinkPartial()** — Link classification
- **Annotations.allAnnotatedLinksInContainer()** — Finding annotated links
- **Annotations.load()** / **Annotations.cachedDataExists()** — Data fetching/caching
- **Annotations.referenceDataForLink()** — Metadata retrieval
- **Annotations.sourceURLForLink()** — Annotation resource URL
- **Transclude.fillTemplateNamed()** — Title rendering
- **Transclude.triggerTranscludesInContainer()** — Initiating transclusion
- **Content.contentTypes.\*** — Media type detection
- **Popups.isVisible()** — Visibility checks
- **synthesizeIncludeLink()** / **newDocument()** — DOM utilities

### Events Listened

- `Extracts.cleanupDidComplete` — Removes hover/click event listeners from annotated targets

### Events Fired

None directly; annotation loading fires events through `Annotations.load()`:
- `Annotations.annotationDidLoad`
- `Annotations.annotationLoadDidFail`

### Shared State

- `Extracts.targetTypeDefinitions` — Type registry (mutated by push)
- `Extracts.additionalRewrites` — Rewrite hooks (mutated by push)
- `Extracts.popFrameProvider` — Either `Popups` or `Popins`

---

## See Also

- [extracts.js](/frontend/extracts-js) - Pop-frame infrastructure and target processing
- [annotations.js](/frontend/annotations-js) - Annotation data loading and caching
- [extracts-content.js](/frontend/extracts-content-js) - Non-annotation content types
- [transclude.js](/frontend/transclude-js) - Content transclusion engine
- [popups.js](/frontend/popups-js) - Popup windowing system
- [Annotation.hs](/backend/annotation-hs) - Backend annotation scraping and processing
- [LinkMetadata.hs](/backend/link-metadata-hs) - Backend metadata database management
