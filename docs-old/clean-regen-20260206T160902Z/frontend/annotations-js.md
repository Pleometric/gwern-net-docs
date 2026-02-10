
# annotations.js

**Path:** `js/annotations.js` | **Language:** JavaScript | **Lines:** ~426

> Client-side annotation data layer: fetches, parses, and caches link metadata from the server's `/metadata/annotation/` API

---

## Overview

annotations.js is the data layer for gwern.net's annotation system. When a user hovers over an annotated link (one with class `link-annotated` or `link-annotated-partial`), this module fetches the pre-generated HTML annotation from the server, parses it into a structured reference data object, and caches it for reuse.

The module follows a strict separation of concerns: it handles *only* data fetching and caching, not display. The actual popup/popin rendering is delegated to extracts-annotations.js, which registers itself with the Extracts system using the data this module provides. This clean split means annotations.js can be tested and reasoned about independently.

The caching strategy is simple but effective: once an annotation is loaded, it stays in memory for the session. Failed loads are also cached (as the string `"LOADING_FAILED"`) to prevent repeated requests to broken URLs. The `waitForDataLoad` pattern enables async coordination without promises—callers register success/failure callbacks that fire when the annotation eventually loads.

---

## Public API

### `isAnnotatedLink(link) → boolean`

Returns true if the link has either `link-annotated` or `link-annotated-partial` class.

**Called by:** extracts-annotations.js, content processors
**Calls:** classList.containsAnyOf

---

### `isAnnotatedLinkFull(link) → boolean`

Returns true only for fully-annotated links (class `link-annotated`).

**Called by:** Extracts.isAnnotatedLink (extracts-annotations.js)

---

### `isAnnotatedLinkPartial(link) → boolean`

Returns true only for partially-annotated links (class `link-annotated-partial`).

**Called by:** Extracts.isPartialAnnotationLink (extracts-annotations.js)

---

### `allAnnotatedLinksInContainer(container) → Array<Element>`

Finds all annotated links within a container element.

**Called by:** Extracts.setUpAnnotationLoadEventsWithin

---

### `targetIdentifier(target) → string`

Computes the cache key for a link. For local links, returns pathname + hash (with `/index` appended to directory paths). For external links, returns the full href.

```javascript
// Local: "/doc/ai/scaling" + "#results" → "/doc/ai/scaling#results"
// External: "https://arxiv.org/abs/..." → "https://arxiv.org/abs/..."
```

**Called by:** sourceURLForLink, referenceDataCacheKeyForLink

---

### `cachedDataExists(link) → boolean`

Returns true if valid (non-failed) cached data exists for this link.

**Called by:** Extracts.setUpAnnotationLoadEventsWithin (to skip already-loaded annotations)

---

### `referenceDataForLink(link) → object | "LOADING_FAILED" | null`

Returns cached reference data, the failure sentinel, or null if not yet loaded.

**Called by:** Extracts.titleForPopFrame_ANNOTATION

---

### `cachedDocumentForLink(link) → Document | null`

Convenience accessor that returns just the parsed document from cached reference data.

---

### `load(link, loadHandler?, loadFailHandler?)`

Initiates an AJAX fetch of the annotation for the given link. On success, parses the response, caches the reference data, and fires `Annotations.annotationDidLoad`. On failure, caches the failure state and fires `Annotations.annotationLoadDidFail`.

```javascript
Annotations.load(link,
    (link) => console.log("Loaded!"),
    (link) => console.log("Failed!")
);
```

**Called by:** Extracts.setUpAnnotationLoadEventsWithin
**Calls:** doAjax, referenceDataFromParsedAPIResponse, cacheReferenceDataForLink, GW.notificationCenter.fireEvent

---

### `waitForDataLoad(link, loadHandler?, loadFailHandler?)`

Registers callbacks to be invoked when an annotation finishes loading (or has already loaded/failed). Uses the notification center for async coordination.

**Called by:** Annotations.load (to wire up caller-provided handlers)
**Calls:** GW.notificationCenter.addHandlerForEvent

---

### `sourceURLForLink(link) → URL`

Constructs the API URL for fetching an annotation. URLs are double-encoded to handle special characters:

```javascript
// Input link to "/doc/ai/scaling#results"
// Output: "/metadata/annotation/%252Fdoc%252Fai%252Fscaling%2523results.html"
```

**Called by:** load, preparePopFrame_ANNOTATION (extracts-annotations.js)

---

## Internal Architecture

### Data Flow

```
User hovers link
       ↓
Extracts.setUpAnnotationLoadEventsWithin registers hover listener
       ↓
After 25ms delay, Annotations.load() called
       ↓
doAjax fetches /metadata/annotation/{encoded-url}.html
       ↓
referenceDataFromParsedAPIResponse parses HTML → structured object
       ↓
cacheReferenceDataForLink stores in cachedReferenceData
       ↓
GW.notificationCenter fires "Annotations.annotationDidLoad"
       ↓
Popup system renders using cached data
```

### Reference Data Structure

The `referenceDataFromParsedAPIResponse` function transforms raw HTML into this structure:

```javascript
{
    document: Document,           // The parsed HTML
    content: {
        title: string,            // Link text (HTML)
        titleLinkHref: string,    // Destination URL
        titleLinkClass: string,   // CSS classes for title link
        titleLinkDataAttributes: string,  // data-* attributes
        author: string,           // Author HTML (if any)
        date: string,             // Date HTML (if any)
        auxLinks: string,         // Tags, backlinks, similars HTML
        authorDateAux: string,    // Combined metadata line
        abstract: string,         // Main annotation body HTML
        thumbnailFigure: string,  // Page thumbnail HTML (if any)
        fileIncludes: string      // Embedded file links HTML (if any)
    },
    template: "annotation-blockquote-inside",
    popFrameTemplate: "annotation-blockquote-not",
    popFrameTitle: string,        // Clean title text
    popFrameTitleLinkHref: string // /ref/ page URL
}
```

### Cache Storage

```javascript
Annotations.cachedReferenceData = {
    "/doc/ai/scaling#results": { /* reference data object */ },
    "https://arxiv.org/abs/...": "LOADING_FAILED",
    // ...
};
```

---

## Key Patterns

### Double URL Encoding

Annotation URLs contain the target URL as a path component. To safely encode URLs with slashes, hashes, and special characters, `sourceURLForLink` applies `fixedEncodeURIComponent` twice:

```javascript
Annotations.basePathname + fixedEncodeURIComponent(fixedEncodeURIComponent(identifier)) + ".html"
```

This produces URLs like `/metadata/annotation/%252Fdoc%252Fai%252Fscaling.html` where `%25` is an encoded `%`.

---

### Failure Caching

Failed loads are cached as the sentinel string `"LOADING_FAILED"` rather than `null`. This distinguishes "never tried" from "tried and failed," preventing repeated requests to broken URLs:

```javascript
if (referenceData == Annotations.loadingFailedString) {
    // Don't retry
}
```

---

### Thumbnail Pre-Caching

When an annotation loads, the module proactively fetches the page thumbnail to warm the browser cache:

```javascript
let pageImage = responseDocument.querySelector(".page-thumbnail");
if (pageImage && Images.isSVG(pageImage) == false)
    doAjax({ location: Images.thumbnailURLForImage(pageImage) });
```

---

### Mobile-Specific Links

Annotations can specify alternate URLs for mobile via `data-href-mobile`:

```javascript
let titleLinkHref = (titleLink.dataset.hrefMobile && GW.isMobile())
    ? titleLink.dataset.hrefMobile
    : titleLink.href;
```

---

## Configuration

| Property | Value | Purpose |
|----------|-------|---------|
| `basePathname` | `/metadata/annotation/` | Server path for annotation API |
| `annotatedLinkFullClass` | `link-annotated` | Class for fully-annotated links |
| `annotatedLinkPartialClass` | `link-annotated-partial` | Class for partial annotations |
| `loadingFailedString` | `"LOADING_FAILED"` | Sentinel for cached failures |

---

## Integration Points

### Events Fired

| Event | Payload | When |
|-------|---------|------|
| `Annotations.didLoad` | none | Module initialization complete |
| `Annotations.annotationDidLoad` | `{ link }` | Annotation fetch succeeded |
| `Annotations.annotationLoadDidFail` | `{ link }` | Annotation fetch failed |

### Events Consumed

None directly—coordination happens through `waitForDataLoad` callbacks.

### Shared State

- **`Annotations.cachedReferenceData`**: The cache object, keyed by target identifier
- **`GW.isMobile()`**: Used for mobile-specific URL selection
- **`GW.mediaQueries.mobileWidth`**: Used for thumbnail figure placement

### Dependencies

- `doAjax`: HTTP request utility
- `newDocument`: DOM parser wrapper
- `GW.notificationCenter`: Event system
- `Images`: Thumbnail URL generation, SVG detection
- `Transclude`: For processing file includes
- `Content`: For content type detection

---

## extracts-annotations.js Integration

The companion file extracts-annotations.js registers two target types with the Extracts system:

1. **ANNOTATION** (full): Triggers on `link-annotated` links
2. **ANNOTATION_PARTIAL** (partial): Triggers on `link-annotated-partial` links

Key integration points:

```javascript
// Loads annotation on hover (after 25ms delay)
Extracts.setUpAnnotationLoadEventsWithin(container)

// Gets cached data for popup title
Annotations.referenceDataForLink(target)

// Checks if data is ready
Annotations.cachedDataExists(annotatedTarget)

// Sets popup base location
Annotations.sourceURLForLink(popFrame.spawningTarget)
```

The hover delay (25ms) prevents unnecessary loads when the cursor passes over links without stopping.

---

## See Also

- [extracts.js](/frontend/extracts-js) - Pop-frame system that displays annotations
- [popups.js](/frontend/popups-js) - Popup positioning and rendering
- [popins.js](/frontend/popins-js) - Mobile-friendly popin system
- [content.js](/frontend/content-js) - Content type system for media annotations
- [transclude.js](/frontend/transclude-js) - Transclusion system for file includes
- [LinkMetadata.hs](/backend/link-metadata-hs) - Server-side annotation database manager
- [Annotation.hs](/backend/annotation-hs) - Server-side annotation scraping dispatcher

---
