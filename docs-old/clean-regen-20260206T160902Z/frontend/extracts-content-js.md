
# extracts-content.js

**Path:** `js/extracts-content.js` | **Language:** JavaScript | **Lines:** ~1,178

> Content type definitions and fill functions for the extract/popup system

---

## Overview

This module defines the *content types* that the extract system can display. While `extracts.js` provides the pop-frame infrastructure (popups/popins), this file specifies *what* content appears for each kind of link. Each content type is a five-element array registered into `Extracts.targetTypeDefinitions`:

1. **Type name** (e.g., `"LOCAL_PAGE"`)
2. **Predicate function name** (e.g., `"isLocalPageLink"`)
3. **Target classes to add** (e.g., `"has-content"`)
4. **Fill function name** (e.g., `"localPageForTarget"`)
5. **Pop-frame classes** (string or function returning string)

For each type, the module provides a predicate (`isXxxLink`) that tests whether a link matches, a fill function (`xxxForTarget`) that returns a DocumentFragment with an include-link for transclusion, and optional `testTarget_`, `preparePopup_`, `rewritePopFrameContent_`, `titleForPopFrame_`, and `updatePopFrame_` hooks.

The ordering of type definitions matters: types are inserted *before* `"LOCAL_PAGE"` (or `"ANNOTATION_PARTIAL"` for `LOCAL_PAGE` itself), so more specific types (annotations, videos, images) match before the generic local-page fallback.

---

## Public API

### Content Type Definitions

Each type is registered via `Extracts.targetTypeDefinitions.insertBefore(...)`:

| Type Name | Predicate | Fill Function | Use Case |
|-----------|-----------|---------------|----------|
| `LOCAL_PAGE` | `isLocalPageLink` | `localPageForTarget` | Same-site pages, anchors |
| `AUX_LINKS` | `isAuxLinksLink` | `auxLinksForTarget` | Backlinks, similars, bibliography |
| `DROPCAP_INFO` | `isDropcapInfoLink` | `dropcapInfoForTarget` | Drop-cap letter info |
| `FOOTNOTE` | `isFootnoteLink` | `footnoteForTarget` | Footnote/sidenote refs |
| `CITATION_CONTEXT` | `isCitationContextLink` | `citationContextForTarget` | Footnote back-links |
| `REMOTE_IMAGE` | `isRemoteImageLink` | `remoteImageForTarget` | External images |
| `REMOTE_VIDEO` | `isRemoteVideoLink` | `remoteVideoForTarget` | YouTube, Vimeo |
| `CONTENT_TRANSFORM` | `isContentTransformLink` | `contentTransformForTarget` | Tweets, Wikipedia, GitHub issues |
| `LOCAL_VIDEO` | `isLocalVideoLink` | `localVideoForTarget` | Self-hosted videos |
| `LOCAL_AUDIO` | `isLocalAudioLink` | `localAudioForTarget` | Self-hosted audio |
| `LOCAL_IMAGE` | `isLocalImageLink` | `localImageForTarget` | Self-hosted images |
| `LOCAL_DOCUMENT` | `isLocalDocumentLink` | `localDocumentForTarget` | PDFs, HTML embeds |
| `LOCAL_CODE_FILE` | `isLocalCodeFileLink` | `localCodeFileForTarget` | Code files |
| `FOREIGN_SITE` | `isForeignSiteLink` | `foreignSiteForTarget` | External iframes |

---

### `Extracts.setUpContentLoadEventsWithin(container)`

Sets up preloading for content-bearing links within `container`. On desktop (Popups), adds `mouseenter` listeners with a 25ms delay to preload content before popup spawn. On mobile (Popins), adds `click` listeners.

**Called by:** `extracts.js` during setup
**Calls:** `Content.load()`, `Content.cachedDataExists()`

---

## Internal Architecture

### Type Definition Registration

All types insert themselves *before* `"LOCAL_PAGE"` in the definitions array, ensuring specific matchers (images, videos) take precedence over the generic page handler:

```javascript
Extracts.targetTypeDefinitions.insertBefore([
    "LOCAL_VIDEO",
    "isLocalVideoLink",
    "has-content",
    "localVideoForTarget",
    (popFrame) => ["video object", ...].join(" ")
], (def => def[0] == "LOCAL_PAGE"));
```

### Fill Function Pattern

All fill functions follow the same pattern: synthesize an include-link with appropriate classes and return it wrapped in a DocumentFragment:

```javascript
localVideoForTarget: (target) => {
    return newDocument(synthesizeIncludeLink(target, {
        "class": "include-caption-not include-strict include-spinner-not"
    }));
}
```

The include-link is later processed by `transclude.js` to actually fetch and inject the content.

### Lifecycle Hooks

For each type `FOO`, the following optional hooks can be defined:

| Hook | When Called | Purpose |
|------|-------------|---------|
| `testTarget_FOO` | After predicate matches | Additional exclusion logic |
| `preparePopFrame_FOO` | Before popup/popin spawns | Modify pop-frame, return `null` to cancel |
| `preparePopup_FOO` | Before popup spawns | Popup-specific preparation |
| `preparePopin_FOO` | Before popin spawns | Popin-specific preparation |
| `titleForPopFrame_FOO` | When setting title | Returns title HTML |
| `updatePopFrame_FOO` | After content loads | Add classes, update title |
| `rewritePopFrameContent_FOO` | After injection | DOM manipulations |
| `rewritePopupContent_FOO` | After injection | Popup-specific rewrites |
| `rewritePopinContent_FOO` | After injection | Popin-specific rewrites |

---

## Key Patterns

### Local Page Content Detection

`isLocalPageLink` distinguishes between anchor-links to the current page and links to other site pages:

```javascript
isLocalPageLink: (target) => {
    return (Content.contentTypes.localPage.matches(target)
         && (isAnchorLink(target) || target.pathname != location.pathname));
}
```

### Full-Page vs. Section Display

`localPageForTarget` decides whether to show an entire page or just a targeted section based on:
- Whether the link is an anchor-link
- Whether target document is already displayed (e.g., in another popup)
- Whether the link is from a TOC

```javascript
let fullPage = !(isAnchorLink(target)
              && (target.closest(".TOC") || Extracts.targetDocument(target)));
```

### Popup Suppression

Several types suppress popup creation when the target is already visible:

```javascript
// FOOTNOTE: Don't spawn if sidenote visible
if (Notes.allNotesForCitation(target).findIndex(note =>
    Popups.isVisible(note)) !== -1)
    return null;

// CITATION_CONTEXT: Don't spawn if citation visible
if (targetElement && Popups.isVisible(targetElement))
    return null;
```

### Object Resizing in Popups

`resizeObjectInObjectPopup()` scales images/videos to fit popup constraints while optionally loosening aspect ratio limits for non-standard media:

```javascript
if (options.loosenSizeConstraints) {
    let popupMaxArea = popupMaxWidth * popupMaxHeight;
    popupMaxWidth = Math.sqrt(popupMaxArea / (popupHeight / popupWidth));
    popupMaxHeight = popupMaxWidth * (popupHeight / popupWidth);
    // Clamp to window dimensions...
}
```

### Transclude Synchronization

For local page popups, `preparePopFrame_LOCAL_PAGE` sets up a handler to synchronize transclusions: when a transclude fires inside a popup, the same transclude in the cached source content also fires.

---

## Configuration

### CSS Custom Properties

Used by `resizeObjectInObjectPopup()`:
- `--GW-popups-popup-max-width` — Maximum popup width
- `--GW-popups-popup-max-height` — Maximum popup height
- `--GW-popups-popup-border-width` — Border thickness

### Constants

```javascript
Extracts.contentLoadHoverDelay = 25;  // ms before preload on hover
```

### Pop-Frame Class Functions

Several types compute their pop-frame classes dynamically:

```javascript
(popFrame) => [
    "footnote",
    (Extracts.popFrameProvider == Popups ? "mini-title-bar" : "no-footer-bar")
].join(" ")
```

---

## Integration Points

### Dependencies

- **Content.contentTypes.\*** — Type matchers (`localPage.matches()`, etc.)
- **Content.load()** / **Content.cachedDataExists()** — Content fetching
- **Content.referenceDataForLink()** — Metadata retrieval
- **Transclude.fillTemplateNamed()** — Title templating
- **AuxLinks.auxLinksLinkType()** — Aux-link type detection
- **Notes.noteNumber()** / **Notes.allNotesForCitation()** — Footnote utilities
- **Popups.isVisible()** — Visibility checks
- **synthesizeIncludeLink()** — Creates transclusion links
- **newDocument()** — DocumentFragment wrapper

### Events Listened

- `GW.contentDidInject` — Triggers transclude sync in source content
- `Popups.popupWillDespawn` / `Popins.popinWillDespawn` — Cleanup handlers

### Events Fired

None directly; relies on events from `extracts.js` and `transclude.js`.

### Shared State

- `Extracts.targetTypeDefinitions` — Registry of content types
- `Extracts.popFrameProvider` — Either `Popups` or `Popins`
- `Extracts.rootDocument` — Root document reference

---

## See Also

- [extracts.js](/frontend/extracts-js) - Pop-frame infrastructure and target processing
- [content.js](/frontend/content-js) - Content type system and data providers
- [transclude.js](/frontend/transclude-js) - Content transclusion engine
- [popups.js](/frontend/popups-js) - Popup windowing system
- [extracts-annotations.js](/frontend/extracts-annotations-js) - Annotation content types
- [popins.js](/frontend/popins-js) - Mobile popin display system
