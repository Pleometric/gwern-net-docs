
# image-focus.js

**Path:** `js/image-focus.js` | **Language:** JavaScript | **Lines:** ~980

> Lightbox image viewer with zoom, pan, keyboard navigation, and gallery slideshow

---

## Overview

image-focus.js provides a full-featured lightbox system for viewing images at full size. When a user clicks on an image in the page content, it opens in a modal overlay with zoom/pan capabilities. The module supports both single-image viewing and a gallery slideshow mode that allows navigation between all images on the page.

The implementation is entirely dependency-free (aside from the core GW utilities) and handles the full lifecycle: detecting focusable images, creating the overlay UI, managing zoom via scroll wheel, panning via drag, keyboard shortcuts, and automatic UI hide/show. It also integrates with the URL hash to allow deep-linking to specific gallery slides (`#if_slide_N`).

Key design decisions include: (1) preloading adjacent images in slideshow mode for smooth navigation, (2) intelligent caption deduplication using Jaccard similarity to avoid showing redundant figcaption/alt/title text, (3) special handling for SVG images which lack intrinsic dimensions, and (4) a responsive approach that resets position on orientation change.

---

## Public API

### `ImageFocus.setup()`

Initializes the image focus system. Creates the overlay DOM, sets up event handlers for orientation changes and hash navigation, and processes existing images. Called automatically on script load.

**Called by:** Self-invoked at module load
**Calls:** `addUIElement`, `doWhenMatchMedia`, `addContentInjectHandler`, `GW.notificationCenter.addHandlerForEvent`

---

### `ImageFocus.processImagesWithin(container)`

Scans a container for focusable images and sets them up for lightbox viewing. Adds `.focusable` and `.gallery-image` classes, attaches click handlers, sets up hover preloading, and wraps images in a span.

**Called by:** `processImagesOnContentInject`
**Calls:** `designateSmallImageIfNeeded`, `wrapElement`

---

### `ImageFocus.focusImage(imageToFocus, scrollToImage = true)`

Opens an image in the lightbox. Shows the overlay, creates the full-size image element, sets caption, updates gallery state if applicable, and fires the `ImageFocus.imageDidFocus` event.

**Called by:** `imageClickedToFocus`, `focusNextImage`, `focusImageSpecifiedByURL`
**Calls:** `enterImageFocus`, `unfocusImage`, `resetFocusedImagePosition`, `setImageFocusCaption`, `preloadImage`

---

### `ImageFocus.exitImageFocus()`

Closes the lightbox. Removes event listeners, hides the overlay, restores page scrolling, preserves "last-focused" state for gallery images, and resets the URL hash. Fires `ImageFocus.imageOverlayDidDisappear`.

**Called by:** `keyUp` (Escape), `mouseUp` (click outside)
**Calls:** `unfocusImage`, `relocate`

---

### `ImageFocus.focusNextImage(next = true)`

Navigates to the next (`next=true`) or previous (`next=false`) image in the gallery.

**Called by:** `keyUp` (arrow keys), `slideshowButtonClicked`
**Calls:** `focusImage`

---

### `ImageFocus.resetFocusedImagePosition(updateOnLoad = false)`

Resets the focused image to fit within the viewport, centered. Handles SVGs specially via aspect-ratio, and for regular images waits for load if dimensions are unknown.

**Called by:** `focusImage`, `keyUp` (spacebar), `doubleClick`, orientation change handler
**Calls:** `setFocusedImageCursor`, `expectedDimensionsForImage`

---

## Internal Architecture

### State Variables

| Variable | Type | Purpose |
|----------|------|---------|
| `overlay` | Element | The `#image-focus-overlay` DOM element |
| `currentlyFocusedImage` | Element | Reference to the original image in the page |
| `imageInFocus` | Element | The cloned/full-size image displayed in overlay |
| `hideUITimer` | number | Timer ID for auto-hiding UI elements |
| `mouseLastMovedAt` | Date | Timestamp for UI hide logic |
| `savedHash` | string | URL hash before entering gallery mode |

### Selectors (Generated at Setup)

- `focusableImagesSelector` — Images that can be clicked to focus (`.markdownBody figure img.focusable`)
- `focusedImageSelector` — Currently focused image (`.focused` suffix)
- `galleryImagesSelector` — Images included in gallery navigation (`.gallery-image` suffix)

### Control Flow: Focusing an Image

```
imageClickedToFocus(event)
  └─> focusImage(image)
        ├─> enterImageFocus()     // Show overlay, add listeners
        ├─> unfocusImage()        // Clear any previous
        ├─> Create <img> from focusedImgSrcForImage()
        ├─> resetFocusedImagePosition()
        ├─> setImageFocusCaption()
        └─> Fire "ImageFocus.imageDidFocus"
```

### Control Flow: Zoom with Scroll Wheel

```
scrollEvent(event)
  ├─> preventDefault()
  ├─> Calculate zoom factor from deltaY
  ├─> Resize image width (height: auto)
  ├─> Determine zoom origin (cursor, window center, or image center)
  ├─> Adjust left/top to keep zoom centered on origin
  ├─> If image < window, nudge toward center
  └─> setFocusedImageCursor()
```

### Control Flow: Pan via Drag

```
imageMouseDown(event)
  ├─> Save mouse coords, image position, filter
  └─> Set window.onmousemove to update image left/top

mouseUp(event)
  └─> Clear window.onmousemove, restore filter
```

---

## Key Patterns

### Intelligent Image Source Resolution

`focusedImgSrcForImage()` determines the best full-size URL for an image:
1. **Wikipedia thumbnails** — Strips `/thumb/` path component to get original
2. **srcset** — Parses and selects highest-resolution variant
3. **data-src-size-full** — Custom attribute for explicit full-size URL
4. **Fallback** — Uses the original `src`

### Caption Deduplication via Jaccard Similarity

The caption can come from three sources: `<figcaption>`, `title` attribute, and `alt` attribute. These often contain overlapping or identical text. Rather than simple string equality, the code uses Jaccard similarity (set intersection / set union of words) with a 0.80 threshold to identify duplicates. This handles:
- Pandoc-generated alt text (which is often plaintext version of caption)
- Minor wording differences from copyediting
- Substrings and prefixes/suffixes

### Preloading Strategy

- On hover over focusable image (after 25ms delay), preload full-size version via `doAjax`
- When an image is focused, preload both adjacent images in the gallery
- Uses `loading="eager"` and `decoding="sync"` on the focused image element

### SVG Dimension Handling

SVGs have no intrinsic size, so the code:
1. Checks for `data-aspect-ratio` attribute
2. If present, sets CSS `aspect-ratio` and calculates dimensions from viewport height
3. If absent, uses the smaller of viewport width/height as both dimensions

### UI Auto-Hide

The overlay UI (buttons, caption, image counter) auto-hides after a timer (3s desktop, 5s mobile). Mouse movement resets the timer. On mobile, tapping when UI is hidden unhides it; tapping on caption "locks" it visible.

---

## Configuration

All configuration lives in the `ImageFocus` object at the top of the file:

| Property | Default | Purpose |
|----------|---------|---------|
| `contentImagesSelector` | `.markdownBody figure img` | Which images are focusable |
| `excludedContainerElementsSelector` | `a, button, figure.image-focus-not` | Exclude images inside these |
| `imageGalleryInclusionTest` | Function | Test if image joins main gallery |
| `shrinkRatio` | `0.975` | Max image size relative to viewport |
| `hideUITimerDuration` | 3000 (5000 mobile) | MS before UI auto-hides |
| `dropShadowFilterForImages` | CSS filter | Shadow applied to focused images |
| `hoverCaptionWidth/Height` | 175 / 75 | Threshold for `.small-image` class |
| `fullSizeImageLoadHoverDelay` | 25 | MS hover before preload triggers |

---

## Integration Points

### Events Fired

| Event | Payload | When |
|-------|---------|------|
| `ImageFocus.didLoad` | — | Script loaded |
| `ImageFocus.setupDidComplete` | — | Setup finished |
| `ImageFocus.imagesDidProcessOnContentInject` | `{container, document}` | Images processed in new content |
| `ImageFocus.imageDidFocus` | `{image}` | Image opened in lightbox |
| `ImageFocus.imageDidUnfocus` | `{image}` | Image removed from lightbox |
| `ImageFocus.imageOverlayDidAppear` | — | Overlay shown |
| `ImageFocus.imageOverlayDidDisappear` | — | Overlay hidden |

### Events Listened To

| Event | Handler |
|-------|---------|
| `GW.hashDidChange` | `focusImageSpecifiedByURL` — opens image if hash is `#if_slide_N` |

### Content Injection

Registers via `addContentInjectHandler` to process images in dynamically loaded content (popups, transclusions, etc.).

### URL Hash Integration

- Entering gallery mode sets hash to `#if_slide_N` (1-indexed)
- Exiting restores the previous hash
- On page load, checks for `#if_slide_N` and auto-focuses that image

### Dependencies

- **GW namespace** — `GW.isMobile()`, `GW.svg()`, `GW.mediaQueries`, `GW.notificationCenter`, `GW.defaultImageAuxText`
- **Utility functions** — `addUIElement`, `doWhenMatchMedia`, `wrapElement`, `revealElement`, `togglePageScrolling`, `doAjax`, `URLFromString`, `relocate`, `copyTextToClipboard`, `jaccardSimilarity`, `addMousemoveListener`, `onEventAfterDelayDo`
- **Typography** — `Typography.processElement` for cleaning caption text
- **Content** — `Content.objectHTMLForURL` for PDF display

---

## See Also

- [Image.hs](/backend/image-hs) - Server-side image processing (dimensions, inversion detection)
- [rewrite.js](/frontend/rewrite-js) - DOM transforms that set up figure structure
- [initial.js](/frontend/initial-js) - Core framework that loads image-focus module
- [typography.js](/frontend/typography-js) - Used for processing caption text
- [popups.js](/frontend/popups-js) - Popup system (different interaction model)
- [content.js](/frontend/content-js) - Content loading (provides `objectHTMLForURL`)
- [dark-mode.js](/frontend/dark-mode-js) - Dark mode affects image inversion display
