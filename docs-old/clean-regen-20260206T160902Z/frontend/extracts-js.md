
# extracts.js

**Path:** `js/extracts.js` | **Language:** JavaScript | **Lines:** ~952

Coordinator that bridges content providers with popup/popin display systems.

---

## Overview

extracts.js is the orchestration layer for gwern.net's popup/popin system. It sits between content.js (which knows *what* to show) and popups.js/popins.js (which know *how* to show it), coordinating the entire extract display pipeline.

The module operates in two modes: popups (desktop) or popins (mobile), selected automatically at setup based on viewport size and device detection. This single codebase handles both paradigms through a provider abstraction—`Extracts.popFrameProvider` points to either the `Popups` or `Popins` object, and type-suffixed function names (`preparePopup_X` vs `preparePopin_X`) allow mode-specific behavior where needed.

The heart of the system is the target type registry (`targetTypeDefinitions`), which maps link types to their handling logic. When a link is hovered (desktop) or clicked (mobile), extracts.js identifies its type, synthesizes an include-link for the transclude system, fills the pop-frame with that content, and applies type-specific rewrites. This design achieves remarkable flexibility—15+ distinct content types are handled through a single unified pipeline.

---

## Public API

### `Extracts.setup()`

Initializes the extract system. Called by extracts-load.js after dependencies are ready.

- Detects mobile/desktop mode and sets `popFrameProvider`
- Injects mode selector UI
- Registers `GW.contentDidInject` handler to process new targets
- Calls `popFrameProvider.setup()`

**Called by:** extracts-load.js, extracts-options.js

### `Extracts.cleanup()`

Tears down the extract system. Used when user disables popups/popins.

- Removes indicator hooks from all targets
- Unbinds event listeners
- Removes pop-frames and containers
- Fires `Extracts.cleanupDidComplete` event

**Called by:** extracts-options.js

### `Extracts.targetTypeInfo(target) → Object|null`

Returns type metadata for a target link, or null if the target shouldn't spawn a pop-frame.

```javascript
{
    typeName: "ANNOTATION",           // Internal type identifier
    predicateFunctionName: "isAnnotatedLink",
    targetClasses: "has-annotation",  // Classes added to target element
    popFrameFillFunctionName: "annotationForTarget",
    popFrameClasses: "annotation"     // Classes added to pop-frame
}
```

**Called by:** Most functions in the extracts system

### `Extracts.fillPopFrame(popFrame) → boolean`

Fills a pop-frame with content based on its spawning target's type. Returns true on success.

Looks up the fill function from `targetTypeInfo`, calls it to get a DocumentFragment, and passes that to `popFrameProvider.setPopFrameContent()`.

**Called by:** `preparePopFrame`, `refreshPopFrameAfterLocalPageLoads`

### `Extracts.titleForPopFrame(popFrame, titleHTML?) → Element`

Returns the title bar element for a pop-frame. Dispatches to type-specific title functions (`titleForPopFrame_ANNOTATION`, etc.) or falls back to `standardPopFrameTitleElementForTarget`.

**Called by:** `preparePopup`, `preparePopin`, `rewritePopinContent`

---

## Internal Architecture

### Target Type Definitions

The system is built around `Extracts.targetTypeDefinitions`, an ordered array where each entry defines a content type:

```javascript
[
    "LOCAL_PAGE",          // Type name (used for function dispatch)
    "isLocalPageLink",     // Predicate function name
    "has-content",         // CSS classes for target
    "localPageForTarget",  // Fill function name
    "local-page"           // CSS classes for pop-frame (string or function)
]
```

**Type resolution is order-dependent**—the first matching predicate wins. This is why extracts-content.js uses `insertBefore` to position new types before `LOCAL_PAGE` and `ANNOTATION_PARTIAL`.

### Pop-Frame Preparation Flow

```
Target hovered/clicked
        ↓
testTarget(target)           → Validates target is eligible
        ↓
preparePopupTarget/          → Mode-specific target prep (removes title attr, etc.)
preparePopinTarget
        ↓
popFrameProvider.addTarget() → Registers with Popups or Popins
        ↓
[user triggers popup/popin]
        ↓
preparePopup/preparePopin    → Main entry point from provider
        ↓
preparePopFrame              → Generic pop-frame setup:
    ├─ fillPopFrame          →   Calls type-specific fill function
    ├─ setLoadingSpinner     →   Shows loading state
    ├─ {type}TitleBarContents→   Builds title bar
    ├─ preparePopFrame_{TYPE}→   Type-specific prepare (optional)
    ├─ Inject stylesheets    →   Copy page styles into pop-frame
    ├─ startDynamicLayout    →   Enable layout system
    └─ Transclude.trigger... →   Kick off content loading
        ↓
[content loads via transclude system]
        ↓
GW.contentDidInject handler  → Catches transclude completion
        ↓
rewritePopFrameContent_{TYPE}→ Type-specific DOM rewrites
```

### Provider Abstraction

The `popFrameProvider` abstraction allows identical code paths for popups and popins:

```javascript
Extracts.popFrameProvider.setPopFrameContent(popFrame, content)
Extracts.popFrameProvider.addClassesToPopFrame(popFrame, ...classes)
Extracts.popFrameProvider.isSpawned(popFrame)
Extracts.popFrameProvider.containingPopFrame(element)
```

Where behavior must differ, the system uses suffixed functions:

- `preparePopup_LOCAL_PAGE` vs `preparePopin_LOCAL_PAGE` (if defined)
- Falls back to `preparePopFrame_LOCAL_PAGE` for shared logic

---

## Key Patterns

### The preparePopup Callback

Popups.js calls `Extracts.preparePopup(popup)` just before spawning. This is the main integration point:

```javascript
preparePopup: (popup) => {
    let target = popup.spawningTarget;

    // Reuse existing popup for same target (deduplication)
    let existingPopup = Extracts.spawnedPopupMatchingTarget(target);
    if (existingPopup) return existingPopup;

    // Delegate to generic preparation
    return Extracts.preparePopFrame(popup);
}
```

The callback must return the popup (or an existing one), or `null` to cancel spawning. This enables patterns like "don't spawn if content is already visible on screen."

### Fill Functions and Include-Links

Fill functions don't generate content directly—they synthesize include-links that the transclude system will resolve:

```javascript
localPageForTarget: (target) => {
    let includeLink = synthesizeIncludeLink(target, {
        class: "include-strict include-block-context-expanded include-spinner-not"
    });
    if (fullPage)
        Extracts.addPopFrameClassesToLink(includeLink, "full-page");
    return newDocument(includeLink);
}
```

The include-link classes control transclude behavior (`include-strict`, `include-block-context-expanded`), and `data-popFrameClasses` transfers to the pop-frame after content loads.

### Desktop vs Mobile Decision

```javascript
let mobileMode = (
    localStorage.getItem("extracts-force-popins") == "true"
    || GW.isMobile()
    || matchMedia("(max-width: 1279px) and (max-height: 959px)").matches
);
Extracts.popFrameProviderName = mobileMode ? "Popins" : "Popups";
```

The decision considers: explicit user preference, device detection, and viewport dimensions.

### Target Matching for Deduplication

```javascript
targetsMatch: (targetA, targetB) => {
    return Extracts.targetIdentifier(targetA) == Extracts.targetIdentifier(targetB)
        && Extracts.targetTypeInfo(targetA).typeName == Extracts.targetTypeInfo(targetB).typeName;
}
```

Two targets "match" if they have the same identifier AND the same type. This prevents duplicate popups and allows recursive pop-frames to avoid self-referential loops.

---

## Configuration

### Local Storage Keys

| Key | Effect |
|-----|--------|
| `extract-popups-disabled` | Disables popups when "true" |
| `extract-popins-disabled` | Disables popins when "true" |
| `extracts-force-popins` | Forces popin mode even on desktop |

### Config Selectors (Extracts.config)

- `targetElementsSelector` — What elements can become extract targets
- `excludedElementsSelector` — Elements to skip
- `excludedContainerElementsSelector` — Containers whose children are skipped
- `contentContainersSelector` — Where to look for targets
- `hooklessLinksContainersSelector` — Where to skip indicator hook injection

---

## Integration Points

### Events Fired

| Event | When |
|-------|------|
| `Extracts.setupDidComplete` | After full initialization |
| `Extracts.cleanupDidComplete` | After teardown |

### Events Listened

| Event | Handler |
|-------|---------|
| `GW.contentDidInject` | Process targets in new content |
| `Popups.didLoad` / `Popins.didLoad` | Deferred setup if provider not ready |
| `DarkMode.didSetMode` | Update pop-frame stylesheet media attributes |
| `Rewrite.contentDidChange` | Reposition pop-frame after content changes |
| `Layout.layoutProcessorDidComplete` | Re-scroll to target after layout |

### Shared State

- `Extracts.rootDocument` — Always the page's `document`
- `Extracts.popFrameProvider` — Either `Popups` or `Popins` object
- `Extracts.popFrameProviderName` — "Popups" or "Popins" string

### Content System Integration

extracts.js relies heavily on the content/transclude system:

1. Fill functions call `synthesizeIncludeLink()` to create include-links
2. `Transclude.triggerTranscludesInContainer()` processes them
3. `GW.contentDidInject` fires when content is ready
4. Rewrite functions transform the injected content

### Annotation System Integration

extracts-annotations.js adds:
- `ANNOTATION` and `ANNOTATION_PARTIAL` type definitions
- `setUpAnnotationLoadEventsWithin()` — Pre-loads annotations on hover
- Pre-loading uses `Annotations.load()` before popup spawns

---

## titleForPopFrame Logic

Title generation follows a dispatch pattern:

```javascript
titleForPopFrame: (popFrame, titleHTML) => {
    let targetTypeName = Extracts.targetTypeInfo(target).typeName;
    let suffix = Extracts.popFrameTypeSuffix();  // "up" or "in"

    // Try popup/popin-specific, then generic, then default
    let specialTitleFunction = (
        Extracts[`titleForPop${suffix}_${targetTypeName}`]  // titleForPopup_ANNOTATION
        ?? Extracts[`titleForPopFrame_${targetTypeName}`]   // titleForPopFrame_ANNOTATION
    );

    if (specialTitleFunction)
        return specialTitleFunction(popFrame, titleHTML);
    else
        return Extracts.standardPopFrameTitleElementForTarget(target, titleHTML);
}
```

Type-specific examples:
- `titleForPopFrame_ANNOTATION` — Uses `Annotations.referenceDataForLink()` to get title
- `titleForPopFrame_LOCAL_PAGE` — Builds from page title + section/footnote info
- `titleForPopFrame_FOOTNOTE` — Shows "Footnote #N"

---

## See Also

- [popups.js](/frontend/popups-js) - Desktop popup display system with windowing capabilities
- [popins.js](/frontend/popins-js) - Mobile popin display system for touch devices
- [extracts-annotations.js](/frontend/extracts-annotations-js) - Annotation content type definitions
- [extracts-content.js](/frontend/extracts-content-js) - Content type definitions (local pages, media, etc.)
- [extracts-options.js](/frontend/extracts-options-js) - User preference UI for enabling/disabling popups
- [extracts-load.js](/frontend/extracts-load-js) - Bootstrap module that initializes the extract system
- [content.js](/frontend/content-js) - Content loading and caching system
- [annotations.js](/frontend/annotations-js) - Annotation data access layer
