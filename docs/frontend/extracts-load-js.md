
# extracts-load.js

**Path:** `js/extracts-load.js` | **Language:** JavaScript | **Lines:** ~47

> Bootstrap module that configures and initializes the extracts/popup system

---

## Overview

`extracts-load.js` is the final module in the extracts system's load sequence. Its sole purpose is to configure the extracts system and trigger initialization. This file must be loaded **after** all other `extracts*.js` files, as documented in its opening comment.

The module performs three critical operations:
1. Sets up `Extracts.config` with CSS selectors that define where and how popups/popins can appear
2. Fires the `Extracts.didLoad` event to notify other systems
3. Calls `Extracts.setup()` to begin the initialization process

This bootstrap pattern ensures that all extract type definitions (annotations, content, etc.) have been registered before the system attempts to process any links on the page.

---

## Configuration Object

The entire module centers on the `Extracts.config` object, which contains five key selector configurations:

### contentContainersSelector

```javascript
contentContainersSelector: ".markdownBody, #TOC, #sidebar"
```

Defines the containers within which the extracts system will search for eligible links. Only links inside these containers will be processed for popups/popins.

**Containers:**
- `.markdownBody` - The main article content
- `#TOC` - Table of contents
- `#sidebar` - Sidebar navigation

### excludedContainerElementsSelector

```javascript
excludedContainerElementsSelector: "h1, h2, h3, h4, h5, h6"
```

Links within these elements will be excluded from extracts processing, even if they're within valid content containers. This prevents popups from spawning on heading anchor links.

### targetElementsSelector

```javascript
targetElementsSelector: "a[href]"
```

The base selector for potential targets. All anchor elements with `href` attributes are candidates for popup/popin treatment.

### excludedElementsSelector

```javascript
excludedElementsSelector: [
    ".section-self-link",
    ".footnote-self-link",
    ".sidenote-self-link",
    "[aria-hidden='true']",
    "[href$='#top']",
    ".extract-not"
]
```

Links matching any of these selectors are explicitly excluded from processing:
- **Self-links** - Section/footnote/sidenote anchor links
- **Hidden elements** - Accessibility-hidden elements
- **"Back to top" links** - Links to `#top`
- **Explicit opt-out** - Elements with `.extract-not` class

### hooklessLinksContainersSelector

```javascript
hooklessLinksContainersSelector: [
    "body.page-index #markdownBody",
    "div#new-popular-notable",
    "#sidebar",
    ".TOC",
    "#floating-header",
    "#page-toolbar",
    ".link-widget"
]
```

Links in these containers won't receive visual indicator hooks (the small decorative elements that indicate a link has popup content). The popups still work; they just don't get the visual indicator styling.

**Hookless contexts:**
- Index pages - Too many links, indicators would be visual clutter
- New/popular/notable sections - UI density concerns
- Navigation areas - Sidebar, TOC, headers, toolbars
- Link widgets - Standalone link display components

---

## Initialization Sequence

The module executes three sequential operations:

### 1. Fire Load Event

```javascript
GW.notificationCenter.fireEvent("Extracts.didLoad");
```

Notifies the system that the extracts configuration is complete. Other modules can listen for this event if they need to perform setup tasks after extracts configuration is available.

### 2. Trigger Setup

```javascript
Extracts.setup();
```

Calls the main setup function defined in [extracts-js](extracts-js). This function:
- Determines whether to use popups (desktop) or popins (mobile)
- Waits for the appropriate provider (Popups/Popins) to load
- Processes all links in content containers
- Sets up event handlers for dynamic content injection

---

## Load Order Dependencies

This module **must** be loaded last in the extracts sequence. The build system enforces this order (from `build_unified_assets.php`):

```php
$js = [
    'popups.js',           // Popup windowing system
    'popins.js',           // Popin (mobile) system
    'annotations.js',      // Annotation data access
    'content.js',          // Content type system
    'transclude.js',       // Transclusion engine
    'extracts.js',         // Core extracts framework
    'extracts-annotations.js',  // Annotation extracts type
    'extracts-content.js',      // Content extracts types
    'extracts-options.js',      // UI options/mode switching
    'extracts-load.js',         // THIS FILE (bootstrap)
    // ... other modules
];
```

**Why this order matters:**

Each `extracts-*.js` file registers type definitions by pushing to `Extracts.targetTypeDefinitions`. For example, `extracts-annotations.js` registers the `ANNOTATION` type, and `extracts-content.js` registers types like `LOCAL_PAGE`, `REMOTE_ARTICLE`, `TWEET`, etc.

If `extracts-load.js` ran before these registrations, `Extracts.setup()` would find no type definitions and process no links.

---

## Integration Points

### Events Fired

- **`Extracts.didLoad`** - Signals configuration is complete
  - Listeners: None in the codebase currently
  - Timing: Immediately after config object is set

### Functions Called

- **`Extracts.setup()`** - Main initialization
  - Defined in: [extracts-js](extracts-js)
  - What it does: Device detection, provider selection, target processing

### Configuration Consumed By

The `Extracts.config` object is used by:

- **`Extracts.addTargetsWithin()`** - Uses all selectors to find and filter links
- **`Extracts.removeTargetsWithin()`** - Uses selectors during cleanup
- **`Extracts.processTargetsInContainer()`** - Uses `contentContainersSelector`

---

## Customization Points

### Adding Excluded Containers

To prevent popups in a new container type:

```javascript
// Before extracts-load.js loads:
Extracts.config.excludedContainerElementsSelector += ", .my-new-container";
```

### Adding Hookless Containers

To suppress indicator hooks in a new context:

```javascript
// In extracts-load.js or before:
hooklessLinksContainersSelector: [
    // ... existing selectors ...
    ".my-dense-link-list"
]
```

### Opting Out Individual Links

Add the `.extract-not` class to any link:

```html
<a href="/article" class="extract-not">No popup for this link</a>
```

---

## Design Rationale

### Why a Separate Bootstrap File?

The extracts system is modular: core functionality lives in `extracts.js`, while specific extract types (annotations, content types) are defined in separate files. This allows:

1. **Extensibility** - New extract types can be added without modifying the core
2. **Code organization** - Each type's logic is self-contained
3. **Conditional loading** - Future versions could load only needed types

The bootstrap file enforces a consistent initialization point regardless of how many type modules are loaded.

### Configuration Over Convention

All selectors are explicitly declared in `Extracts.config` rather than hardcoded throughout the codebase. This:

- Makes customization easier
- Documents system behavior in one place
- Allows runtime modification if needed
- Supports A/B testing or user preferences

### Event-Driven Architecture

The `Extracts.didLoad` event follows the notification center pattern used throughout gwern.net. This decouples modules and allows future extensions to hook into the initialization sequence without modifying existing code.

---

## Common Issues

### Popups Not Appearing

**Symptom:** Links have content/annotations but don't spawn popups

**Likely causes:**
1. Link is in an excluded container (headings, etc.)
2. Link has `.extract-not` class
3. Link is not in a content container (`.markdownBody`, `#TOC`, `#sidebar`)
4. Another module disabled extracts before setup

**Debug:** Check `Extracts.config` selectors and inspect the link's DOM position

### Indicator Hooks Missing

**Symptom:** Popups work but links lack the visual indicator

**Likely cause:** Link is in a hookless container

**Solution:** This is usually intentional for UI density, but you can remove the container from `hooklessLinksContainersSelector` if desired

---

## See Also

- [extracts.js](/frontend/extracts-js) - Main extracts system that this module initializes
- [extracts-annotations.js](/frontend/extracts-annotations-js) - Annotation extract type definitions
- [extracts-content.js](/frontend/extracts-content-js) - Content extract type definitions
- [extracts-options.js](/frontend/extracts-options-js) - User preference UI for popups/popins
- [popups.js](/frontend/popups-js) - Desktop popup windowing system
- [popins.js](/frontend/popins-js) - Mobile popin system
- [initial.js](/frontend/initial-js) - GW namespace providing setup infrastructure
