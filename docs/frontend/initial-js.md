
# initial.js

**Path:** `js/initial.js` | **Language:** JavaScript | **Lines:** ~1,351

> Site-wide initialization and custom pub/sub event system (notification center)

---

## Overview

`initial.js` is the foundation script for gwern.net's JavaScript architecture. It runs synchronously in the document `<head>`, establishing the `GW` namespace and the notification center—a custom publish/subscribe event bus that all other modules use to communicate.

The notification center replaces the need for a framework like React by providing a flexible event-driven architecture. Modules register handlers for named events (like `GW.contentDidLoad`), and when content is loaded or injected anywhere on the site, the appropriate events fire and all registered handlers execute in a controlled, phased order. This pattern allows loose coupling between features like annotations, transcludes, typography fixes, and popups.

The file also establishes media query helpers, debugging infrastructure, scroll state tracking, and "do-when" utilities that defer code execution until specific DOM or page states are reached.

---

## Public API

### Notification Center

#### `GW.notificationCenter.addHandlerForEvent(eventName, f, options) → void`

Registers a handler function to be called when the named event fires.

```javascript
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", (eventInfo) => {
    // Process eventInfo.container
}, {
    phase: "rewrite",
    condition: (info) => info.contentType === "annotation",
    once: false,
    name: "myHandlerName"
});
```

**Options:**
- `condition` — Function returning boolean; handler only runs if true
- `once` — If true, handler auto-removes after first (successful) invocation
- `phase` — Controls execution order (see Handler Phases below)
- `name` — String identifier for debugging/tracking

**Called by:** Most modules via `addContentLoadHandler()` / `addContentInjectHandler()`

---

#### `GW.notificationCenter.fireEvent(eventName, eventInfo) → void`

Fires a named event, calling all registered handlers in phase order.

```javascript
GW.notificationCenter.fireEvent("GW.contentDidLoad", {
    source: "Annotation.load",
    container: annotationElement,
    document: document,
    contentType: "annotation",
    loadLocation: new URL(href)
});
```

**Called by:** `DOMContentLoaded` listener, transclude.js, annotations system, pop-frames

---

#### `GW.notificationCenter.removeHandlerForEvent(eventName, f) → void`

Unregisters a specific handler function from an event.

---

#### `GW.notificationCenter.removeAllHandlersForEvent(eventName) → void`

Removes all handlers registered for a given event.

---

### Content Handler Convenience Functions

#### `addContentLoadHandler(name, handler, phase, condition, once) → void`

Convenience wrapper for registering `GW.contentDidLoad` handlers. Also stores handler in `GW.contentLoadHandlers[name]`.

```javascript
addContentLoadHandler("wrapTables", (eventInfo) => {
    // Rewrite tables in eventInfo.container
}, "rewrite");
```

---

#### `addContentInjectHandler(name, handler, phase, condition, once) → void`

Convenience wrapper for registering `GW.contentDidInject` handlers. Also stores handler in `GW.contentInjectHandlers[name]`.

---

### Event Listeners (Animation-Frame Throttled)

#### `addNamedEventListener(target, eventName, fn, options) → wrapper`

Adds a high-performance event listener that runs at most once per animation frame.

```javascript
addNamedEventListener(document, "scroll", (event) => {
    // Runs at most once per frame
}, {
    name: "myScrollHandler",
    defer: true,
    ifDeferCallWhenAdd: true
});
```

**Options:**
- `name` — String identifier for later removal
- `defer` — If true, delays adding until page is loaded
- `ifDeferCallWhenAdd` — If true (and defer=true), calls handler immediately when added

---

#### `removeNamedEventListener(eventName, name) → void`

Removes a named event listener.

---

#### `addScrollListener(fn, options) → wrapper`

High-performance scroll listener (targets `document` by default).

```javascript
addScrollListener((event) => {
    // Runs at most once per frame
}, { name: "myScrollHandler", defer: true });
```

---

#### `removeScrollListener(name) → void`

Removes a named scroll listener.

---

#### `addMousemoveListener(fn, options) → wrapper`

High-performance mousemove listener (targets `window` by default).

---

#### `removeMousemoveListener(name) → void`

Removes a named mousemove listener.

---

#### `addWindowResizeListener(fn, options) → wrapper`

High-performance resize listener (targets `window`).

---

#### `removeWindowResizeListener(name) → void`

Removes a named resize listener.

---

### Media Queries

#### `doWhenMatchMedia(mediaQuery, options) → void`

Registers callbacks for media query state changes.

```javascript
doWhenMatchMedia(GW.mediaQueries.mobileWidth, {
    name: "adjustForMobile",
    ifMatchesOrAlwaysDo: (mediaQuery) => { /* mobile layout */ },
    otherwiseDo: (mediaQuery) => { /* desktop layout */ },
    whenCanceledDo: (mediaQuery) => { /* cleanup */ },
    callWhenAdd: true
});
```

**Options:**
- `name` — Required string identifier
- `ifMatchesOrAlwaysDo` — Called when matches (or always if `otherwiseDo` is null)
- `otherwiseDo` — Called when query stops matching
- `whenCanceledDo` — Called when media query is canceled
- `callWhenAdd` — If true (default), calls immediately when added

---

#### `cancelDoWhenMatchMedia(name) → void`

Deactivates a named media query listener.

---

### Do-When Utilities

#### `doWhenPageLoaded(f) → void`

Runs `f` immediately if `window.load` has fired, otherwise defers until it fires.

---

#### `doWhenDOMContentLoaded(f) → void`

Runs `f` immediately if `DOMContentLoaded` has fired (checked via `GW.DOMContentLoaded` flag), otherwise defers.

---

#### `doWhenElementExists(f, selector) → void`

Uses `MutationObserver` to run `f(element)` as soon as an element matching `selector` exists.

---

#### `doWhenBodyExists(f) → void`

Uses MutationObserver to run `f` as soon as `<body>` exists, or immediately if already present.

---

#### `doWhenMainExists(f) → void`

Uses MutationObserver to run `f` as soon as `<main>` exists, or immediately if already present. Also sets `document.main` to the element.

---

### Scroll State

#### `GW.scrollState`

Object tracking scroll position and direction:

```javascript
GW.scrollState = {
    lastScrollTop: number,              // Previous scroll position
    newScrollTop: number,               // Current scroll position
    unbrokenDownScrollDistance: number, // Continuous down-scroll distance
    unbrokenUpScrollDistance: number    // Continuous up-scroll distance
};
```

---

#### `togglePageScrolling(enable?) → void`

Enable/disable/toggle page scrolling. Adds `scroll-enabled-not` class to `<html>`.

---

#### `isPageScrollingEnabled() → boolean`

Returns true if page scrolling is currently enabled.

---

### Debugging

#### `GWLog(string, source, level) → void`

Conditional debug logging. Output controlled by `GW.logLevel`.

```javascript
GWLog("Processing complete", "annotations", 1);
```

---

#### `GW.setLogLevel(level, permanently) → void`

Set log verbosity. If `permanently` is true, stores in localStorage.

---

#### `GWStopWatch(f, ...args) → result`

Times a function execution with console output.

---

#### `GWTimer(f, ...args) → milliseconds`

Returns execution time of a function in milliseconds.

---

#### `GWServerLogError(errorString, errorType) → void`

Reports an error by sending a request to a special 404 URL (requires `utility.js`).

---

### Helper Functions

#### `Array.prototype.π(strings) → Array`

String array product. Concatenates each element of the array with each element of the argument.

```javascript
[ "a", "b" ].π([ "x", "y" ])  // → [ "ax", "ay", "bx", "by" ]
```

---

#### `_π(...args) → Array`

Sequential string array product across multiple arguments.

```javascript
_π(["a", "b"], ["x", "y"], ["1", "2"])  // All combinations
```

---

## Internal Architecture

### Notification Center Data Structures

```
GW.notificationCenter = {
    eventHandlers: {
        "GW.contentDidLoad": [
            { f: Function, options: { phase, condition, once, name } },
            ...
        ],
        ...
    },
    handlerPhaseOrders: {
        "GW.contentDidLoad": ["transclude", "rewrite"],
        "GW.contentDidInject": ["rewrite", "eventListeners"]
    },
    prefireProcessors: {
        "GW.contentDidInject": Function  // Unpacks flags bitfield
    },
    currentEvents: [],        // Events currently firing (prevents mid-fire additions)
    waitingHandlers: {}       // Handlers queued during event firing
}
```

### Event Firing Control Flow

1. Event name added to `currentEvents` (blocks new handler registration)
2. Pre-fire processor runs if defined (e.g., unpacks `flags` bitfield)
3. Handlers called in phase order; `condition` checked before each
4. Handlers with `once: true` removed after successful call
5. Event removed from `currentEvents`
6. Waiting handlers (registered during firing) now added

---

## Key Patterns

### Handler Phases

Phases control handler execution order within an event. The phase string syntax:

| Phase String | Meaning |
|--------------|---------|
| `"<"` | Before all named phases |
| `"<foo"` | Before phase "foo" |
| `"foo"` | During phase "foo" |
| `">foo"` | After phase "foo" |
| `">"` | After all named phases |
| `""` (empty) | Default position (before `">"` phase) |

For `GW.contentDidLoad`, the phase order is `["transclude", "rewrite"]`, so execution flows:

```
< → <transclude → transclude → >transclude → <rewrite → rewrite → >rewrite → (default) → >
```

This ensures transclusion handlers run before rewrite handlers.

### Mid-Fire Handler Registration Safety

If `addHandlerForEvent` is called while that event is firing, the handler goes to `waitingHandlers` and is added after firing completes. This prevents concurrent modification issues.

### Animation-Frame-Throttled Event Listeners

For scroll/resize events, `addNamedEventListener` uses a clever pattern: add a listener with `{ once: true }`, then in the callback, wait for `requestAnimationFrame` before running the actual handler and re-adding the listener.

```javascript
let wrapper = (event) => {
    requestAnimationFrame(() => {
        if (wrapper.removed == true)
            return;
        fn(event);
        target.addEventListener(eventName, wrapper, { once: true, passive: true });
    });
};
```

This caps handler execution at the frame rate regardless of how often the browser fires the event.

### Content Event Info Dictionary

Both `GW.contentDidLoad` and `GW.contentDidInject` receive an info object with:

- `source` — Where the event originated (e.g., `"DOMContentLoaded"`, `"Annotation.load"`)
- `container` — The DOM element containing new content
- `document` — The Document or DocumentFragment
- `contentType` — `"localPage"`, `"annotation"`, etc.
- `loadLocation` — URL object for the content's source

`GW.contentDidInject` adds `flags` (bitfield) unpacked by the pre-fire processor into boolean properties:

```javascript
GW.contentDidInjectEventFlags = {
    clickable:         1 << 0,
    stripCollapses:    1 << 1,
    fullWidthPossible: 1 << 2,
    localize:          1 << 3
};
// After prefireProcessor, eventInfo.clickable, eventInfo.stripCollapses, etc. are booleans
```

---

## Configuration

### Media Queries

```javascript
GW.mediaQueries = {
    mobileWidth:           matchMedia("(max-width: 649px)"),
    systemDarkModeActive:  matchMedia("(prefers-color-scheme: dark)"),
    hoverAvailable:        matchMedia("only screen and (hover: hover) and (pointer: fine)"),
    portraitOrientation:   matchMedia("(orientation: portrait)"),
    printView:             matchMedia("print")
};
```

### Browser Detection

```javascript
GW.isMobile()     // Touch + narrow viewport OR no hover capability
GW.isFirefox()    // Firefox browser
GW.isTorBrowser() // Tor browser (no service workers)
GW.isX11()        // X11 display server
```

### Logging

| Setting | Location | Effect |
|---------|----------|--------|
| `gw-log-level` | localStorage | Controls `GWLog` verbosity (0 = silent, higher = more verbose) |
| `GW.logSourcePadLength` | Code constant | Column width for log source field (28) |

Set log level programmatically:
```javascript
GW.setLogLevel(2, true);  // Level 2, persist to localStorage
```

---

## Integration Points

### Events Fired

| Event | When | Phases |
|-------|------|--------|
| `GW.contentDidLoad` | New content loaded (network, template, initial page) | `transclude`, `rewrite` |
| `GW.contentDidInject` | Content injected into DOM (page, pop-frame, cache) | `rewrite`, `eventListeners` |

### Events Listened

- `DOMContentLoaded` — Fires both content events for initial page load
- `load` — Logged only
- `readystatechange` — Logged only
- `scroll` — Updates `GW.scrollState`

### Shared State

| Property | Description |
|----------|-------------|
| `GW.DOMContentLoaded` | Boolean, true after DOMContentLoaded |
| `GW.scrollState` | Tracks scroll position and direction distances |
| `GW.mediaQueries` | Pre-defined MediaQueryList objects |
| `GW.eventListeners` | Registry of named event listeners |
| `GW.contentLoadHandlers` | Dictionary of content load handlers by name |
| `GW.contentInjectHandlers` | Dictionary of content inject handlers by name |
| `document.main` | Reference to `<main>` element (set by `doWhenMainExists`) |

### Dependencies

- None (this is the first script loaded)
- **Requires `utility.js` for:** `GWServerLogError` (uses `doAjax`), array methods like `removeIf` and `remove`

---

## See Also

- [rewrite.js](/frontend/rewrite-js) - Primary consumer of the notification center; defines most content handlers
- [content.js](/frontend/content-js) - Content loading system that triggers contentDidLoad events
- [utility.js](/frontend/utility-js) - General-purpose utilities used by initial.js
- [popups.js](/frontend/popups-js) - Popup system that fires content events when pop-frames load
- [extracts.js](/frontend/extracts-js) - Extract/popup system that depends on the notification center
- [transclude.js](/frontend/transclude-js) - Uses transclude phase of GW.contentDidLoad for inline embedding
- [popins.js](/frontend/popins-js) - Mobile popup variant that uses the same event system
