
# utility.js

**Path:** `js/utility.js` | **Language:** JavaScript | **Lines:** ~1,679

> General-purpose utility functions used throughout the gwern.net frontend

---

## Overview

This file provides the foundational utility layer for gwern.net's frontend JavaScript. It contains no gwern-specific business logic—just general-purpose helpers that every other module depends on.

The utilities fall into several categories: prototype extensions to built-in objects (Array, String, URL, Element, Node), DOM manipulation helpers (creating, wrapping, unwrapping elements), geometry/visibility functions for intersection testing, and AJAX primitives. The design philosophy favors extending native prototypes over utility namespaces, so you'll see `array.first` and `"my-string".kebabCaseToCamelCase()` rather than `Utils.first(array)`.

Most functions are pure or have minimal side effects. The file loads early and has no dependencies on other gwern modules.

---

## Public API

### DOM Element Creation

#### `newElement(tagName, attributes, properties) → HTMLElement`

Creates and returns a new DOM element. This is the workhorse for element creation throughout the codebase.

```javascript
let button = newElement("BUTTON", { class: "action-btn", "data-id": "123" }, { disabled: true });
```

**Called by:** Nearly everything—popups.js, sidenotes.js, extracts.js, rewrite.js, transclude.js
**Calls:** `document.createElement`

---

#### `newDocument(content) → DocumentFragment`

Creates a DocumentFragment from various input types: null (empty), string (parsed HTML), Node, NodeList, or another DocumentFragment.

```javascript
let frag = newDocument("<p>Hello</p><p>World</p>");
```

**Called by:** transclude.js, extracts.js, rewrite.js, content.js
**Calls:** `newElement`

---

#### `elementFromHTML(html) → HTMLElement | null`

Parses an HTML string and returns the single root element, or null if the HTML doesn't define exactly one element.

**Called by:** Various modules needing to parse HTML snippets
**Calls:** `newDocument`

---

### DOM Wrapping/Unwrapping

#### `wrapElement(element, wrapperSpec, options) → HTMLElement`

Wraps an element in a new container. The `wrapperSpec` format is `"tagName.class1.class2"` (tag defaults to "div").

```javascript
wrapElement(img, "figure.image-container", { moveClasses: true });
```

**Options:**
- `useExistingWrapper`: Reuse parent if it matches
- `moveClasses`: Transfer classes from element to wrapper (boolean or array)

**Called by:** rewrite.js, sidenotes.js, transclude.js
**Calls:** `newElement`, `isOnlyChild`, `transferClasses`

---

#### `unwrap(wrapper, options) → Node[]`

Removes a wrapper element, leaving its children in place. Returns array of unwrapped nodes.

**Options:**
- `moveID`: Transfer wrapper's ID to single child
- `moveClasses`: Copy classes to children (boolean or array)
- `moveAttributes`: Copy specified attributes to children
- `preserveBlockSpacing`: Preserve `--bsm` CSS property

**Called by:** rewrite.js, transclude.js
**Calls:** `copyClasses`, `copyAttributes`

---

#### `wrapAll(selector, wrapperSpec, options)`

Wraps all elements matching a selector. The `wrapperSpec` can be a string or a function.

**Options:**
- `root`: Element to search within (default: document)

---

#### `unwrapAll(selector, options)`

Unwraps all elements matching a selector.

---

### URL Utilities

#### `URLFromString(urlString, baseURL?) → URL`

Creates a URL object from strings that `new URL()` can't handle: hash-only (`#foo`), absolute paths (`/page`), or relative paths (`../other`).

```javascript
URLFromString("#section");        // → full URL with hash
URLFromString("/about");          // → https://gwern.net/about
URLFromString("sibling.html");    // → relative to current page
```

**Called by:** Nearly all modules dealing with links
**Calls:** `new URL()`

---

#### `modifiedURL(url, mods) → URL`

Returns a new URL with specified properties changed.

```javascript
modifiedURL(link.href, { hash: "", search: "" });
```

---

#### `URL.prototype.getQueryVariable(key) → string | null`
#### `URL.prototype.setQueryVariable(key, value)`
#### `URL.prototype.deleteQueryVariable(key)`
#### `URL.prototype.pathSegments` (getter)

URL prototype extensions for query parameter manipulation and path parsing.

---

#### `getQueryVariable(variable) → string | null`

Gets a URL query parameter from `window.location`.

---

#### `selectorFromHash(hash) → string | null`

Converts a URL hash to a CSS selector, with proper escaping. Returns null for empty hashes or Chrome's text fragment syntax (`#:~:`).

```javascript
selectorFromHash("#fn:1");  // → "#fn\\:1"
```

---

### AJAX

#### `doAjax(options)`

XMLHttpRequest wrapper with sensible defaults and callback-based API.

```javascript
doAjax({
    location: "/api/data",
    method: "POST",
    params: { id: 123 },
    serialization: "JSON",
    onSuccess: (event) => { /* handle response */ },
    onFailure: (event) => { /* handle error */ }
});
```

**Options:**
- `location`: URL (default: document.location)
- `method`: "GET" or "POST"
- `params`: Object of parameters
- `serialization`: "URL" or "JSON" (for POST)
- `responseType`: XHR response type
- `headers`: Custom headers object
- `onLoadStart`, `onProgress`, `onSuccess`, `onFailure`: Callbacks
- `checkFor404Redirect`: Detect soft 404s (default: true)
- `checkFor404RedirectURL`: URL that indicates a 404

**Called by:** content.js (for fetching pages/annotations)
**Calls:** `XMLHttpRequest`, `urlEncodeQuery`, `URLFromString`

---

#### `urlEncodeQuery(params) → string`

Encodes an object as URL query string.

---

### Geometry & Visibility

#### `isOnScreen(element, margin?) → boolean`

Returns true if the element intersects the viewport.

---

#### `isWithinRect(element, rect, margin?) → boolean`

Returns true if the element intersects the given DOMRect.

---

#### `isWithinRectOf(firstElement, secondElement, margin?) → boolean`

Returns true if firstElement intersects secondElement's bounding rect (or viewport if secondElement is null).

---

#### `doRectsIntersect(rectA, rectB, margin?) → boolean`

Tests whether two DOMRects intersect, with optional margin for "close enough" detection.

---

#### `pointWithinRect(point, rect) → boolean`

Tests whether a point `{x, y}` falls within a DOMRect.

---

#### `rectUnion(rect, ...rects) → DOMRect`

Returns a DOMRect that is the union of all provided rects.

---

### Observers

#### `lazyLoadObserver(fn, target, options) → IntersectionObserver`

Sets up an IntersectionObserver that calls `fn` when `target` enters view, then disconnects. Includes short-circuit optimization for already-visible elements.

**Options:** Standard IntersectionObserver options plus `checkPositionImmediately`

**Called by:** Lazy-loading throughout the codebase

---

#### `resizeObserver(fn, target)`

Sets up a ResizeObserver. If `fn` returns `false`, the observer disconnects.

---

### Class & Style Manipulation

#### `copyClasses(source, target, classes?)`
#### `removeClasses(element, classes?)`
#### `transferClasses(source, target, classes?)`

Copy, remove, or transfer CSS classes between elements. If `classes` array is omitted, operates on all classes.

---

#### `saveStyles(element, options)`
#### `restoreStyles(element)`
#### `stripStyles(element, options)`

Save inline styles to a `.savedStyles` property, restore them later, or strip styles (optionally preserving some).

---

### Node/Element Inspection

#### `isOnlyChild(node) → boolean | undefined`

Returns true if node is the only non-empty child of its parent.

---

#### `isNodeEmpty(node, options) → boolean | undefined`

Returns true if node contains only whitespace. Many options for excluding certain elements (identified elements, specific selectors, media elements).

---

### Text & Whitespace

#### `paragraphizeTextNodesOfElement(element, options)`

Wraps loose text nodes and inline elements in `<p>` tags. Used for normalizing content structure.

---

#### `Element.prototype.trimWhitespace(options)`
#### `Element.prototype.trimWhitespaceFromStart(options)`
#### `Element.prototype.trimWhitespaceFromEnd(options)`

Remove empty nodes from element boundaries. Options control recursion and within-node trimming.

---

### Miscellaneous

#### `doIfAllowed(fn, passHolder, passName, options)`

Simple mutex: only runs `fn` if `passHolder[passName]` is true, then sets it false until next frame (or immediately if `releaseImmediately: true`).

---

#### `onEventAfterDelayDo(target, event, delay, fn, options) → Function`

Sets up delayed event handling with optional cancellation events. Returns a cleanup function.

---

#### `relocate(s)`

Updates browser URL via `history.replaceState` without navigation.

---

#### `getHashTargetedElement() → Element | null`

Returns the element targeted by the current URL hash.

---

#### `selectElementContents(element)`

Programmatically selects an element's contents.

---

#### `getSelectionAsDocument(doc?) → DocumentFragment`

Returns current selection as a DocumentFragment.

---

---

## Prototype Extensions

The file extends several built-in prototypes:

### Array
- `.first` / `.last` (getters): First/last element or null
- `.remove(item)`: Remove first occurrence
- `.removeIf(test)`: Remove first item passing test
- `.insertBefore(item, test)`: Insert before first item passing test
- `.findLastIndex(test)`: Polyfill for older browsers
- `.unique()`: Return array with duplicates removed

### String
- `.capitalizeWords()`: Capitalize first letter of each word
- `.trimQuotes()`: Remove surrounding quotes
- `.startsWithAnyOf(prefixes)` / `.endsWithAnyOf(suffixes)` / `.includesAnyOf(substrings)`
- `.hashCode()`: Numeric hash (for caching/deduplication)
- `.kebabCaseToCamelCase()` / `.camelCaseToKebabCase()`

### URL
- `.getQueryVariable(key)` / `.setQueryVariable(key, value)` / `.deleteQueryVariable(key)`
- `.pathSegments` (getter): Array of path segments

### HTMLAnchorElement
- Same query variable methods as URL

### Element
- `.addActivateEvent(fn, options)` / `.removeActivateEvent()`: Handle click + keyboard activation
- `.swapClasses(classes, whichToAdd)`: Toggle between two classes
- `.trimQuotes()`: Remove quote characters from element text

### Node
- `.textNodes` (getter): All text nodes within
- `.firstTextNode` / `.lastTextNode` (getters)

### DOMTokenList
- `.containsAnyOf(tokens)` / `.containsAllOf(tokens)`

### Document / DocumentFragment
- `.innerHTML` (getter): Serialize contents to HTML string

### Selection
- `.selectNode(node)`: Select a specific node

### Set
- `.intersection(other)` / `.union(other)`: Set operations (polyfills)

---

## Polyfills

The file includes polyfills for:
- `Array.prototype.findLastIndex` (Firefox ≤103, Chrome ≤96)
- `crypto.randomUUID` (Safari \<15.4)
- `window.requestIdleCallback` (Safari)
- `Set.prototype.intersection` / `Set.prototype.union`

---

## Key Patterns

### Consistent Options Pattern

Most functions accept an `options` object with `Object.assign` for defaults:

```javascript
function wrapElement(element, wrapperSpec = "", options) {
    options = Object.assign({
        useExistingWrapper: false,
        moveClasses: false
    }, options);
    // ...
}
```

This allows callers to pass only the options they care about.

### Prototype Extension Style

Rather than `GW.utils.first(arr)`, the codebase uses `arr.first`. This reads more naturally but means this file must load before anything else. The pattern is consistent throughout.

### DocumentFragment as Interchange Format

`newDocument()` is the standard way to create DOM content from strings, nodes, or other fragments. Many functions return DocumentFragments rather than elements, allowing flexible composition.

---

## Configuration

No configuration—these are pure utilities.

---

## Integration Points

### No Events

This module doesn't fire or listen to events. It's a pure utility layer.

### No Shared State

Functions are stateless (except for `.savedStyles` property set by `saveStyles()`).

### Load Order

Must load before all other gwern JS modules. It's typically the first or second script loaded (after possibly `initial.js`).

---

## See Also

- [initial.js](/frontend/initial-js) - GW namespace and notification center; loads alongside utility.js
- [rewrite.js](/frontend/rewrite-js) - Heavy user of DOM manipulation utilities (wrapElement, unwrap, newElement)
- [transclude.js](/frontend/transclude-js) - Uses newDocument, wrap/unwrap for content embedding
- [content.js](/frontend/content-js) - Uses doAjax for fetching content from server
- [popups.js](/frontend/popups-js) - Uses geometry functions and element creation utilities
- [extracts.js](/frontend/extracts-js) - Uses URL utilities and DOM manipulation helpers
