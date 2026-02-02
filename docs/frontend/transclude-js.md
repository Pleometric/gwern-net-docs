
# transclude.js

**Path:** `js/transclude.js` | **Language:** JavaScript | **Lines:** ~2,592

Dynamic content embedding system - fetches content from other pages and injects it inline, replacing include-links.

---

## Overview

Transclusion is a core Zettelkasten feature that lets you embed content from other pages inline rather than just linking to them. When you put an include-link like `<a class="include" href="/sidenote#comparisons"></a>` in a page, this module replaces that link with the actual content of the `#comparisons` section from `/sidenote`.

The module handles three major concerns:

1. **Content fetching** - Lazy or immediate loading of source content via the Content/Annotations data providers
2. **Content slicing** - Extracting specific portions using anchors, range syntax (`#start#end`), CSS selectors, or block context
3. **Content injection** - Replacing the include-link with processed content, handling footnotes, TOC updates, and structure rectification

A key design decision is the lazy loading default: include-links only trigger when scrolled into view (with a margin). This prevents expensive transclusions from blocking initial page load. The `include-strict` class overrides this for content that must load immediately.

The template system adds another layer: instead of transcluding raw HTML, you can use templates that get filled with data extracted from the source content. This powers how annotations are displayed in popups.

---

## Public API

### Transclude.transclude(includeLink, now) → void

The main entry point. Processes an include-link, fetching and injecting its content.

```javascript
Transclude.transclude(includeLink, now = false);
```

- `now=false` (default): Uses lazy loading unless `include-strict` is set
- `now=true`: Loads immediately

**Called by:** handleTranscludes content handler, self (for delayed loads)
**Calls:** dataProvider.load, sliceContentFromDocument, includeContent

### Transclude.isIncludeLink(link) → boolean

Tests whether a link is an include-link by checking for `include*` classes.

```javascript
Transclude.isIncludeLink(link)
// Checks for: include, include-annotation, include-content, include-strict, etc.
```

**Called by:** Everywhere that processes links

### Transclude.allIncludeLinksInContainer(container) → Array

Returns all include-links within a container element.

**Called by:** handleTranscludes, triggerTranscludesInContainer

### Transclude.isAnnotationTransclude(link) → boolean

Determines if the link should transclude an annotation (vs. raw content).

Decision logic:
- If `transcludeAnnotationsByDefault` is true AND link has full annotation: transclude annotation unless `include-content` class is set
- Otherwise: transclude annotation only if `include-annotation` class is set

**Called by:** dataProviderForLink, contentTypeIdentifierForIncludeLink

### Transclude.sliceContentFromDocument(sourceDocument, includeLink) → DocumentFragment

Extracts the appropriate portion of content based on the include-link's URL and options.

Handles:
- Full page (extracts `#markdownBody` or `body`)
- Single anchor (`#section-id`)
- Range syntax (`#start#end`)
- Block context (`include-block-context`)
- CSS selector filtering (`data-include-selector`, `data-include-selector-not`)

**Called by:** transclude (after data loads)
**Calls:** blockContext, targetElementInDocument

### Transclude.triggerTranscludesInContainer(container, eventInfo, options) → void

Triggers all include-links in a container. Used for batch processing, e.g., before printing.

**Called by:** beforeprint listener

### fillTemplate(template, data, context, options) → DocumentFragment

Fills a template string with data. Supports conditionals, variable substitution, comments, and escapes.

```javascript
let content = fillTemplate(
  "<div><{title}></div><[IF abstract]><p><{abstract}></p><[IFEND]>",
  { title: "Hello", abstract: "World" }
);
```

**Called by:** Transclude.transclude (via processData)

### templateDataFromHTML(html) → object

Extracts template data from HTML using `data-template-field` and `data-template-fields` attributes.

```html
<span data-template-field="foo">Bar</span>  <!-- { foo: "Bar" } -->
<span data-template-fields="foo:$title" title="Baz"></span>  <!-- { foo: "Baz" } -->
```

**Called by:** fillTemplate

---

## Internal Architecture

### Include-Link Classes

| Class | Effect |
|-------|--------|
| `include` | Basic include-link marker |
| `include-annotation` | Force transclude annotation |
| `include-content` | Force transclude content (not annotation) |
| `include-strict` | Load immediately, not lazily |
| `include-lazy` | Extra-lazy: only when visible in viewport |
| `include-even-when-collapsed` | Load even inside collapsed blocks |
| `include-unwrap` | Discard container element, keep contents |
| `include-block-context` | Include surrounding block, not just target |
| `include-rectify-not` | Skip HTML structure rectification |
| `include-localize-not` | Don't integrate footnotes/TOC |

### Content Slicing Flow

```
URL: /page#start#end
        │
        ▼
sliceContentFromDocument()
        │
        ├─► No hash → Extract #markdownBody or body
        │
        ├─► Double hash (#start#end) → Range extraction
        │       │
        │       ├─► ##end     → Start of page to #end
        │       ├─► #start#   → #start to end of page
        │       └─► #start#end → #start to #end (exclusive)
        │
        └─► Single hash (#target) → Element extraction
                │
                ├─► include-block-context? → blockContext()
                │                              │
                │                              └─► Find containing block
                │                                  (figure, li, p, section, etc.)
                │
                └─► include-unwrap? → Return children only

        ▼
Apply data-include-selector-not (remove matching elements)
        ▼
Apply data-include-selector (keep only matching elements)
        ▼
Return DocumentFragment
```

### Transclusion Lifecycle

```
1. handleTranscludes (content load handler)
       │
       ▼
2. Transclude.transclude(link)
       │
       ├─► Check: already loading/complete? → exit
       │
       ├─► Check: in collapsed block? → wait for expand
       │
       ├─► Set loading state (spinner)
       │
       ├─► Lazy? (not include-strict)
       │       │
       │       └─► lazyLoadObserver → wait for scroll
       │
       ▼
3. dataProvider.load(link)  [Content or Annotations]
       │
       ▼
4. dataProvider.waitForDataLoad()
       │
       ├─► Template specified? → Transclude.doWhenTemplateLoaded()
       │
       ▼
5. processData() → fillTemplate() if needed
       │
       ▼
6. sliceContentFromDocument()
       │
       ▼
7. includeContent() → inject into DOM
       │
       ├─► Wrap content
       ├─► Delete metadata sections (if transcluding into full page)
       ├─► Rectify heading levels
       ├─► Fire GW.contentDidInject
       ├─► Rectify HTML structure (shift wrapper up tree)
       ├─► Update footnotes
       ├─► Update TOC
       └─► Unwrap and fire Rewrite.contentDidChange
```

### Template System

Templates use a custom syntax:

```
<{fieldName}>              <!-- Variable substitution -->
<[IF condition]>...<[IFEND]>   <!-- Conditional -->
<[IF cond]>...<[ELSE]>...<[IFEND]>
<[IF1 nested]>...<[IF1END]>    <!-- Nested conditional (numbered) -->
<( comment )>              <!-- Comment (removed) -->
>\                         <!-- Line continuation -->
<
\x                         <!-- Character escape -->
```

Conditional expressions support:
- `&` (AND), `|` (OR), `!` (NOT)
- `[brackets]` for grouping
- `'quoted'` or `"quoted"` string literals
- Whitespace-separated comparison: `fieldA fieldB` (equality test)

---

## Key Patterns

### Lazy Loading with Intersection Observer

```javascript
if (includeLink.classList.contains("include-strict") == false) {
    includeLink.delayed = true;
    requestIdleCallback(() => {
        lazyLoadObserver(() => {
            Transclude.transclude(includeLink, true);
        }, includeLink, {
            root: scrollContainerOf(includeLink),
            rootMargin: (includeLink.classList.contains("include-lazy")
                         ? "0px"
                         : Transclude.defaultLoadViewportMargin)  // "110%"
        });
    });
}
```

The 110% margin means content starts loading before it's actually visible.

### Block Context Detection

Finding the appropriate containing block for a target element:

```javascript
// Specific blocks (always used if matched)
specificBlockElementSelectors: [
    ".footnote, .sidenote",
    ".aux-links-append",
    ".epigraph"
],

// General blocks (checked with length limit)
generalBlockElementSelectors: [
    "figure",
    "li",
    "p",
    "blockquote",
    "section, .markdownBody > *"
],

blockContextMaximumLength: 250  // Characters - prevents huge blocks
```

### Alias Classes

Compound behaviors are expressed as alias classes that expand:

```javascript
Transclude.addIncludeLinkAliasClass("include-annotation-partial", (includeLink) => {
    includeLink.classList.add("include-annotation");
    includeLink.dataset.includeSelectorNot = ".annotation-abstract, .file-includes, figure, ...";
    includeLink.dataset.templateFields = "annotationClassSuffix:$";
    includeLink.dataset.annotationClassSuffix = "-partial";
});
```

### HTML Structure Rectification

After injection, block content gets shifted up the tree until it's in an allowed parent:

```javascript
let allowedParentSelector = ["section", "blockquote", "div", ".include-wrapper"];
if (wrapper.querySelector("section") == null)
    allowedParentSelector.push("li", "figcaption");

while (wrapper.parentElement.matches(allowedParentSelector) == false) {
    // Shift wrapper up, splitting parent if needed
}
```

This prevents invalid HTML like `<p><div>...</div></p>`.

---

## Configuration

### Transclude Object Properties

```javascript
Transclude.transcludeAnnotationsByDefault: true  // Prefer annotations over content
Transclude.defaultLoadViewportMargin: "110%"     // Intersection observer margin
Transclude.blockContextMaximumLength: 250        // Max chars for block context
```

### Data Attributes on Include-Links

| Attribute | Purpose |
|-----------|---------|
| `data-include-template` | Template name or `$key` into data |
| `data-include-selector` | CSS selector for elements to include |
| `data-include-selector-not` | CSS selector for elements to exclude |
| `data-include-selector-options` | `first` to use querySelector vs querySelectorAll |
| `data-block-context-options` | `expanded` to skip `<p>` as block boundary |
| `data-template-field` | Single field name, innerHTML is value |
| `data-template-fields` | Comma-separated `fieldName:$attr` or `fieldName:.prop` |

---

## Integration Points

### Events Fired

- **GW.contentDidLoad** - After content is fetched and processed
  - `source: "transclude"`, `contentType`, `includeLink`, `loadLocation`
- **GW.contentDidInject** - After content is injected into DOM
  - `source: "transclude"`, `container` (wrapper), `flags`, `includeLink`
- **Rewrite.contentDidChange** - After delayed transclusion completes
  - `source: "transclude"`, `nodes`, `where`
- **Transclude.templateDidLoad** / **Transclude.templateLoadDidFail**

### Events Listened

- **Collapse.collapseStateDidChange** - For delayed transcludes in collapsed blocks
- **GW.contentDidLoad** / **GW.contentDidInject** - handleTranscludes registered as content handler

### Data Providers

- **Content** - For `include-content` or default content transcludes
- **Annotations** - For `include-annotation` transcludes

The provider is selected via `Transclude.dataProviderForLink(includeLink)`.

### Shared State

- `includeLink.eventInfo` - Contains `document`, `container`, `context`, `loadLocation`
- `includeLink.delayed` - Set true if loading was deferred
- `includeLink.footnote` - Reference to associated footnote (during footnote processing)
- `Transclude.templates` - Cache of loaded template strings

### Related Modules

- **Content** (`content.js`) - Provides content for non-annotation transcludes
- **Annotations** (`annotations.js`) - Provides annotation data
- **Popups** (`popups.js`) - Uses transclusion for popup content
- **Collapse** (`collapse.js`) - Coordinates with collapsed blocks

---

## See Also

- [content.js](/frontend/content-js) - Content loading and caching; one of two data providers for transclusion
- [rewrite.js](/frontend/rewrite-js) - Content handlers and event system triggered after transclusion
- [initial.js](/frontend/initial-js) - Notification center used for transclude events
- [annotations.js](/frontend/annotations-js) - Annotation data provider; alternative to Content for transclusion
- [popups.js](/frontend/popups-js) - Popup system that uses transclusion for popup content
- [LinkMetadata.hs](/backend/link-metadata-hs) - Backend annotation generation that produces transcludable content
