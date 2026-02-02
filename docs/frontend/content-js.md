
# content.js

**Path:** `js/content.js` | **Language:** JavaScript | **Lines:** ~2,020

Polymorphic content loading system for fetching and transforming diverse content types.

---

## Overview

content.js is the backbone of gwern.net's rich link preview and transclusion system. It provides a unified interface for loading content from wildly different sources—local pages, Wikipedia articles, tweets, YouTube videos, code files—each requiring different fetch strategies and transformations.

The module uses a type-dispatch pattern: when asked to load content for a link, it iterates through registered content types until one claims the link via its `matches()` function. That content type then handles everything: determining what URLs to fetch, parsing responses, and building the final document fragment. This polymorphism lets the popup and transclusion systems remain source-agnostic.

Content is cached by URL (or custom cache key) to avoid redundant fetches. A secondary reference data cache stores processed/transformed versions of content, keyed separately so the same fetched content can produce different reference data for different link contexts (e.g., full page vs. specific section).

---

## Public API

### Content.load(link, loadHandler?, loadFailHandler?, sourceURLsRemaining?)

Main entry point. Fetches content for a link and caches the result.

```javascript
Content.load(link,
    (link) => { /* success */ },
    (link) => { /* failure */ }
);
```

**Called by:** Extracts.js, Transclude.js, popup/popin systems
**Calls:** `doAjax()`, `contentTypeForLink()`, `sourceURLsForLink()`, `contentFromResponse()`

Fires `Content.contentDidLoad` or `Content.contentLoadDidFail` events on completion.

---

### Content.cachedContentForLink(link) → object|"LOADING_FAILED"|null

Returns cached content for a link, or null if not yet loaded. Special case: if the link points to the current page and isn't cached, triggers a load.

**Called by:** Extracts, Transclude, any system checking if content is ready

---

### Content.cachedDocumentForLink(link) → DocumentFragment|null

Convenience wrapper returning just the `.document` property from cached content.

---

### Content.referenceDataForLink(link) → object|null|"LOADING_FAILED"

Returns processed reference data for a link. Computes and caches it on first call using the content type's `referenceDataFromContent()`.

**Called by:** Annotation rendering, popup title generation

---

### Content.contentTypeForLink(link) → ContentType|null

Returns the content type object that handles this link, or null if none match.

```javascript
let type = Content.contentTypeForLink(link);
if (type === Content.contentTypes.localPage) { ... }
```

---

### Content.waitForDataLoad(link, loadHandler?, loadFailHandler?)

Registers callbacks for when content loading completes. If already loaded/failed, calls handler immediately.

---

### Content.sourceURLsForLink(link) → [URL]

Returns the URL(s) to fetch for a link. Delegates to the matching content type.

---

## Internal Architecture

### Cache Structure

```
Content.cachedContent = {
    "https://gwern.net/gpt-3": { document: DocumentFragment, title: "...", ... },
    "https://en.wikipedia.org/wiki/GPT-3": { document: DocumentFragment },
    ...
}

Content.cachedReferenceData = {
    "https://gwern.net/gpt-3": { content: DocumentFragment, pageTitle: "...", ... },
    "https://gwern.net/gpt-3:::section-id": { content: DocumentFragment, ... },
    ...
}
```

### Load Flow

```
Content.load(link)
    → contentTypeForLink(link)           // Find handler
    → sourceURLsForLink(link)            // Get URLs to try
    → doAjax(sourceURL)                  // Fetch
    → contentFromResponse(response)      // Parse & transform
    → cacheContentForLink(content)       // Store
    → fireEvent("Content.contentDidLoad") // Notify
```

### Content Type Resolution

Links can override type detection via `data-link-content-type` attribute (kebab-case, converted to camelCase). Otherwise, types are checked in definition order—first match wins.

---

## Key Patterns

### Polymorphic Content Types

Each content type is an object with a standard interface:

```javascript
{
    matches: (link) => boolean,           // REQUIRED: Does this type handle this link?
    isSliceable: boolean,                 // REQUIRED: Can content be excerpted by ID/selector?

    // Either these two:
    sourceURLsForLink: (link) => [URL],   // What to fetch
    contentFromResponse: (response, link, sourceURL) => { document: DocumentFragment },

    // Or this one (for content that doesn't need fetching):
    contentFromLink: (link) => { document: DocumentFragment },

    // Optional:
    contentCacheKeyForLink: (link) => string,
    referenceDataFromContent: (content, link) => object,
    referenceDataCacheKeyForLink: (link) => string,
    permittedContentTypes: ["text/html"],  // HTTP Content-Type whitelist
    additionalAPIRequestHeaders: { ... }
}
```

### Fallback URL Chain

`sourceURLsForLink()` returns an array. If the first URL fails, the loader tries subsequent URLs:

```javascript
// localCodeFile tries syntax-highlighted HTML first, falls back to raw
sourceURLsForLink: (link) => {
    let codeFileURL = URLFromString(link.href);
    let syntaxHighlightedURL = URLFromString(codeFileURL.href + ".html");
    return [ syntaxHighlightedURL, codeFileURL ];  // Try .html first
}
```

### Redirect Handling

Content types can return `{ loadURLs: [...] }` instead of `{ document: ... }` to trigger a redirect within the loading system (used by Wikipedia for redirects):

```javascript
if (redirectLink) {
    return {
        loadURLs: Content.contentTypes.wikipediaEntry.sourceURLsForLink(newLink)
    }
}
```

---

## Configuration

### File Extensions

Each media/document type defines its recognized extensions:

| Content Type | Extensions |
|--------------|-----------|
| `localImage` | bmp, gif, ico, jpeg, jpg, png, svg |
| `localVideo` | mp4, webm |
| `localAudio` | mp3 |
| `localDocument` | html, pdf, csv, doc, docx, ods, xls, xlsx |
| `localCodeFile` | bash, c, conf, css, diff, hs, html, js, json, jsonl, md, opml, patch, php, py, R, sh, xml, txt |

### Wikipedia Processing

`extraneousElementSelectors` (line 851): Elements removed from Wikipedia content
`preservedInlineStyleProperties` (line 875): CSS properties kept when stripping styles

### Tweet Nitter Hosts

`liveNitterHosts` (line 1378): List of Nitter instances for tweet embedding

---

## Content Types Reference

| Type | Matches | Sliceable | Source |
|------|---------|-----------|--------|
| `dropcapInfo` | `.link-dropcap` class | No | Generated from link data attributes |
| `foreignSite` | External `.link-live` (not handled elsewhere) | No | iframe embed |
| `wikipediaEntry` | `*.wikipedia.org/wiki/*` (most) | No | Wikipedia REST API |
| `githubIssue` | `github.com/*/*/issues/N` | No | GitHub API |
| `tweet` | `x.com/*/status/*` with archive URL | No | Nitter archive |
| `localCodeFile` | Local files with code extensions | No | Server (tries .html first) |
| `localFragment` | `/metadata/*.html`, `/static/template/*.html` | Yes | Server |
| `remoteImage` | `upload.wikimedia.org` images | Yes | Direct URL |
| `remoteVideo` | YouTube, Vimeo links | Yes | iframe embed |
| `localDocument` | Local pdf, html, spreadsheets | No | iframe embed |
| `localVideo` | Local mp4, webm | Yes | `<video>` element |
| `localAudio` | Local mp3 | Yes | `<audio>` element |
| `localImage` | Local images | Yes | `<img>` element |
| `localPage` | Local pages (no extension or `/index`) | Yes | Server HTML |

---

## Integration Points

### Events Fired

- `Content.contentDidLoad` — content successfully loaded and cached
- `Content.contentLoadDidFail` — all source URLs failed
- `GW.contentDidLoad` — fired after content document is created (for post-processing)

### Events Listened

None directly. Load is triggered by callers (Extracts, Transclude).

### Shared State

- `Content.cachedContent` — accessed by Extracts.js, Transclude.js
- `Content.cachedReferenceData` — accessed by annotation systems

### Dependencies

- `GW.notificationCenter` — event system
- `doAjax()` — HTTP requests
- `newDocument()` — DocumentFragment creation
- `URLFromString()` — URL parsing
- `Annotations` — for `figcaptionHTMLForMediaLink()`
- `AuxLinks` — for fragment type detection
- `Transclude` — for `blockContext()`, template fill context

---

## Adding a New Content Type

1. Add entry to `Content.contentTypes` object
2. Implement required interface:

```javascript
myNewType: {
    matches: (link) => {
        // Return true if this type handles the link
        return link.hostname === "example.com";
    },

    isSliceable: false,  // Can parts be selected via hash/selector?

    sourceURLsForLink: (link) => {
        // Return array of URLs to try fetching
        let apiURL = URLFromString("https://api.example.com/...");
        return [ apiURL ];
    },

    contentFromResponse: (response, link, sourceURL) => {
        // Parse response and return content object
        let doc = newDocument(response);
        // ... transform as needed ...
        return { document: doc };
    },

    // Optional: custom reference data
    referenceDataFromContent: (content, link) => {
        return {
            content: content.document,
            // ... additional fields for templates ...
        };
    }
}
```

3. Position matters: types are checked in order, first match wins
4. If your type overlaps with existing types, add exclusion checks or place it earlier

---

## See Also

- [initial.js](/frontend/initial-js) - Notification center that content.js uses for events
- [rewrite.js](/frontend/rewrite-js) - DOM transformation handlers triggered after content loads
- [transclude.js](/frontend/transclude-js) - Inline transclusion system that uses Content as data provider
- [extracts.js](/frontend/extracts-js) - Popup/extract system that consumes content via Content.load
- [annotations.js](/frontend/annotations-js) - Annotation data layer, alternative data provider to Content
- [popups.js](/frontend/popups-js) - Popup rendering system that displays loaded content
