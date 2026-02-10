# gwtar.js

**Path:** `build/gwtar.js` | **Language:** JavaScript | **Lines:** ~705

Self-extracting archive loader that enables offline viewing of gwern.net pages with lazy-loaded assets.

---

## Overview

gwtar.js is the JavaScript engine powering the `.gwtar.html` format—a portable, self-contained HTML file that bundles a webpage with all its assets (images, audio, scripts, stylesheets) into a single downloadable file while still supporting efficient lazy loading.

The script handles two loading strategies:
1. **Range-based loading** (preferred): Uses HTTP Range requests to fetch only needed byte ranges from the embedded tarball
2. **Full-response loading** (fallback): Downloads the entire file when the server doesn't support Range requests

When a `.gwtar.html` file is opened, this script uses the precomputed `assets` manifest (plus `overhead`) to compute byte ranges; it does not parse tar headers at runtime. It progressively loads assets as they're needed—replacing network URLs with blob URLs created from the extracted data.

---

## Key Features

- **Self-extracting archives**: Single HTML file contains everything needed for offline viewing
- **Lazy asset loading**: Assets are extracted on-demand, not all at once
- **HTTP Range request support**: Efficient partial downloads when server supports it
- **Graceful degradation**: Falls back to full download if Range requests fail
- **Multipart response parsing**: Handles `multipart/byteranges` responses
- **Request observation**: Uses PerformanceObserver to detect and intercept asset requests

---

## How It Works

### Archive Structure

A `.gwtar.html` file contains:
1. HTML prefix with this script and `gwtar_noscript.html` content
2. A tarball containing:
   - The original HTML page (asset "0")
   - All referenced assets (images, scripts, stylesheets, etc.)

### Loading Flow

```
Page Load
    ↓
getMainPageHTML()
    ↓
┌─────────────────────────────────────────┐
│ Try HTTP Range request for HTML (asset 0)│
└─────────────────────────────────────────┘
    ↓                              ↓
[Range works]                [Range fails (200)]
    ↓                              ↓
loadAllScriptsAndThenDo()    getFullPageData()
    ↓                              ↓
renderMainPage()             Stream full response
    ↓                              ↓
spawnRequestObserver()       loadAllWaitingAssets()
    ↓                              ↓
[Intercept asset requests]   [Extract as data arrives]
    ↓                              ↓
getResources() → replaceResourceInDocument()
```

### Asset Extraction

Each asset has pre-computed byte ranges stored in an `assets` object:

```javascript
assets["image.png"] = {
    name: "image.png",
    size: 12345,
    byteRangeStart: 1024,
    byteRangeEnd: 13368,
    "content-type": "image/png"
}
```

When an asset is needed:
1. Byte range is fetched (or extracted from already-downloaded data)
2. Data is wrapped in a Blob with appropriate MIME type
3. A blob URL replaces the original URL in the DOM

---

## Public Functions

### getMainPageHTML()

Entry point. Attempts Range-based loading of the main HTML, falls back to full download.

### getResources(assetInfoRecords, callbacks)

Fetches multiple assets via HTTP Range requests, batching them efficiently.

```javascript
getResources([assets["image.png"], assets["style.css"]], {
    onSuccess: (event, assetInfo) => {
        replaceResourceInDocument(assetInfo);
    }
});
```

### replaceResourceInDocument(assetInfo)

Replaces all references to an asset URL with a blob URL containing the extracted data.

### renderMainPage()

Parses the extracted HTML, activates scripts, and replaces the document content.

### parseMultipartBody(body, boundary)

Parses `multipart/byteranges` HTTP responses into individual parts with headers and body data.

---

## Helper Functions

### Array Extensions

```javascript
array.first   // First element or null
array.last    // Last element or null
array.nonnull() // Filter out null/undefined values
```

### URL Utilities

```javascript
URLFromString(urlString, baseURL)  // Parse various URL formats
modifiedURL(url, mods)             // Create modified URL copy
```

### DOM Utilities

```javascript
newElement(tagName, attributes, properties)  // Create configured element
newDocument(content)                         // Create DocumentFragment
elementFromHTML(htmlString)                  // Parse single element from HTML
```

### Tarball Utilities

```javascript
roundUpToMultiple(number, divisor)  // Round up to chunk boundary
tarballRecordSize(fileByteSize)     // Calculate tar record size with header
```

---

## Error Handling

### handlePageRequestFailure()

Displays appropriate error messages when loading fails:
- **Local file**: Shows instructions for extracting the tarball manually
- **Server error**: Indicates server configuration problem

The noscript content from `gwtar_noscript.html` provides the error UI.

---

## Integration

### With gwtar_noscript.html

The noscript HTML provides:
- JavaScript-disabled warning
- Local file opening instructions (with shell command)
- Server configuration error message

### With Build System

The gwtar format is created by the build system, which:
1. Generates the asset manifest with sizes and byte ranges
2. Creates the tarball of all assets
3. Prepends the HTML loader (this script + noscript content)

---

## Configuration

The script expects global variables defined in the HTML prefix:

```javascript
var assets = { "0": { basename: "page", size: 1234 }, ... };
var overhead = 512;  // Bytes before tarball data
var totalArchiveSize = 123456;  // Total file size
```

---

## Browser Compatibility

- Uses modern APIs: `fetch`, `PerformanceObserver`, `Blob`, `URL.createObjectURL`
- Requires JavaScript (gracefully shows noscript content otherwise)
- Range requests require server support (falls back if unavailable)

---

## See Also

- [gwtar_noscript.html](/frontend/gwtar-noscript-html) - Error messages and noscript fallback
- [sync.sh](/backend/sync-sh) - Build system that may generate gwtar files
- [rewrite.js](/frontend/rewrite-js) - Main site JavaScript that gwtar pages include
- [initial.js](/frontend/initial-js) - Early initialization code
