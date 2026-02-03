# gwtar_noscript.html

**Path:** `build/gwtar_noscript.html` | **Language:** HTML | **Lines:** ~13

Fallback content displayed when a `.gwtar.html` file cannot be loaded normally.

---

## Overview

gwtar_noscript.html provides the error messaging UI for the gwtar self-extracting archive format. It's embedded within `<noscript>` tags in every `.gwtar.html` file and displays contextual help when:

1. JavaScript is disabled in the browser
2. The file is opened locally (via `file://` protocol)
3. The server fails to serve the file correctly

The content is intentionally minimal—just text and basic styling—since it must work without any external assets or JavaScript.

---

## Content Structure

### Main Message

```html
<p>This HTML page <span class="js-disabled-warning">
<strong>requires JavaScript to be enabled</strong> to render, as it
</span>is a self-extracting <code>gwtar</code> HTML file.</p>
```

Explains the gwtar format with a link to documentation.

### Local File Warning

```html
<div class="local-file-warning" style="display: none;">
    <p>You are seeing this message... because <code>gwtar</code> files
    <strong>cannot be opened locally</strong>...</p>
    <p><code>perl -ne'print $_ if $x; $x=1 if /&lt;!-- GWTAR END/'
        &lt; <span class="gwtar-file-base-name">foo</span>.gwtar.html
        | tar --extract</code></p>
</div>
```

Provides a shell command to manually extract the tarball when opened as a local file.

### Server Failure Warning

```html
<div class="server-fail-warning" style="display: none;">
    <p>...the web server is incorrectly configured or does not support
    <code>gwtar</code> files...</p>
</div>
```

Indicates a server-side configuration problem.

---

## How It Works

### Display Logic

The content sits inside `<noscript>` tags, so browsers show it when JavaScript is disabled. When JavaScript IS enabled, `gwtar.js` handles display:

```javascript
// In gwtar.js handlePageRequestFailure():
let noscript = document.querySelector("noscript");
noscript.outerHTML = noscript.innerHTML;  // Unwrap noscript

// Hide all warnings first
document.querySelectorAll(warningsSelector).forEach(warning => {
    warning.style.display = "none";
});

// Show appropriate warning based on protocol
if (location.protocol == "file:") {
    document.querySelector(".local-file-warning").style.display = "";
} else {
    document.querySelector(".server-fail-warning").style.display = "";
}
```

### Dynamic Content

The script populates placeholder spans:
- `.gwtar-file-base-name` - Gets the actual gwtar filename
- `.html-file-base-name` - Gets the extracted HTML filename

---

## CSS Classes

| Class | Purpose |
|-------|---------|
| `.js-disabled-warning` | Text shown only when JS is disabled |
| `.local-file-warning` | Container for local file instructions |
| `.server-fail-warning` | Container for server error message |
| `.gwtar-file-base-name` | Placeholder for gwtar filename |
| `.html-file-base-name` | Placeholder for HTML filename |

---

## Manual Extraction Command

The provided shell command extracts the tarball portion:

```bash
perl -ne'print $_ if $x; $x=1 if /<!-- GWTAR END/' < foo.gwtar.html | tar --extract
```

**How it works:**
1. Perl reads the file line by line
2. Starts printing after the `<!-- GWTAR END` marker
3. Pipes the tarball data to `tar --extract`
4. Results in the original HTML and all assets

---

## Integration

### Embedded in gwtar Files

This HTML is concatenated into every `.gwtar.html` file during the build process, wrapped in `<noscript>` tags.

### With gwtar.js

The JavaScript loader (`gwtar.js`) manipulates this content's visibility based on the detected failure mode.

---

## See Also

- [gwtar.js](/frontend/gwtar-js) - JavaScript loader that controls this content
- [sync.sh](/backend/sync-sh) - Build process that assembles gwtar files
