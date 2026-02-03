# google-search.html

**Path:** `google-search.html` | **Language:** HTML | **Lines:** ~139

Custom Google Search form interface for gwern.net with site filtering options.

## Overview

`google-search.html` is a standalone HTML page that provides a customized search interface for gwern.net using Google's standard search engine. Unlike `google-cse.html` which embeds Google's Custom Search widget, this page implements a native HTML form that submits searches to Google with site-specific filters.

The page features a clean, minimalist design with support for both light and dark color schemes and provides options to search all of gwern.net, essays only, or documentation only. The search form opens results in a new tab, maintaining the user's current browsing context.

## File Structure

**Location:** `/Users/m/Projects/gwern-analysis/gwern.net/google-search.html`

**Type:** HTML page (standalone)

**Lines:** 140 lines

## HTML Structure

### Title and Metadata

```html
<title>Gwern.net Search</title>
```

Simple title without additional meta tags, suggesting this is designed for embedding or iframe use rather than direct navigation.

### Styling System

The page includes two style blocks:

1. **Base styles** (`#search-styles`)
2. **Dark mode styles** (`#search-styles-dark` with `prefers-color-scheme: dark` media query)

This approach provides automatic dark mode support based on user's system preferences.

### Main Layout

```html
<body>
    <main>
        <form class="searchform">...</form>
        <fieldset id="search-where-selector">...</fieldset>
    </main>
</body>
```

**Layout technique:**
- Body uses flexbox to center content vertically
- Main element contains the search UI
- Pointer events disabled on body, re-enabled on interactive elements
- Flexbox ensures content stays centered regardless of viewport size

## Search Form

```html
<form action="https://www.google.com/search"
      class="searchform"
      method="get"
      name="searchform"
      target="_blank">
    <input autocomplete="on"
           class="form-control search"
           placeholder="Gwern.net search"
           required="required"
           type="text">
    <input class="query" name="q" type="hidden">
    <button class="button" type="submit">Search</button>
</form>
```

### Form Behavior

**Action:** `https://www.google.com/search` - Submits to Google's search engine

**Method:** GET - Query parameters appear in URL

**Target:** `_blank` - Opens results in new tab/window

### Input Fields

1. **Visible text input:**
   - Class: `form-control search`
   - Autocomplete enabled
   - Required field (HTML5 validation)
   - Placeholder: "Gwern.net search"

2. **Hidden query input:**
   - Name: `q` (Google's query parameter)
   - Populated by JavaScript before form submission
   - Combines user query with site filter

### Search Button

Styled submit button with:
- Hover effects (background darkens, text lightens)
- Focus states with outline
- Fixed width (5em) vs flexible text input
- Pointer cursor on hover

## Site Filter Selector

```html
<fieldset id="search-where-selector">
    <legend>Search in:</legend>
    <label><input type="radio" value="site:gwern.net" checked /> All</label>
    <label><input type="radio" value="site:gwern.net -site:gwern.net/doc/" /> Essays only</label>
    <label><input type="radio" value="site:gwern.net/doc/" /> Docs only</label>
</fieldset>
```

### Filter Options

| Option | Google Site Filter | Purpose |
|--------|-------------------|---------|
| **All** | `site:gwern.net` | Searches entire gwern.net domain |
| **Essays only** | `site:gwern.net -site:gwern.net/doc/` | Excludes /doc/ directory |
| **Docs only** | `site:gwern.net/doc/` | Only searches /doc/ directory |

**Default:** "All" is checked by default

### Site Filter Logic

The radio buttons use Google's `site:` operator syntax:
- Positive filter: `site:gwern.net/doc/` includes only that path
- Negative filter: `-site:gwern.net/doc/` excludes that path
- These values would be combined with the user's query via JavaScript

## Color Scheme Design

### Light Mode (Default)

```css
body {
    background-color: #fff;
    color: (inherited);
}
.searchform input.search {
    background-color: #fff;
    color: #000;
    outline: 1px solid #ccc;
}
.searchform button {
    background-color: #e4e4e4;
    outline: 1px solid #ccc;
}
```

**Light mode palette:**
- Background: Pure white (#fff)
- Text: Black (#000)
- Borders: Light gray (#ccc)
- Buttons: Very light gray (#e4e4e4)
- Hover: Medium gray (#777)

### Dark Mode

```css
@media all and (prefers-color-scheme: dark) {
    body {
        background-color: #161616;
    }
    .searchform input.search {
        background-color: #161616;
        color: #f1f1f1;
        outline: 1px solid #5c5c5c;
    }
    .searchform button {
        background-color: #404040;
        color: #f1f1f1;
    }
}
```

**Dark mode palette:**
- Background: Very dark gray (#161616)
- Text: Off-white (#f1f1f1)
- Borders: Medium-dark gray (#5c5c5c)
- Buttons: Dark gray (#404040)
- Hover: Medium-light gray (#a6a6a6)

## Responsive Design

No explicit mobile breakpoint or `@media` rules are defined; responsiveness comes from the base flexbox layout and em-based sizing.

### Font Sizing

```css
html {
    font-size: 18px;  /* Base size */
}
```

Uses rem/em units throughout for scalable, accessible sizing.

## Styling Patterns

### Focus States

Multi-level focus indication:

```css
.searchform input.search:focus {
    outline: 2px solid #777;  /* Direct focus */
}
.searchform:focus-within input.search {
    outline: 2px solid #ccc;  /* Container focus */
}
```

Uses `:focus-within` pseudo-class to style parent containers when children are focused, providing clear visual hierarchy.

### Appearance Reset

```css
.searchform input.search {
    appearance: none;
    border: none;
}
.searchform button {
    appearance: none;
    border: none;
}
```

Removes default browser styling to ensure consistent cross-browser appearance.

### Pointer Events

```css
body {
    pointer-events: none;
}
#search-where-selector label,
.searchform {
    pointer-events: auto;
}
```

**Purpose:** Disables interaction with background/body while enabling only the search UI. This is likely because the page is embedded in a modal/popup where the underlying page should be non-interactive.

## JavaScript Integration (Implied)

While no JavaScript is present in the file, the form likely requires JavaScript to:

1. Combine the user's query with the selected site filter
2. Populate the hidden `q` input before submission
3. Handle form submission event

**Expected behavior:**
```javascript
form.addEventListener('submit', (e) => {
    const userQuery = document.querySelector('.search').value;
    const siteFilter = document.querySelector('[name="search-where"]:checked').value;
    const combinedQuery = `${userQuery} ${siteFilter}`;
    document.querySelector('.query').value = combinedQuery;
});
```

## Design Philosophy

### Minimalism

- No header, navigation, or footer
- Pure white/black backgrounds
- Centered single-purpose interface
- Minimal decoration

### Accessibility

- Semantic HTML (form, fieldset, legend, label)
- Focus states clearly defined
- Required field validation
- Autocomplete enabled for user convenience
- Sufficient color contrast in both modes

### User Experience

- Target blank keeps context
- Radio buttons for clear mutual exclusion
- Placeholder text provides context
- Button hover states provide feedback
- Dark mode respects system preferences

## Use Cases

### Primary Use: Embedded Search

The minimal design and pointer-events manipulation suggest this page is designed to be:

1. **Loaded in iframe** - As part of a search modal/popup
2. **Embedded via popup system** - Using gwern.net's popup.js
3. **Transcluded** - Via the transclusion system

### Secondary Use: Direct Access

Can also be accessed directly at `/google-search.html` for standalone searching.

## Comparison with google-cse.html

| Feature | google-search.html | google-cse.html |
|---------|-------------------|-----------------|
| **Backend** | Standard Google Search | Google Custom Search Engine |
| **Results** | Opens in new tab | Embedded in page |
| **Customization** | Site filter radio buttons | None (configured in CSE panel) |
| **JavaScript** | Required (implied) | Optional (widget handles all) |
| **Styling** | Full custom design | Minimal widget styling |
| **Dark mode** | Built-in CSS | Widget-dependent |
| **Complexity** | Higher (form handling) | Lower (widget does everything) |

## Browser Compatibility

**Modern features used:**
- `:focus-within` - CSS3 pseudo-class (Chrome 60+, Firefox 52+, Safari 10.1+)
- `prefers-color-scheme` - Media query (Chrome 76+, Firefox 67+, Safari 12.1+)
- Flexbox - Layout (IE11+)
- `appearance: none` - Styling (with prefixes: IE/Edge, Chrome, Firefox, Safari)

**Graceful degradation:**
- Focus-within fallback: Direct :focus still works
- Dark mode fallback: Light mode styles apply
- Flexbox fallback: Still functional without perfect centering

## Performance

- **Zero JavaScript** - HTML/CSS only (JS handled by parent page)
- **Inline styles** - No external CSS requests
- **No images** - Pure text/color interface
- **Minimal markup** - Under 2KB total

## Integration Points

### Expected Parent Page JavaScript

Parent page likely provides:
1. Form submission handler
2. Query combination logic
3. Popup/modal management
4. Analytics tracking

### Popup System Integration

Coordinates with:
- **popups.js** - Opens/closes search interface
- **extracts.js** - Manages embedded content lifecycle
- **transclude.js** - May transclude search box into other contexts

---

## See Also

- [google-cse.html](/templates/google-cse) - Google Custom Search Engine alternative implementation
- [popups.js](/frontend/popups-js) - Popup system that displays search interfaces
- [extracts.js](/frontend/extracts-js) - Popup/embed content coordinator
- [content.js](/frontend/content-js) - Content type definitions and handlers
- [default.html](/templates/default) - Main page template that may link to search
- [include-sidebar](/templates/include-sidebar) - Sidebar that may contain search links
