# google-cse.html

**Path:** `google-cse.html` | **Language:** HTML | **Lines:** ~38

Google Custom Search Engine integration page for gwern.net.

## Overview

`google-cse.html` is a minimal HTML template that embeds Google's Custom Search Engine (CSE) into the gwern.net site. This provides a Google-powered search interface that indexes gwern.net content with Google's infrastructure, offering an alternative to the custom site search implementation.

The page uses Google's CSE JavaScript widget which handles all search functionality client-side, requiring minimal custom code. It provides a clean, styled interface that matches the site's design while leveraging Google's search capabilities.

## File Structure

**Location:** `/Users/m/Projects/gwern-analysis/gwern.net/google-cse.html`

**Type:** HTML fragment/template

**Lines:** 39 lines (minimal, single-purpose)

## HTML Structure

### Title Element
```html
<title>Gwern.net Search</title>
```

Sets the page title for browser tabs and bookmarks.

### Styling Block

The page includes embedded CSS to style the Google CSE widget:

```css
.search-results-placeholder {
    font-family: Arial, sans-serif;
    opacity: 0.25;
    text-align: center;
    display: flex;
    justify-content: center;
    align-items: center;
    height: calc(100% - 4em);
    font-size: 1.25em;
    user-select: none;
}
```

**Key style customizations:**

1. **Placeholder styling** - Shows a faded message before search results appear
2. **Results wrapper** - `div.gsc-results-wrapper-visible` gets top margin when results display
3. **Branding control** - Hides Google's "Find more on Google" branding footer
4. **Responsive design** - Mobile breakpoint at 649px adjusts font sizes and padding

**Mobile adjustments (< 649px):**
- Reduces placeholder font size from 1.25em to 1em
- Reduces search button padding from default to 6px 8px
- Adjusts CSE container padding to 1em 0.5em

### Google CSE Widget

```html
<script async src="https://cse.google.com/cse.js?cx=009114923999563836576:dv0a4ndtmly"></script>
<div class="gcse-search"></div>
```

**Components:**

1. **CSE Script** - Async-loaded from Google's CDN with custom search engine ID (`cx` parameter)
2. **Search Container** - Empty div with class `gcse-search` that the script populates
3. **Placeholder** - Static HTML message that appears until results load

**CSE ID:** `009114923999563836576:dv0a4ndtmly` - This is gwern.net's registered Google CSE instance

### Placeholder Element

```html
<div class="search-results-placeholder">
    <p>[Search results will appear here]</p>
</div>
```

Provides visual feedback before the Google widget initializes and shows results.

## Google CSE Classes

The page styles several Google-provided CSS classes:

| Class | Purpose | Customization |
|-------|---------|---------------|
| `.gcse-search` | Main search widget container | Populated by Google's script |
| `.gsc-results-wrapper-visible` | Results container when visible | Adds 1.5em top margin |
| `.gcsc-more-maybe-branding-root` | Branding footer container | Centers with 1em margin |
| `.gcsc-find-more-on-google-branding` | Google branding link | Hidden via display: none |
| `.gsc-search-button-v2` | Search submit button | Mobile-specific padding |
| `.gsc-control-cse` | Overall CSE control wrapper | Mobile-specific padding |

## Integration Context

This page is typically:

1. **Loaded in popup/modal** - Via the popup system when user requests search
2. **Transcluded** - May be embedded into other pages via transclusion system
3. **Standalone page** - Can be accessed directly at `/google-cse.html`

The minimal structure (no header, nav, footer) suggests it's designed primarily for embedding rather than standalone viewing.

## Design Decisions

**Why Google CSE?**
- Offloads search infrastructure to Google
- No need to maintain search index
- Provides instant, high-quality results
- Handles query expansion, typo correction, etc.

**Why hide branding?**
- Cleaner visual integration with site design
- Reduces visual clutter
- Still respects Google's terms (branding root remains in DOM)

**Why placeholder message?**
- Prevents layout shift when widget loads
- Provides immediate feedback to user
- Improves perceived performance

## Comparison with google-search.html

Unlike `google-search.html` which implements a custom search form that submits to Google's standard search page, `google-cse.html`:

- Uses Google's embedded widget (results in-page)
- Requires no JavaScript configuration
- Relies entirely on Google's CSE script
- Has simpler markup and styling
- Provides seamless in-page search experience

## Configuration

The CSE instance is configured via Google's CSE admin panel (not in this file):
- Which sites to index (`site:gwern.net`)
- Search result customization
- Branding options
- Analytics integration
- SafeSearch settings

## Browser Compatibility

The page uses:
- **Async script loading** - Supported by all modern browsers
- **Flexbox** - For centering placeholder (IE11+)
- **CSS calc()** - For responsive heights (IE9+)
- **Media queries** - For responsive design (IE9+)

## Performance Considerations

- **Async loading** - Script doesn't block page rendering
- **Minimal CSS** - Under 1KB of styles
- **No dependencies** - Self-contained except for Google's CSE script
- **CDN delivery** - Google's script served from fast CDN

---

## See Also

- [google-search.html](/templates/google-search) - Alternative search form using standard Google search
- [popups.js](/frontend/popups-js) - Popup system that displays search interface
- [transclude.js](/frontend/transclude-js) - May embed this page into other contexts
- [extracts.js](/frontend/extracts-js) - Coordinates popup/embed behavior
- [default.html](/templates/default) - Main page template that may link to search
- [include-navbar](/templates/include-navbar) - Navbar that may contain search links
