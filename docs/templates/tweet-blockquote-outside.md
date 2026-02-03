---
sidebar_position: 12
---

# tweet-blockquote-outside.tmpl

**Path:** `template/include/tweet-blockquote-outside.tmpl` | **Language:** HTML5/Pandoc | **Lines:** ~19

Template fragment for rendering tweet content with a blockquote wrapper.

## Overview

This template generates the HTML structure for displaying Twitter/X post content in popups and transclusions with a `<blockquote>` element wrapping the entire content. This variant is used when the tweet is being presented as a quotation or cited material, providing appropriate semantic markup and default citation styling.

The structure is nearly identical to `tweet-blockquote-not.tmpl`, but the outer wrapper is a `<blockquote>` instead of a `<div>`. This subtle difference affects both semantic meaning (for accessibility and SEO) and default browser styling. The template includes rich metadata for author attribution, timestamps, link targets, and archived URLs.

The "outside" designation indicates that the blockquote wraps the entire content container, including both the attribution line and the tweet body. This is part of the gwern.net annotation system's flexible template system, which can adapt presentation based on context.

## Template Variables

- `contentTypeClass` - CSS class for content type styling (e.g., `content-transform-tweet`)
- `authorLinkClass` - CSS class for the author link element
- `authorLinkHref` - URL to the author's Twitter/X profile
- `whichTab` - Text describing tab behavior (e.g., "a new", "the same")
- `tabOrWindow` - Text "tab" or "window"
- `linkTarget` - HTML target attribute value (e.g., `_blank`, `_self`)
- `authorLinkIconMetadata` - Data attributes for icon/avatar styling
- `authorPlusAvatar` - Author name with avatar image
- `tweetLinkClass` - CSS class for the tweet timestamp link
- `tweetLinkHref` - URL to the specific tweet
- `archivedTweetURLDataAttribute` - Data attribute containing archived tweet URL
- `tweetLinkIconMetadata` - Data attributes for link icon styling
- `tweetDate` - Formatted date/time of the tweet
- `tweetContent` - The main body content of the tweet

## Output Structure

The template produces this structure:

```html
<blockquote class="content-transform [type-class]">
    <p class="data-field tweet-links">
        <a class="[author-class]"
           title="Open <author-url> in [which] [tab/window]"
           href="[author-url]"
           target="[target]"
           [icon-metadata]
        >[author+avatar]</a> on <a
           class="[tweet-class]"
           title="Open <tweet-url> in [which] [tab/window]"
           href="[tweet-url]"
           [archived-url-data]
           [icon-metadata]
        >[date]</a>
    </p>
    <div class="data-field tweet-content">[tweet body]</div>
</blockquote>
```

The `<blockquote>` wrapper provides citation semantics. Inside, the structure mirrors the non-blockquote variant: attribution metadata in a `<p>` tag followed by tweet content in a `<div>`.

**Template caveat:** The tweet link `title` attribute uses a malformed placeholder (`<<{tweetLinkHref>>`), so the tweet URL is not interpolated as shown in the example.

---

## See Also

- [tweet-blockquote-not.tmpl](/templates/tweet-blockquote-not) - Variant without blockquote wrapper
- [annotation-blockquote-not.tmpl](/templates/annotation-blockquote-not) - Similar pattern for annotations
- [Annotation.hs](/backend/annotation-hs) - Scrapes tweet metadata and content from Twitter/X
- [LinkMetadata.hs](/backend/link-metadata-hs) - Manages annotation database and metadata
- [extracts.js](/frontend/extracts-js) - Displays popup/transclude content
- [popups.js](/frontend/popups-js) - Popup positioning and rendering
- [content.js](/frontend/content-js) - Content type routing and template selection
