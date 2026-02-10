---
sidebar_position: 11
---

# tweet-blockquote-not.tmpl

**Path:** `template/include/tweet-blockquote-not.tmpl` | **Language:** HTML5/Pandoc | **Lines:** ~19

Template fragment for rendering tweet content without a blockquote wrapper.

## Overview

This template generates the HTML structure for displaying Twitter/X post content in popups and transclusions without using a `<blockquote>` element. Instead, it uses a semantic `<div>` wrapper, which is appropriate when the tweet is being presented as informational content rather than as a direct quotation.

The template creates a structured layout with author attribution, timestamp, and links, followed by the tweet content. It includes rich metadata such as link targets, icon metadata for styling, and archived URL references. The "not" variant (no blockquote) is used when the context doesn't require citation semantics, such as when displaying tweets as informational annotations or previews.

This template is part of the annotation system pipeline: `Annotation.hs` scrapes tweet data, `LinkMetadata.hs` manages the annotation database, and `content.js` + `extracts.js` render the populated template in the browser.

## Template Variables

- `contentTypeClass` - CSS class for content type styling (e.g., `content-transform-tweet`)
- `authorLinkClass` - CSS class for the author link element
- `authorLinkHref` - URL to the author's Twitter/X profile (defined but not used by the current template)
- `titleLinkHref` - URL destination for the title link (often same as tweet URL)
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
<div class="content-transform [type-class]">
    <p class="data-field tweet-links">
        <a class="[author-class]"
           title="Open <title-url> in [which] [tab/window]"
           href="[title-url]"
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
</div>
```

The layout emphasizes attribution (author and timestamp links) followed by content. The `data-field` classes enable JavaScript targeting for dynamic behavior like link rewriting or content updates.

**Template caveats:** The author link currently uses `titleLinkHref` (not `authorLinkHref`). The tweet link title placeholder in the template is malformed (`<<{tweetLinkHref>>`), so it does not interpolate as documented.

---

## See Also

- [tweet-blockquote-outside.tmpl](/templates/tweet-blockquote-outside) - Variant with blockquote wrapper for citation contexts
- [annotation-blockquote-not.tmpl](/templates/annotation-blockquote-not) - Similar "no-blockquote" pattern for annotations
- [Annotation.hs](/backend/annotation-hs) - Scrapes tweet metadata and content from Twitter/X
- [LinkMetadata.hs](/backend/link-metadata-hs) - Manages annotation database and metadata
- [extracts.js](/frontend/extracts-js) - Displays popup/transclude content
- [popups.js](/frontend/popups-js) - Popup positioning and rendering
- [content.js](/frontend/content-js) - Content type routing and template selection
