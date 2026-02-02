
# links.css

**Path:** `css/links.css` | **Language:** CSS | **Lines:** ~1147

> Link icons and annotation indicators

---

## Overview

`links.css` provides visual indicators for links throughout gwern.net, serving two primary purposes: (1) marking links that have annotations (abstracts, excerpts, etc.) with subtle visual cues, and (2) adding domain/filetype-specific icons after links to provide context about link destinations.

The annotation indicators use dotted underlines and vertical "dog-ear" markers to help users identify which links have useful popups, reducing frustration from hovering over unannotated links. The link icon system displays small graphical or textual symbols (like "PDF", "W" for Wikipedia, social media logos) that convey the source, topic, or file format at a glance—particularly useful for mobile users who need to know if a link leads to a PDF or other special content type.

The CSS implements a sophisticated system of CSS custom properties that allow per-icon customization of size, positioning, opacity, font selection, and hover behavior. Icons are manually curated rather than auto-generated from favicons because most favicons are low-resolution, don't fit the monochrome theme, and most domains are linked only once or twice (making icon recognition pointless).

---

## Key Sections

The file is organized into several major sections:

### Pop-Frame Indicators (lines 1-86)

- **Indicator hook** (`.has-indicator-hook`, lines 17-39): Container styling for links with visual indicators, adds spacing and positioning context.
- **Vertical bar indicator** (`.indicator-hook::before`, lines 41-66): Creates a subtle vertical line with gradient effects to mark annotated links; uses `linear-gradient` backgrounds to create a composite visual effect with spacing.
- **Dotted underlines** (`.has-annotation`, lines 70-79): Applies a dotted underline pattern using `linear-gradient` to visually distinguish annotated links.
- **Colored link hover** (lines 83-85): Changes underline color on hover for links with `data-link-icon-color`.

### Link Icons (lines 88-1147)

- **Philosophy documentation** (lines 92-129): Extended comment explaining the design rationale behind manual icon curation vs. auto-generated favicons.
- **Common styles** (lines 131-223): Base classes for icon containers and shared behavior.
- **SVG icon styles** (lines 146-223): Styling for graphical icons using CSS background images from sprite sheets.
- **Text icon styles** (lines 227-346): Styling for text-based icons (Unicode characters, emoji, initials).
- **Per-domain customizations** (lines 349-1147): Individual style overrides for specific domains and file types.

---

## Icon System

The link icon system works through a combination of data attributes added by backend code and CSS selectors:

### Data Attributes

Backend processing (LinkIcon.hs) adds two primary attributes to links:

- **`data-link-icon`**: The icon identifier (e.g., `"wikipedia"`, `"pdf"`, `"W"`, `"🤖"`).
- **`data-link-icon-type`**: Space-separated type tokens that control rendering:
  - `svg` — Render as SVG background image from sprite
  - `text` — Render as text/character content
  - `quad` — Four-letter square icon
  - `tri` — Three-letter icon (typically org initials)
  - `sans` — Use sans-serif font
  - `mono` — Use monospaced font
  - `bold` — Use bold weight
  - `italic` — Use italic style
  - `overline` — Add overline decoration

### CSS Hook Structure

Icons are injected via `.link-icon-hook::after` pseudo-elements:

```css
/* For SVG icons */
a[data-link-icon-type*='svg'] .link-icon-hook::after {
    content: "";
    background-image: var(--link-icon-url);
    /* Positioning via CSS custom properties */
}

/* For text icons */
a[data-link-icon-type*='text'] .link-icon-hook::after {
    content: var(--link-icon);
    /* Typography via CSS custom properties */
}
```

The `.link-icon-hook` element is typically a `<span>` added by frontend JavaScript inside the link, allowing the `::after` pseudo-element to position the icon correctly.

### Rendering Flow

1. Backend (LinkIcon.hs) analyzes URL and determines appropriate icon
2. Adds `data-link-icon` and `data-link-icon-type` attributes to `<a>` element
3. Frontend JavaScript adds `.link-icon-hook` span inside link
4. CSS selects on attributes and applies corresponding styles
5. `::after` pseudo-element renders the actual icon

---

## CSS Custom Properties

The system uses a hierarchical set of CSS custom properties with defaults and per-icon overrides:

### Common Properties (All Icons)

- **`--link-icon-size`**: Icon size (default varies by type)
- **`--link-icon-offset-x`**: Horizontal spacing from text (default varies)
- **`--link-icon-offset-y`**: Vertical baseline offset (default varies)
- **`--link-icon-opacity`**: Icon opacity (default varies)

### SVG Icon Properties

- **`--link-icon-url`**: Path to SVG icon in sprite sheet (set per-icon)
- **`--link-icon-url-hover`**: Optional colored version for hover state
- **`--link-icon-color-hover`**: Color to apply on hover (for colored icons)
- **`--dark-mode-invert-filter`**: Invert filter for dark mode compatibility

Defaults for SVG icons:
```css
--link-icon-size-default: 0.55em;
--link-icon-offset-x-default: 0.20em;
--link-icon-offset-y-default: 0.15em;
--link-icon-opacity-default: 0.55;
```

### Text Icon Properties

- **`--link-icon`**: The text content (character, emoji, or string)
- **`--link-icon-font`**: Font family stack
- **`--link-icon-font-weight`**: Font weight (default 600)
- **`--link-icon-font-style`**: Font style (default normal)
- **`--link-icon-text-decoration`**: Text decoration (default none)

Defaults for text icons:
```css
--link-icon-size-default: 0.75em;
--link-icon-offset-x-default: 0.12em;
--link-icon-offset-y-default: 0.25em;
--link-icon-opacity-default: 0.83;
--link-icon-font-default: Noto Emoji, Quivira, var(--GW-serif-font-stack);
```

### Per-Icon Customization

Each icon can override any property:
```css
a[data-link-icon='wikipedia'] {
    --link-icon-size: 0.60em;
    --link-icon-offset-x: 0.12em;
    --link-icon-offset-y: -0.05em;
    --link-icon-opacity: 0.65;
}
```

---

## Key Selectors

### Annotation Indicators

- **`.markdownBody a.has-annotation`**: Links with full annotations, gets dotted underline
- **`.markdownBody a.has-annotation-partial`**: Links with partial annotations, gets dotted underline
- **`.markdownBody a.has-annotation.decorate-not`**: Suppresses decoration when explicitly disabled
- **`.markdownBody a.has-indicator-hook`**: Container for the vertical indicator bar
- **`.markdownBody a.has-indicator-hook .indicator-hook::before`**: The actual vertical indicator visual

### Link Icon Base Selectors

- **`.markdownBody a.has-icon`**: Base class for links with icons, adds right margin
- **`.markdownBody a.has-icon.icon-disable .link-icon-hook`**: Hides icon when disabled
- **`a[data-link-icon-type*='svg']`**: All SVG-based icons
- **`a[data-link-icon-type*='text']`**: All text-based icons
- **`a[data-link-icon-type*='quad']`**: Quad-letter square icons (4-character abbreviations)
- **`a[data-link-icon-type*='tri']`**: Three-letter icons (typically organization initials)

### Type Modifiers

- **`a[data-link-icon-type*='sans']`**: Icons using sans-serif font
- **`a[data-link-icon-type*='mono']`**: Icons using monospaced font
- **`a[data-link-icon-type*='bold']`**: Bold text icons
- **`a[data-link-icon-type*='italic']`**: Italic text icons
- **`a[data-link-icon-type*='overline']`**: Icons with overline decoration

### Special Context Selectors

- **`.editorial a[data-link-icon-type*='svg'] .link-icon-hook::after`**: Adjusts icon positioning in editorial/typewriter font contexts
- **`.markdownBody .colors-invert a[data-link-icon-type*='svg'] .link-icon-hook::after`**: Inverts SVG icons when in color-inverted contexts (certain admonition boxes)

### Per-Icon Selectors

Each icon has a specific selector like:
- **`a[data-link-icon='wikipedia']`**: Wikipedia links
- **`a[data-link-icon='pdf']`**: PDF file links
- **`a[data-link-icon='gwern']`**: Internal gwern.net links
- **`a[data-link-icon='¶']`**: Within-page anchor links
- **`a[data-link-icon='𝛘']`**: arXiv links

### Hover Behavior

- **`a[data-link-icon-type*='svg']:hover .link-icon-hook::after`**: Reduces opacity on hover for SVG icons
- **`a[data-link-icon-type*='svg'][data-link-icon-color]:hover .link-icon-hook::after`**: Switches to colored version for icons with hover colors
- **`a[data-link-icon-type*='text'][data-link-icon-color]:hover .link-icon-hook::after`**: Applies hover color to text icons
- **`a[data-link-icon-color]:hover`**: Changes link underline color to match icon

---

## Common Icon Examples

### Textual Icons

| Icon | Selector | Domain/Type | Notes |
|------|----------|-------------|-------|
| `𝛘` | `data-link-icon='𝛘'` | arXiv | Greek chi character |
| `¶` | `data-link-icon='¶'` | Anchor links | Pilcrow symbol |
| `W` | `data-link-icon='wikipedia'` | Wikipedia | SVG rendered |
| `n` | `data-link-icon='n'` | Nature journal | Lowercase italic |
| `MS` | `data-link-icon='MS'` | Microsoft Research | Two-letter abbrev |
| `GPP` | `data-link-icon='GPP'` | Game Programming Patterns | Three-letter tri |
| `VICE` | `data-link-icon='VICE'` | VICE Media | Quad-letter |
| `♡` | `data-link-icon='♡'` | Fandom/Wikia | Heart symbol |
| `✉` | `data-link-icon='✉'` | Email/mailinglist | Envelope |
| `🤖` | `data-link-icon='🤖'` | beepb00p.xyz | Robot emoji |

### Graphical (SVG) Icons

| Icon | Selector | Domain | Description |
|------|----------|--------|-------------|
| `wikipedia` | `data-link-icon='wikipedia'` | Wikipedia | Globe with puzzle piece |
| `pdf` | `data-link-icon='pdf'` | PDF files | Adobe Acrobat logo |
| `github` | `data-link-icon='github'` | GitHub | Octocat silhouette |
| `twitter` | `data-link-icon='twitter'` | Twitter/X | Bird icon |
| `reddit` | `data-link-icon='reddit'` | Reddit | Snoo alien |
| `youtube` | `data-link-icon='youtube'` | YouTube | Play button |
| `amazon` | `data-link-icon='amazon'` | Amazon | Smile arrow |
| `arxiv` | `data-link-icon='arxiv'` | arXiv | Cornell logo |
| `openai` | `data-link-icon='openai'` | OpenAI | OpenAI logo |
| `anthropic` | `data-link-icon='anthropic'` | Anthropic | Anthropic logo |

### Special Purpose Icons

| Icon | Selector | Purpose |
|------|----------|---------|
| `bibliography` | `data-link-icon='bibliography'` | Link-bibliography pages |
| `arrows-pointing-inwards-to-dot` | `data-link-icon='arrows-pointing-inwards-to-dot'` | Backlinks |
| `video` | `data-link-icon='video'` | Video files |
| `≈` | `data-link-icon='≈'` | Similar-links |

---

## Responsive Behavior

The CSS includes responsive adjustments for mobile devices:

### PDF Icons on Mobile

```css
/* Desktop: Very small to avoid clutter (PDFs are common) */
@media not all and (max-width: 649px) {
    a[data-link-icon='pdf'] {
        --link-icon-size: 0.45em;
        --link-icon-offset-x: 0.10em;
        --link-icon-opacity: 0.50;
    }
}

/* Mobile: Larger for legibility on physical screens */
@media all and (max-width: 649px) {
    a[data-link-icon='pdf'] {
        --link-icon-size-default: 0.6em;
        --link-icon-offset-y-default: 0.25em;
    }
}
```

This dual sizing acknowledges that PDF icons are extremely common (would create visual clutter on desktop) but mobile users critically need to know when a link leads to a PDF (due to poor mobile PDF support).

---

## Dark Mode Handling

SVG icons include dark mode support through CSS filters:

```css
a[data-link-icon-type*='svg'] .link-icon-hook::after {
    --dark-mode-invert-filter: invert(1);
    /* Applied by dark-mode.css when dark theme active */
}
```

Colored icons (those with hover states) disable inversion:
```css
a[data-link-icon-type*='svg'][data-link-icon-color]:hover .link-icon-hook::after {
    --dark-mode-invert-filter: none;
    background-image: var(--link-icon-url-hover, var(--link-icon-url));
}
```

Color-inverted contexts (certain admonition types) apply additional inversion:
```css
.markdownBody .colors-invert a[data-link-icon-type*='svg'] .link-icon-hook::after {
    filter: invert(1);
}
```

---

## Typography Context Adjustments

The CSS adjusts icon positioning based on surrounding typography:

### Editorial/Typewriter Font Context

```css
.editorial a[data-link-icon-type*='svg'] .link-icon-hook::after {
    background-position-y: 0.80ex;  /* vs. 1ex default */
}

.editorial a[data-link-icon-type*='svg'][data-link-icon-type*='quad'] .link-icon-hook::after {
    background-position-y: 0.50ex;  /* vs. 0.75ex default */
}
```

Typewriter fonts have different metrics, requiring different vertical alignment to maintain visual consistency.

---

## Integration Points

### Backend: LinkIcon.hs

The backend Haskell module analyzes link URLs and adds the data attributes:

```haskell
-- Pseudocode representation
linkIcon :: URL -> (String, String)  -- (icon-name, icon-type)
linkIcon url
  | "wikipedia.org" `isInfixOf` url = ("wikipedia", "svg")
  | ".pdf" `isSuffixOf` url = ("pdf", "svg")
  | "arxiv.org" `isInfixOf` url = ("𝛘", "text italic")
  | ...
```

The module maintains regex patterns and domain mappings that determine which icon to use for each link. It also manages the icon sprite sheet and generates the `--link-icon-url` custom property values.

See: [link-icon-hs](../backend/link-icon-hs) for backend implementation details.

### Frontend: rewrite.js / annotations.js

Frontend JavaScript likely:
1. Inserts the `.link-icon-hook` span element inside links
2. Manages the `.has-icon` class on link elements
3. Handles `.icon-disable` for contexts where icons should be suppressed
4. Adds annotation indicator classes (`.has-annotation`, `.has-indicator-hook`)

The annotation system determines which links have useful popups/popovers and marks them with appropriate classes.

See: [annotations-js](annotations-js) for annotation indicator logic.

### Icon Sprite Sheet

SVG icons reference a sprite sheet (likely `static/img/icons/icons.svg` or similar) through CSS custom properties:

```css
/* Set by backend per-link */
a[data-link-icon='wikipedia'] {
    --link-icon-url: url('/static/img/icons/icons.svg#wikipedia');
}
```

The sprite contains all graphical icons in SVG format, allowing efficient caching and styling.

---

## Design Philosophy

From the extensive comment at lines 92-125:

> "The philosophy of link icons is that the hyperlink text & surrounding context often leaves much out... Link icons provide some additional information in a compact iconic form which doesn't interrupt the text or require manual annotation."

### Manual Curation Rationale

The file explicitly avoids auto-generating icons from favicons because:

1. **Quality**: Most favicons are low-resolution and unrecognizable
2. **Consistency**: Favicons need manual tweaking to fit gwern.net's monochrome aesthetic and work in both light/dark modes
3. **Long tail**: Most domains are linked only 1-2 times, so readers wouldn't recognize or benefit from their icons
4. **Maintainability**: Manual curation isn't burdensome for a single-author site and ensures quality

### Visual Balance

The CSS carefully balances several concerns:
- **Desktop clutter**: Common icons (PDF, Wikipedia) are made very small to avoid visual noise
- **Mobile legibility**: Same icons are enlarged on mobile where they're critical information
- **Opacity**: Most icons use reduced opacity (0.5-0.85) to stay subtle
- **Hover feedback**: Icons become more prominent on hover to indicate interactivity

---

## Configuration

Icons are configured through per-icon selectors that override default CSS custom properties. To add a new icon:

1. **Backend**: Add URL pattern and icon assignment to LinkIcon.hs
2. **Assets**: Add SVG to sprite sheet (for graphical icons) or identify Unicode character (for text icons)
3. **CSS**: Add selector with appropriate property overrides:

```css
a[data-link-icon='new-domain'] {
    --link-icon-size: 0.70em;
    --link-icon-offset-x: 0.15em;
    --link-icon-offset-y: 0.20em;
    --link-icon-opacity: 0.65;
}
```

No JavaScript changes required—the system is purely declarative once the data attributes are in place.

---

## See Also

- [links.css (CSS documentation)](/css/links) - Detailed CSS documentation for link icons and indicators
- [LinkIcon.hs](/backend/link-icon-hs) - Backend module that assigns icons to URLs
- [Config.LinkIcon](/backend/config-link-icon-hs) - URL-to-icon mapping rules and color constants
- [build_icon_sprite_file.php](/php/build-icon-sprite-file) - Generates the SVG sprite sheet
- [popups.js](/frontend/popups-js) - Popup system that displays the annotations marked by indicators
- [rewrite.js](/frontend/rewrite-js) - DOM transformation that injects .link-icon-hook elements
- [dark-mode-adjustments.css](/css/dark-mode-adjustments) - Dark mode filters and image handling
