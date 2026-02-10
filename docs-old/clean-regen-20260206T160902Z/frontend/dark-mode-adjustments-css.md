
# dark-mode-adjustments.css

**Path:** `css/dark-mode-adjustments.css` | **Language:** CSS | **Lines:** 163

> Dark mode filter adjustments for images and icons

---

## Overview

`dark-mode-adjustments.css` handles the visual adaptation of images, icons, and UI elements for dark mode. It primarily applies CSS filters for visual content that can't be easily recolored through variables alone, and also overrides a small set of color variables (e.g., body background/text) for dark mode tuning.

The file implements a sophisticated three-tier image handling strategy: invertible images (diagrams, charts) get full inversion with hue rotation; non-invertible images (artwork, photos) get grayscale only; and manually-marked images can opt out entirely. All filters are removed on hover to let users see the original, except for elements marked `.drop-filter-on-hover-not`.

A key design principle evident throughout is **performance over purity**—the background color is slightly off-black (`#161616` instead of `#000000`) to prevent pixel toggling jank on OLED displays, and filter transitions are carefully controlled to avoid layout thrashing.

---

## Key Selectors

### `.dark-mode-invert`
Applied to elements that should be inverted in dark mode. Uses a CSS variable `--dark-mode-invert-filter` that is set via CSS (by component styles or defaults):

```css
.dark-mode-invert,
.dark-mode-invert::before,
.dark-mode-invert::after {
    filter: var(--dark-mode-invert-filter, none);
}
```

The variable-based approach allows different components to define appropriate filters without JavaScript intervention.

### Image Class Hierarchy
Images follow a strict precedence order:

1. **`.invert-not` / `.invert-not-auto`** - No filtering, preserve original appearance
2. **`.invert` / `.invert-auto`** - Full inversion treatment (grayscale + invert + hue-rotate)
3. **No class** - Grayscale only (default for photos/artwork)

The `-auto` suffix variants are applied client-side by the `invertOrNot` logic, while unsuffixed classes are manually authored.

### Admonition Icons
Each admonition type has specific filter rules:

```css
div.admonition.tip::before      { filter: invert(1); }
div.admonition.note::before     { filter: none; }
div.admonition.warning::before  { filter: none; }
div.admonition.error::before    { filter: none; }
```

Only `.tip` admonitions invert their icon, suggesting the tip icon is light-colored in the base theme.

### Tablesorter Icons
Dark mode uses alternate icon files instead of filters:

```css
table th.tablesorter-header     { background-image: url('/.../tablesorter-bg-dark.gif'); }
table th.tablesorter-headerAsc  { background-image: url('/.../tablesorter-asc-dark.gif'); }
table th.tablesorter-headerDesc { background-image: url('/.../tablesorter-desc-dark.gif'); }
```

This avoids filter overhead for frequently-toggled UI elements.

---

## Filter Strategies

### Full Inversion (Invertible Images)
Applied to diagrams, charts, and line art marked `.invert` or `.invert-auto`:

```css
filter: grayscale(50%) invert(100%) brightness(95%) hue-rotate(180deg);
```

**Layer breakdown:**
1. **`grayscale(50%)`** - Reduce color saturation (applied first, before inversion)
2. **`invert(100%)`** - Flip luminance (black→white, white→black)
3. **`brightness(95%)`** - Slightly dim to reduce harshness
4. **`hue-rotate(180deg)`** - Restore color relationships after inversion

The hue rotation compensates for inversion's color distortion. For example, a blue line chart inverts to yellow; rotating 180° brings it back to blue-ish.

### Partial Desaturation (Non-Invertible Images)
Default for unmarked images (photos, complex artwork):

```css
filter: grayscale(50%);
```

Reduces visual distraction without destroying color relationships. Maintains recognizability of photographs while reducing their "brightness" in the dark UI.

### No Filtering (Opt-Out)
For manually-curated or automatically-detected complex images:

```css
#markdownBody figure img.invert-not,
#markdownBody figure img.invert-not-auto {
    filter: none;
}
```

The `#markdownBody` specificity override ensures this rule beats the default grayscale.

### Hover Behavior
All filtered images restore to original on hover. A delayed transition is defined, but a later rule disables transitions for `figure img` and variants to avoid rendering issues:

```css
figure img:not(.drop-filter-on-hover-not):hover {
    filter: none;
    transition: filter 0s ease 0.25s;  /* 250ms delay before restoring */
}
```

**Exception:** SVG images marked `.invert` keep their filter on hover (lines 94-97), likely because SVGs scale cleanly and the inverted version is more readable in dark mode.

### Alt-Text Handling
Image `::before` pseudo-elements (used for alt-text display) are separately inverted:

```css
figure img.invert::before,
figure img.invert-auto::before {
    filter: invert(1);
}
```

This ensures alt-text remains readable when the parent image is inverted. However, transitions are disabled for `figure img` and invert variants (including alt-text) due to rendering conflicts—noted as **TEMPORARY** pending migration to a color-based scheme.

---

## CSS Custom Properties

### Color Variables
Set at `:root` to override light-mode defaults:

| Variable | Value | Purpose |
|----------|-------|---------|
| `--GW-body-background-color` | `#161616` | Slightly off-black to prevent pixel toggling on OLED |
| `--GW-body-text-color` | `#f1f1f1` | Slightly off-white to reduce contrast harshness |
| `--GW-popins-popin-backdrop-color` | `rgba(0, 0, 0, 0.6)` | Modal overlay darkness |
| `--GW-popins-popin-title-bar-button-color` | `#bbb` | Button color in popin title bars |

### Pattern Variables
Reference existing image patterns defined elsewhere:

```css
--GW-popups-popup-title-bar-pattern: var(--GW-image-pattern-dotted-161616-on-252525-2x-gif);
--GW-popups-popup-title-bar-pattern-focused: var(--GW-image-pattern-dotted-161616-on-3e3e3e-2x-gif);
--GW-checkerboard-scrollbar-background-image: var(--GW-image-checkerboard-888-000-2x-gif);
--GW-checkerboard-scrollbar-hover-background-image: var(--GW-image-checkerboard-bfbfbf-000-2x-gif);
```

These are 2x-resolution GIFs for retina displays, with colors chosen to match the dark background (`#161616`).

---

## UI Element Adjustments

### Loading Spinners
Popup loading indicators are inverted and faded:

```css
.popframe.loading::before {
    filter: invert(1);
    opacity: 0.4;
}
```

The light-mode spinner is presumably dark-colored, so inversion makes it visible on the dark background.

### Loading Failed Messages
Error messages are simply faded without inversion:

```css
.popframe.loading-failed::after {
    opacity: 0.4;
}
```

This suggests the error text/icon is already styled appropriately for dark mode via color variables.

### Masked Links Key Toggle
Info alert images get a drop-shadow for visibility:

```css
div#masked-links-key-toggle-info-alert img {
    filter: drop-shadow(0 0 3px var(--GW-reader-mode-masked-links-key-toggle-info-alert-panel-text-shadow-color));
}
```

The shadow color is pulled from a reader-mode variable, maintaining consistency across features.

### Icons
Special icons are inverted to be visible:

```css
.has-recently-modified-icon .recently-modified-icon-hook::before,
.manicule svg {
    filter: invert(1);
}
```

These are decorative elements that need to stand out against the dark background.

---

## Integration Points

### Load Order
This file is bundled into `dark-mode-GENERATED.css` and inlined in the `<head>` with a media attribute. JavaScript toggles the media attribute to force dark, force light, or follow system preference; it does not set `--dark-mode-invert-filter`.

### Server-Side Coordination
Image classes like `.invert-auto` and `.invert-not-auto` are applied during Hakyll build:

- **[typography-hs](../backend/typography-hs)** or annotation pipeline analyzes images
- Heuristics determine invertibility (line art vs photos, color complexity)
- Classes are baked into HTML at build time

This shifts the computational cost from client to build-time.

### Cascade Relationship
This file **overrides** base styles defined in other stylesheets:

- Base colors likely in `default.css` or `colors.css`
- Popup styles in `popups.css`
- Table styles in `tablesorter.css`

The specificity is generally equal, so load order matters—this must load **after** base styles.

---

## Design Patterns

### Filter Composition Order
CSS filters apply left-to-right, so the order in `grayscale(50%) invert(100%) brightness(95%) hue-rotate(180deg)` is critical:

1. Desaturate colors (reduces extremes)
2. Invert luminance (main transformation)
3. Reduce brightness (compensate for perceived brightness increase)
4. Rotate hue (restore color relationships)

Reordering would produce different results. For example, inverting before desaturating would yield more saturated colors.

### Performance Considerations
Several choices prioritize rendering performance:

- **Off-black background** (`#161616`) prevents OLED pixel toggle delay causing scroll jank
- **Separate dark icon files** for tablesorter instead of filters (fewer GPU operations)
- **Transition delays** (250ms) prevent filter thrashing on quick hovers
- **Disabled transitions** for alt-text to avoid layout recalculation bugs

### Graceful Degradation
The filter variable pattern allows safe failure:

```css
filter: var(--dark-mode-invert-filter, none);
```

If JavaScript fails to set the variable, no filter is applied (graceful light-mode fallback).

### Accessibility Trade-offs
The slight off-white text (`#f1f1f1` vs `#fff`) and off-black background reduce contrast ratio from 21:1 to ~19:1. The comment explains this is **intentional**:

> "contrast from being *too* high & 'stark'"

This suggests user testing found pure white-on-black uncomfortable, possibly due to halation effects in dark environments.

---

## Common Patterns

### `.invert` vs `.invert-auto`
The parallel class system allows mixing manual curation with automation:

- **Manual** (`.invert`): Author explicitly marks diagram as invertible
- **Auto** (`.invert-auto`): Build system's heuristic marks it
- **Override**: Manual `.invert-not` can override auto-classification

This is visible in selectors like:
```css
figure img.invert,
figure img.invert-auto {
    /* same treatment */
}
```

### Pseudo-Element Filtering
Several rules target `::before` and `::after` separately:

```css
.dark-mode-invert::before,
.dark-mode-invert::after { filter: ... }

figure img.invert::before,
figure img.invert-auto::before { filter: invert(1); }
```

This handles icon fonts, generated content, and alt-text rendering that may have opposite color needs from their parent.

### Scoped Overrides
The `#markdownBody` qualifier on `.invert-not` rules (lines 131-134) ensures the override only applies to content images, not UI elements that might coincidentally use the same class.

---

## Known Issues

### Alt-Text Transition Bug
Lines 109-122 disable transitions for image alt-text:

```css
/* TEMPORARY until we transition to a color-based instead of
   filter-based scheme for this. —SA 2022-07-29 */
figure img,
figure img:hover,
/* ... */
{
    transition: none;
}
```

The comment indicates this is a **temporary workaround** for a rendering issue. Future plan is to use CSS custom properties for colors instead of filters.

### SVG Hover Inconsistency
SVG images retain their inversion filter on hover (lines 94-97), while raster images don't. This creates inconsistent UX but is presumably intentional due to SVG rendering characteristics.

---

## See Also

- [dark-mode-js](dark-mode-js) - Toggle controller, preference persistence
- [dark-mode-initial-js](dark-mode-initial-js) - Early-loading dark mode bootstrap
- [colors-css](colors-css) - Base color definitions this file overrides
- [build-mode-css](../php/build-mode-css) - PHP script that combines colors with adjustments
- [initial-css](initial-css) - Core styles that reference these color variables
- [popups-js](popups-js) - Uses the `.popframe.loading` spinner styles
