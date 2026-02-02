
# colors.css

**Path:** `css/colors.css` | **Language:** CSS | **Lines:** 335

> Light mode color design tokens

---

## Overview

The `colors.css` file defines all color design tokens for gwern.net's light mode theme using CSS custom properties (CSS variables). It serves as the foundational color palette that establishes the visual identity of the site, defining over 150 `--GW-*` prefixed color variables that control every colorable element from body text to popup shadows.

This file follows a systematic approach to color theming: every color value is abstracted into a semantic CSS variable rather than using hardcoded hex values directly in stylesheets. This enables the entire site's color scheme to be overridden for dark mode through a single generated stylesheet (`dark-mode-GENERATED.css`) that redefines these same variables with appropriate dark-mode values.

The color system is organized into logical categories (links, blockquotes, tables, popups, etc.), with each category containing all necessary color tokens for that component. Values are primarily specified as hex colors (`#fff`, `#888`) with occasional use of `rgba()` for transparency effects (backdrops, overlays). The naming convention is semantic rather than presentational: `--GW-body-link-hover-color` instead of `--GW-dark-gray`.

---

## Color Categories

The design system organizes colors into 30+ semantic categories:

### Core Colors
- **Body**: Background (`#fff`), text (`#000`), selection
- **Links**: Default, hover, visited, and inverted variants
- **Typography**: Headings, lists, dropcaps

### Content Blocks
- **Blockquotes**: Border and background colors for 4 nesting levels
- **Abstracts**: Border styling
- **Block context highlighting**: Background for highlighted spans
- **Collapse blocks**: Disclosure buttons (regular and in-blockquote variants)
- **Inline collapses**: Text-only disclosure styling
- **Aux-links collapse**: Border colors

### Navigation & UI
- **Table of Contents**: 12 variables for borders, backgrounds, hover states, numbers
- **Page toolbar**: Control buttons, icons, selected states
- **Mobile floating header**: Shadows, scroll indicators
- **Nav header**: Link colors and hover states
- **"Back to top" link**: Default and hover
- **"Skip to content" accessibility link**: Text, border, background

### Typography Elements
- **Headings**: H1/H2 border colors
- **Lists**: Bulleted list marker colors
- **Epigraphs**: Quotation mark styling
- **Dropcaps**: 5 different font styles (Goudy, Yinit, De Zs, Cheshire, Kanzlei)

### Footnotes & Sidenotes
- **Footnotes**: Borders, highlights, backlinks, section dividers
- **Footnote references**: Outline colors for highlighting
- **Sidenotes**: Shadows, borders, scrollbars, self-links
- **Annotations**: Section highlighting borders

### Tables & Data
- **Tables**: Borders, captions, scrollbars, hover states, zebra striping
- **Sorted columns**: Background and text colors for active sort
- **Row hover**: Outline colors

### Code & Syntax
- **Code elements**: Inline code borders and backgrounds
- **Pre blocks**: Borders, backgrounds, scrollbars, line highlights
- **Syntax highlighting**: 25 token type colors (keywords, comments, strings, etc.)
- **Math blocks**: Backgrounds, flash animations, scrollbars

### Interactive Elements
- **Figures**: Outline colors for figures and captions
- **Embeds**: Border colors
- **Comments section**: Top border separator
- **Image focus**: Drop shadow on hover

### Pop-frames System
- **Popups**: 15 variables for backgrounds, borders, shadows, title bars, scrollbars, focused states
- **Popins**: Backgrounds, borders, backdrops, title bars, scrollbars, stack counters
- **Extracts options dialog**: 11 variables for the configuration UI
- **Object popframes**: Background colors

### Admonitions
- **Note**: Left border (`#909090`), background (`#d8d8d8`)
- **Tip**: Left border (`#d8d8d8`), background (`#f0f0f0`)
- **Warning**: Left border (`#5a5a5a`), background (`#9a9a9a`), white text
- **Error**: Left border (`#2d2d2d`), background (`#5a5a5a`), white text
- **Reversed links**: Special link colors for dark admonitions

### Special Features
- **Reader mode**: Alert panel with dark semi-transparent background
- **X of the day**: Border colors
- **Footer**: Bottom ornament line color

---

## Key Variables

### Most Frequently Used

```css
/* Core foundation */
--GW-body-background-color: #fff;
--GW-body-text-color: #000;

/* Primary link colors */
--GW-body-link-color: #333;
--GW-body-link-hover-color: #888;
--GW-body-link-visited-color: #666;

/* Selection */
--GW-text-selection-background-color: #333;
--GW-text-selection-color: #fff;
```

### Component Neutrals

```css
/* Standard borders */
--GW-table-border-color: #000;               /* Strong borders */
--GW-TOC-border-color: #ccc;                 /* Light borders */
--GW-popups-popup-border-color: #ccc;        /* UI chrome */

/* Standard backgrounds */
--GW-TOC-background-color: #f8f8f8;          /* Light gray */
--GW-code-element-background-color: #fafafa; /* Near-white */
--GW-admonition-note-background-color: #d8d8d8; /* Mid-gray */
```

### Interactive States

```css
/* Hover transformations */
--GW-body-link-hover-color: #888;            /* Lighter on hover */
--GW-TOC-link-hover-background-color: #ececec;
--GW-collapse-disclosure-button-hover-color: #ddd;

/* Focus states (popups) */
--GW-popups-popup-border-focused-color: #aaa;
--GW-popups-popup-title-focused-color: #000;
--GW-popups-popup-scrollbar-thumb-focused-color: #ccc;
```

### Syntax Highlighting (Pandoc theme)

```css
--GW-syntax-highlight-color-normal: #1f1c1b;     /* Base text */
--GW-syntax-highlight-color-keyword: #002561;    /* Dark blue */
--GW-syntax-highlight-color-comment: #77947b;    /* Muted green */
--GW-syntax-highlight-color-control-flow: #003900; /* Dark green */
--GW-syntax-highlight-color-alert: #bf0303;      /* Red */
--GW-syntax-highlight-color-error: #ff0000;      /* Bright red */
```

### Transparency Effects

```css
/* Semi-transparent overlays */
--GW-popins-popin-backdrop-color: rgba(0, 0, 0, 0.4);
--GW-extracts-options-dialog-backdrop-background-color: rgba(255, 255, 255, 0.95);
--GW-reader-mode-masked-links-key-toggle-info-alert-panel-background-color: rgba(0, 0, 0, 0.8);

/* Semi-transparent buttons */
--GW-TOC-collapse-button-color: rgba(248, 248, 248, 0.8);
```

---

## Design System

### Naming Convention

All variables follow the pattern: `--GW-{component}-{element}-{property}-{state?}`

**Examples:**
- `--GW-body-link-color` → component: body, element: link, property: color
- `--GW-popups-popup-title-bar-button-color-hover` → component: popups, element: popup-title-bar-button, property: color, state: hover
- `--GW-table-zebra-stripe-alternate-row-background-color` → component: table, element: zebra-stripe-alternate-row, property: background-color

**Naming principles:**
- **Semantic, not presentational**: `--GW-body-link-color` (semantic) not `--GW-dark-gray-1` (presentational)
- **Component-first organization**: All variables for a component are grouped together
- **State suffixes**: `-hover`, `-focused`, `-disabled`, `-active`, `-highlighted`
- **Multi-level nesting**: Blockquotes use `-level-one` through `-level-four` suffixes
- **Inverted variants**: Links on dark backgrounds get `-inverted-` variants

### Color Value Patterns

**Grayscale progression:**
- `#fff` → white (backgrounds)
- `#fafafa`, `#f8f8f8`, `#f6f6f6`, `#f0f0f0` → very light grays (subtle backgrounds)
- `#ececec`, `#e6e6e6`, `#e4e4e4`, `#ddd`, `#d8d8d8` → light grays (borders, hover states)
- `#ccc`, `#c8c8c8`, `#c4c4c4`, `#bbb`, `#aaa` → mid grays (UI chrome, borders)
- `#999`, `#909090`, `#888`, `#777` → dark grays (text, icons)
- `#666`, `#555`, `#444`, `#333` → very dark grays (text, links)
- `#2d2d2d`, `#1f1c1b`, `#1b1b1b`, `#191919`, `#0d0d0d`, `#000` → near-black to black

**Chromatic colors** (rare in light mode):
- Blues: `#8bd0ed` (sorted table headers), `#002561` (syntax highlighting)
- Greens: `#77947b`, `#003900` (syntax highlighting)
- Reds: `#bf1722` (skip-to-content), `#bf0303`, `#ff0000` (alerts/errors)
- Teal: `#e2f0f2` (table hover)
- Yellow: `#ffd` (code line highlight)

### Variable References

Some variables reference other variables using `var()`:

```css
--GW-popups-popup-background-color: var(--GW-body-background-color);
--GW-popins-popin-background-color: var(--GW-body-background-color);
--GW-extracts-options-dialog-background-color: var(--GW-body-background-color);
--GW-extracts-options-dialog-button-background-color: var(--GW-body-background-color);
--GW-popups-popup-title-link-hover-color: var(--GW-body-link-hover-color);
--GW-popups-popup-title-link-hover-focused-color: var(--GW-body-link-hover-color);
```

This ensures consistency: changing the body background automatically updates all dependent components.

### Dark Mode Override Strategy

The entire light mode palette is redefined for dark mode through a generated stylesheet:

1. **Light mode (colors.css)**: Defines all `--GW-*` variables with light values under `:root`
2. **Dark mode (dark-mode-GENERATED.css)**: Redefines the same variables under `:root` in a separate stylesheet
3. **Component stylesheets**: Use `var(--GW-*)` for color values
4. **Automatic switching**: JavaScript toggles the `media` attribute on `#inlined-styles-colors-dark` (and dark favicons) between `all` (active) and `not all` (disabled), not by toggling a body class

**Example of how a variable is overridden:**

```css
/* colors.css (light mode) - always loaded */
:root {
  --GW-body-background-color: #fff;
  --GW-body-text-color: #000;
}

/* dark-mode-GENERATED.css (dark mode) - activated via media attribute */
:root {
  --GW-body-background-color: #000;
  --GW-body-text-color: #fff;
}
```

The dark mode stylesheet has `media="not all"` by default (disabled). When dark mode is activated, JavaScript changes it to `media="all"`, causing the dark mode `:root` definitions to override the light mode ones through CSS source order.

### Gradual Intensity Levels

Components with nested or progressive states use systematic color progressions:

**Blockquote nesting (progressively darker):**
```css
--GW-blockquote-background-color-level-one: #f8f8f8;
--GW-blockquote-background-color-level-two: #e6e6e6;
--GW-blockquote-background-color-level-three: #d8d8d8;
```

**Admonition severity (progressively darker):**
```css
--GW-admonition-tip-background-color: #f0f0f0;      /* Lightest */
--GW-admonition-note-background-color: #d8d8d8;     /* Light */
--GW-admonition-warning-background-color: #9a9a9a;  /* Dark */
--GW-admonition-error-background-color: #5a5a5a;    /* Darkest */
```

**Popup focus states (darker when focused):**
```css
--GW-popups-popup-border-color: #ccc;               /* Unfocused */
--GW-popups-popup-border-focused-color: #aaa;       /* Focused */
```

---

## Integration Points

### Dark Mode Generation

The dark mode color system is **generated, not hand-authored**:

1. **Generation script** (`build/color-scheme-convert.php` or similar) reads `colors.css`
2. **Transforms each color** using algorithmic rules:
   - Invert lightness in HSL color space
   - Adjust saturation for readability
   - Special case certain hues (greens, blues)
   - Preserve relative contrast ratios
3. **Outputs `dark-mode-GENERATED.css`** with all variables redefined inside `body.dark-mode { ... }`
4. **Manual adjustments** can override specific variables in `dark-mode-adjustments.css`

### Dark Mode Adjustments

[dark-mode-adjustments-css](dark-mode-adjustments-css) applies **non-color** dark mode fixes:
- Image filters (`filter: invert()` for certain graphics)
- Opacity adjustments for embeds
- `mix-blend-mode` overrides
- Special handling for SVGs, LaTeX equations, portraits

**Division of labor:**
- `colors.css` → defines light mode palette (150+ variables)
- `dark-mode-GENERATED.css` → redefines all color variables for dark mode (auto-generated)
- `dark-mode-adjustments.css` → manual CSS rules for images, filters, and edge cases

### Usage in Component Stylesheets

All component stylesheets reference these variables exclusively:

```css
/* In initial.css, default.css, popups.css, etc. */
body {
  background-color: var(--GW-body-background-color);
  color: var(--GW-body-text-color);
}

a {
  color: var(--GW-body-link-color);
}

a:hover {
  color: var(--GW-body-link-hover-color);
}
```

**Most colors** in component stylesheets use this design token system, though some hardcoded colors still appear in edge cases (e.g., the developer console in `default.css` uses `#fff` and `#000` directly).

### Build System Integration

The color generation happens during the **build process** (`sync.sh`):

```bash
# Pseudo-code representation
php build/color-scheme-convert.php \
  --input css/colors.css \
  --output css/dark-mode-GENERATED.css \
  --mode invert-hsl
```

This ensures the dark mode stylesheet is always in sync with the light mode source of truth.

### JavaScript Interactions

JavaScript (particularly `dark-mode-initial.js`) interacts with this system:

1. **Reads user preference** from localStorage
2. **Toggles `media` attribute** on the dark mode stylesheet (`#inlined-styles-colors-dark`) between `all` and `not all`
3. **CSS cascade applies** the appropriate variable definitions based on source order
4. **No JavaScript color manipulation** - all handled by CSS variables and media attribute switching

---

## See Also

- [dark-mode-adjustments-css](dark-mode-adjustments-css) - Image filters and non-color dark mode fixes
- [initial-css](initial-css) - Core layout styles that use these color variables
- [default-css](default-css) - Typography styles that use these color variables
- [dark-mode-js](dark-mode-js) - JavaScript that toggles dark mode on/off
- [color-js](color-js) - JavaScript color space conversion utilities
- [color-scheme-convert](../php/color-scheme-convert) - PHP script that generates dark mode colors
- [special-occasions-css](special-occasions-css) - Holiday themes that override these colors
