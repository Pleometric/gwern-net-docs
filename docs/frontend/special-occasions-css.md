
# special-occasions.css

**Path:** `css/special-occasions.css` | **Language:** CSS | **Lines:** 391

> Holiday and special event theming (Halloween, Christmas, April Fools, Easter)

---

## Overview

This CSS file defines seasonal themes for holidays and special events throughout the year. It provides comprehensive color palette overrides and visual customizations for Halloween, Christmas, April Fools, and Easter, transforming the entire site's appearance to match each occasion's aesthetic.

The themes operate through body class selectors (e.g., `body.special-halloween-dark`, `body[class*='special-christmas']`) that are dynamically applied by [special-occasions-js](special-occasions-js) based on the current date and user's dark/light mode preference. Each theme redefines dozens of CSS custom properties to create cohesive visual experiences—from blood-red accents for Halloween to festive red-and-green color schemes for Christmas.

Beyond color changes, the themes include custom ornaments, special logo treatments, and even animated effects. The system is designed to be non-intrusive, automatically activating during appropriate time windows and gracefully reverting when the occasion ends.

---

## Themed Events

### April Fools (April 1, 8AM-3PM)

**Selector:** `.popup.april-fools-special-birthdaycat`

A minimal theme focused on a single animated Easter egg. Defines styles for a special "birthday cat" popup that drops down from the top of the viewport after 5 minutes of browsing (implemented in the JS).

**Key Styles:**
- Fixed popup dimensions: 514×704px
- Animated drop-down effect via `.offset` and `.moved` classes
- 5-second ease transition from `top: -800px` to `top: 8px`
- Positioned at right edge (`right: 64px`)

Currently the most understated theme—no color changes or widespread visual modifications.

### Halloween (October 31, 6PM → November 1, 6AM)

**Selector:** `body.special-halloween-dark`

A gothic, horror-themed palette using shades of blood red against the dark mode backdrop. The theme is deliberately nighttime-only (activated at 6PM on Oct 31) to avoid celebrating "while there is daylight."

**Color Palette:**
```css
--GW-blood-red: #b00           /* Primary red */
--GW-blood-red-dried: #700     /* Darker, muted red */
--GW-blood-red-arterial: #e00  /* Bright, vivid red */
```

**What Changes:**
- **Navigation & headers:** Red borders and links throughout (`--GW-nav-header-link-color`)
- **Typography:** Red drop caps, red heading borders (H1, H2)
- **UI elements:** Red borders on figures, footnotes, sidenotes, collapse buttons
- **Interactive elements:** Red hover states, red scroll indicators, red back-to-top links
- **Ornaments:** Custom Halloween-themed horizontal rule decorations:
  - `sun-verginasun-black.svg` (1st occurrence)
  - `japanesecrest-tsukinihoshi-dottedmoon.svg` (2nd)
  - `asterism-triplewhitestar.svg` (3rd)
- **Site-of-the-day:** Custom wavy lines ornament (`three-wavy-lines-ornament-left/right.svg`)
- **Logo:** Hidden SVG logo, replaced with special Halloween bitmap version (JS-controlled)

**Special Touches:**
- `.heading` text colored in blood red
- Mobile sidebar links in arterial red
- Custom horizontal rule ornaments (black sun, dotted moon, triple stars)
- Logo image scaled and repositioned at different breakpoints

**Media Query Adjustments:**
- Mobile (`max-width: 649px`): Logo scaled to 133%, offset -15% left/top
- Tablet (`650-1179px`): Logo scaled to 150%, offset -30% left, -15% top
- Desktop (`min-width: 1180px`): Logo scaled to 150%, offset -30% left, -10% top

### Christmas (December 24, 6PM → December 25, all day)

**Selectors:** `body.special-christmas-light` and `body.special-christmas-dark`

A festive red-and-green theme with two separate palettes for light and dark modes. The most elaborate theme, featuring extensive color variations for different shades of green (holly, mistletoe, pine, fir, spruce) and red (Santa hat, holly berry, Christmas lights).

**Light Mode Palette:**
```css
--GW-santa-hat-red: #900
--GW-holly-berry-red: #c00
--GW-holly-leaf-green: #060
--GW-christmas-lights-red: #f00
--GW-mistletoe-green: #0c0
--GW-pine-needle-green: #050
--GW-fir-needle-green: #014421
--GW-spruce-needle-green: #4bd24d
--GW-christmas-lights-gold: #ff0
```

**Dark Mode Palette:**
```css
--GW-santa-hat-red: #d00
--GW-holly-berry-red: #f00
--GW-holly-leaf-green: #0a3
--GW-christmas-lights-red: #f00
--GW-mistletoe-green: #005519
--GW-pine-needle-green: #5eff66
--GW-spruce-needle-green: #5ce15b
--GW-fir-needle-green: #a7cab6
--GW-christmas-lights-gold: #996515
```

**What Changes (both modes):**
- **Navigation:** Green headers with red hover states
- **Typography:** Red drop caps, green heading borders
- **Links:** Pine green default, fir green visited, holly berry red hover
- **Blockquotes:** Alternating color schemes by nesting level:
  - Odd levels (1,3,5): Red links, green hovers
  - Even levels (2,4,6): Green links, red hovers
- **Lists:** Alternating colored bullets via `ol > li:nth-of-type(odd/even)::before`
  - Odd items: Spruce needle green
  - Even items: Christmas lights red
- **UI elements:** Green borders universally (TOC, figures, footnotes, collapse buttons)
- **Logo:** Christmas-themed logo (hidden SVG, replaced via JS)

**Special Effects:**
- **Text glow:** Headers, links, and drop caps have gold Christmas light glow
  - Light mode: Subtle 1-2px glow
  - Dark mode: Stronger glow with background color shadow for contrast
- **Dynamic colorization (JS-driven):**
  - List bullets alternating red/green
  - Horizontal rules alternating red/green
  - "Site of the day" background ornament in green
  - Footer logo in red

**Media Query Adjustments:**
- Mobile: Logo 133% scale, -15% offset
- Tablet: Logo 150% scale, -30%/-15% offset
- Desktop: Logo 125% scale, -10%/-10% offset (less dramatic than Halloween)

### Easter (Exact dates through 2050)

**Selector:** `body.special-easter`

Placeholder theme with minimal implementation. Currently only adds a class; no visual changes defined in CSS. The JS file shows a commented-out logo injection, suggesting this is a work-in-progress.

---

## CSS Custom Properties

The theming system works by overriding the site's global CSS variables. Each theme redefines 30-50+ properties to create cohesive color schemes.

### Commonly Overridden Properties

**Navigation & Headers:**
- `--GW-nav-header-link-color`
- `--GW-nav-header-link-hover-color`
- `--GW-H1-border-color`
- `--GW-H2-border-color`

**Content Borders:**
- `--GW-TOC-border-color`
- `--GW-abstract-border-color`
- `--GW-pre-element-border-color`
- `--GW-figure-outline-color`
- `--GW-figure-caption-outline-color`

**Typography:**
- `--GW-body-text-color` (only in `.heading` contexts)
- `--GW-body-link-color`
- `--GW-body-link-visited-color`
- `--GW-body-link-hover-color`
- `--GW-epigraph-quotation-mark-color`

**Drop Caps (all 4 variants):**
- `--GW-dropcaps-yinit-color`
- `--GW-dropcaps-yinit-text-shadow-color`
- `--GW-dropcaps-de-zs-color`
- `--GW-dropcaps-cheshire-color`
- `--GW-dropcaps-kanzlei-color`

**Footnotes & Sidenotes:**
- `--GW-footnotes-section-top-rule-color`
- `--GW-footnote-border-color`
- `--GW-footnote-backlink-border-color`
- `--GW-footnote-backlink-border-hover-color`
- `--GW-footnote-highlighted-border-color`
- `--GW-sidenote-border-color`
- `--GW-sidenote-self-link-border-color`
- `--GW-sidenote-highlight-box-shadow-color`

**UI Controls:**
- `--GW-collapse-disclosure-button-text-color`
- `--GW-collapse-disclosure-button-text-hover-color`
- `--GW-page-toolbar-border-color`
- `--GW-page-toolbar-control-button-color`
- `--GW-back-to-top-link-color`
- `--GW-back-to-top-link-hover-color`

**Decorative Elements:**
- `--GW-x-of-the-day-border-color`
- `--GW-bottom-ornament-line-color`
- `--GW-floating-header-scroll-indicator-color`
- `--GW-highlighted-link-outline-color`

### Halloween-Specific

```css
--GW-blood-red
--GW-blood-red-dried
--GW-blood-red-arterial
```

### Christmas-Specific

```css
--GW-santa-hat-red
--GW-holly-berry-red
--GW-holly-leaf-green
--GW-christmas-lights-red
--GW-mistletoe-green
--GW-pine-needle-green
--GW-fir-needle-green
--GW-spruce-needle-green
--GW-christmas-lights-gold
```

---

## Key Selectors

### Body Classes

Special occasion themes are activated through body classes:

```css
body.special-halloween-dark        /* Halloween dark mode (primary) */
body.special-christmas-light       /* Christmas light mode */
body.special-christmas-dark        /* Christmas dark mode */
body[class*='special-christmas']   /* Both Christmas modes */
body.special-easter                /* Easter */
```

**Note:** The JS can apply `special-halloween-light` based on `DarkMode.computedMode()`, but the CSS defines no rules for it—only `special-halloween-dark` has styles. April Fools does not use a `body.special-april-fools` class; its styles target `.popup.april-fools-special-birthdaycat` directly.

**Pattern:** `body.special-{occasion}-{mode}` or just `body.special-{occasion}`

The attribute selector `[class*='special-christmas']` targets both light and dark Christmas modes simultaneously, used for shared styles.

### Logo Manipulation

All themes except April Fools replace the site logo:

```css
/* Hide default SVG logo */
body.special-halloween-dark #sidebar svg.logo-image { visibility: hidden; }

/* Show replacement when JS injects it */
body.special-halloween-dark #sidebar svg.logo-image.visible { visibility: visible; }

/* Style replacement image */
body.special-halloween-dark #sidebar span.logo-image img {
    filter: none;
    /* Responsive sizing and positioning at different breakpoints */
}
```

The pattern: hide the SVG, inject a bitmap `<img>` wrapped in `<span class="logo-image">`, scale and reposition it.

### Blockquote Nesting (Christmas)

Christmas theme creates alternating color schemes for nested blockquotes:

```css
/* Odd nesting levels: red links */
body[class*='special-christmas'] blockquote.blockquote-level-1,
body[class*='special-christmas'] blockquote.blockquote-level-3,
body[class*='special-christmas'] blockquote.blockquote-level-5 {
    --GW-body-link-color: var(--GW-holly-berry-red);
    --GW-body-link-visited-color: var(--GW-santa-hat-red);
    --GW-body-link-hover-color: var(--GW-pine-needle-green);
}

/* Even nesting levels: green links */
body[class*='special-christmas'] blockquote.blockquote-level-2,
body[class*='special-christmas'] blockquote.blockquote-level-4,
body[class*='special-christmas'] blockquote.blockquote-level-6 {
    --GW-body-link-color: var(--GW-pine-needle-green);
    --GW-body-link-visited-color: var(--GW-fir-needle-green);
    --GW-body-link-hover-color: var(--GW-holly-berry-red);
}
```

### Ornament Replacements (Halloween)

Custom SVG ornaments for horizontal rules:

```css
body.special-halloween-dark hr::after {
    filter: none;
    opacity: 1;
}
body.special-halloween-dark hr.horizontal-rule-nth-1::after {
    background-image: url('/static/img/ornament/halloween/sun-verginasun-black.svg');
}
body.special-halloween-dark hr.horizontal-rule-nth-2::after {
    background-image: url('/static/img/ornament/halloween/japanesecrest-tsukinihoshi-dottedmoon.svg');
}
body.special-halloween-dark hr.horizontal-rule-nth-3::after {
    background-image: url('/static/img/ornament/halloween/asterism-triplewhitestar.svg');
}
```

The pattern cycles through 3 Halloween-specific ornaments for sequential horizontal rules.

### Index Page Overrides

Both Halloween and Christmas have special styles for the homepage:

```css
body.page-index.special-halloween-dark #sidebar a:hover {
    color: var(--GW-blood-red);
}
body.page-index.special-halloween-dark #sidebar a.logo {
    pointer-events: auto;  /* Make logo clickable on index */
}

body.page-index[class*='special-christmas'] #sidebar a:hover {
    color: var(--GW-holly-leaf-green);
}
body.page-index[class*='special-christmas'] #sidebar a.logo {
    pointer-events: auto;
}
```

Normally the logo link has `pointer-events: none` on the index page; special occasions override this to link to special dropcap pages (`/dropcap#halloween`, `/dropcap#christmas`).

### Responsive Breakpoints

Logo scaling uses three breakpoints:

```css
/* Mobile: up to 649px */
@media all and (max-width: 649px) { ... }

/* Tablet: 650-1179px */
@media all and (min-width: 650px) {
    @media all and (max-width: 1179px) { ... }
}

/* Desktop: 1180px+ */
@media all and (min-width: 650px) {
    @media all and (min-width: 1180px) { ... }
}
```

Each breakpoint adjusts logo `width`, `left`, and `top` positioning differently to maintain visual balance.

---

## Integration Points

### JavaScript Activation (special-occasions.js)

The CSS is passive—all activation logic lives in [special-occasions-js](special-occasions-js):

**Date Detection:**
```javascript
isItHalloween()    // Oct 31 6PM-midnight, Nov 1 midnight-6AM (language-gated)
isItChristmas()    // Dec 24 6PM through Dec 25
isItAprilFools()   // Apr 1, 8AM-3PM
isItEaster()       // Exact dates hardcoded through 2050
```

**Class Application:**
```javascript
applySpecialOccasionClasses()  // Called on page load and dark mode changes
```

Runs through `GW.specialOccasions` array, calling each occasion's test function. If true, applies custom logic (or falls back to adding `body.special-{name}` class).

**Mode-Specific Classes:**
Halloween and Christmas themes check `DarkMode.computedMode()` and apply mode-specific classes:
```javascript
// Halloween always uses dark
let specialClass = "special-halloween-dark";

// Christmas varies by mode
let specialClass = DarkMode.computedMode() == "light"
    ? "special-christmas-light"
    : "special-christmas-dark";

document.body.classList.add(specialClass);
```

**Logo Replacement:**
```javascript
injectSpecialPageLogo("halloween", {
    mode: "dark",
    sequence: "previousBeforeSaved",  // Cycle through multiple logos
    link: "/dropcap#halloween"
});
```

Replaces the default SVG logo with holiday-specific bitmap images. The CSS then styles these with `filter: none` and responsive positioning.

**Dynamic Colorization (Christmas only):**
```javascript
colorizeElements([
    [ "ul > li:nth-of-type(odd)", "--list-bullet", "#f00" ],
    [ "ul > li:nth-of-type(even)", "--list-bullet", "#0f0" ],
    [ "hr", "--icon-image", "#f00" ],
    [ "#x-of-the-day", "--ornament-image-left", "#f00" ]
]);
```

Programmatically recolors SVG icons by parsing `data:` URIs and replacing color codes. This allows alternating red/green bullets and ornaments without hardcoding dozens of CSS rules.

### Dark Mode System

Themes respond to dark mode changes:
```javascript
GW.notificationCenter.addHandlerForEvent("DarkMode.computedModeDidChange", (info) => {
    applySpecialOccasionClasses();  // Reapply with new mode
});
```

When user toggles dark mode during Christmas, the system switches between `-light` and `-dark` body classes, swapping entire color palettes.

### Testing/Debugging

**Test Pages:**
- `/lorem-halloween` → `body.test-halloween`
- `/lorem-christmas` → `body.test-christmas`
- `/lorem-april-fools` → `body.test-april-fools`
- `/lorem-easter` → `body.test-easter`

**Console Function:**
```javascript
toggleSpecialOccasionTest("christmas", true)   // Force-enable
toggleSpecialOccasionTest("christmas", false)  // Disable
toggleSpecialOccasionTest()                    // Clear all tests
```

Stored in `localStorage` with key `special-occasion-test-{name}`.

### Asset Loading

The JS handles all image loading, but CSS references ornament assets:

**Halloween ornaments:**
- `/static/img/ornament/halloween/sun-verginasun-black.svg`
- `/static/img/ornament/halloween/japanesecrest-tsukinihoshi-dottedmoon.svg`
- `/static/img/ornament/halloween/asterism-triplewhitestar.svg`
- `/static/img/ornament/halloween/three-wavy-lines-ornament-left.svg`
- `/static/img/ornament/halloween/three-wavy-lines-ornament-right.svg`
- `/static/img/ornament/halloween/swissspiralroll.svg`

**Christmas logos (JS-loaded):**
- `/static/img/logo/christmas/{light|dark}/logo-christmas-{light|dark}-{N}-small-{1x|2x|3x}.{png|jpg|webp}`

**Halloween logos (JS-loaded):**
- `/static/img/logo/halloween/dark/logo-halloween-dark-{N}-small-{1x|2x|3x}.{png|jpg|webp}`

Logo selection supports:
- Multiple numbered variants for randomization
- Three pixel densities (1x, 2x, 3x) for retina displays
- SVG or bitmap formats
- Sequencing modes (random, next/previous)

---

## See Also

- [special-occasions-js](special-occasions-js) - Date detection, class application, logo injection, SVG colorization
- [dark-mode-js](dark-mode-js) - Mode computation and change events that trigger theme updates
- [colors-css](colors-css) - Base color variables overridden by holiday themes
- [dark-mode-adjustments-css](dark-mode-adjustments-css) - Dark mode base that Halloween builds on
- [color-js](color-js) - Color transformations used for list bullet colorization
- [initial-js](initial-js) - Utility functions (`doWhenBodyExists`, `doWhenMatchMedia`)

---

**Implementation Notes:**

1. **Why separate light/dark classes?** Christmas works in both modes, with distinct color palettes optimized for each background. Halloween sets `DarkMode.defaultMode = "dark"` to encourage dark mode, and while the JS can technically apply a `special-halloween-light` class, the CSS only defines styles for `special-halloween-dark`—light mode users see no Halloween theming.

2. **Why hide the SVG logo?** The default logo is an inline SVG for fast rendering and styling flexibility. Special occasion logos are often bitmaps (photos, complex illustrations) that can't be easily embedded inline, so they're loaded as `<img>` elements and the SVG is hidden.

3. **Ornament cycling:** The `horizontal-rule-nth-{1,2,3}` classes cycle through three ornaments. If more than 3 `<hr>` elements exist on a page, the pattern repeats (4th uses ornament 1, 5th uses ornament 2, etc.).

4. **Performance:** All themes use CSS custom properties, so changing themes is instant—just swapping variables. The only performance cost is the logo image load (mitigated by responsive image selection and version hashing).

5. **Accessibility:** Color changes maintain WCAG contrast ratios. The glow effects on Christmas text are subtle (1-2px) and don't interfere with readability.
