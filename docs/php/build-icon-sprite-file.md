---
title: "build_icon_sprite_file.php"
description: "This script implements an SVG sprite generation system that consolidates individual icon files into a single icons.svg sprite sheet."
sidebar_position: 4
---

# build_icon_sprite_file.php

This script implements an SVG sprite generation system that consolidates individual icon files into a single icons.svg sprite sheet.

<div className="doc-meta">
  <div><strong>Path</strong><code>build/build_icon_sprite_file.php</code></div>
  <div><strong>Language</strong>PHP</div>
  <div><strong>Lines</strong>101</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/build_icon_sprite_file.php">build/build_icon_sprite_file.php</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing PHP asset generation, build hooks, template assembly, or maintenance scripts around build_icon_sprite_file.
</div>

## Overview

This script implements an SVG sprite generation system that consolidates individual icon files into a single `icons.svg` sprite sheet. Each icon becomes a named `<view>` element that can be referenced via fragment identifiers (e.g., `icons.svg#arrow-right`), enabling efficient caching and reducing HTTP requests while maintaining the flexibility of individual icons.

The sprite construction algorithm processes each SVG file, extracts its viewBox dimensions, positions it within the sprite's coordinate space using horizontal translation, and wraps the icon contents in a `<g>` group with the appropriate transform. Icons are spaced at 1.1x their width to prevent visual overlap.

A key challenge the script addresses is normalizing heterogeneous SVG attributes across different icon sources. It handles missing viewBox attributes by inferring them from width/height, moves `transform` attributes from the root SVG to nested groups, and filters out namespace declarations that would conflict in the combined file.

## Key Functions/Variables

This script is primarily procedural with inline logic, but uses several important variables:

**`$iconSpacingFactor`**
Multiplier applied to icon width when calculating horizontal position (default: 1.1). This 10% buffer prevents adjacent icons from visually bleeding into each other.

**`$position`**
Running total of the horizontal offset for positioning the next icon in the sprite. Incremented by `iconWidth * iconSpacingFactor` for each processed icon.

**`$viewBox`**
Captured from each icon's viewBox attribute (or inferred from width/height). Used to calculate positioning and to generate the `<view>` element's viewBox.

**Processing logic:**
1. Deletes existing `icons.svg` if present
2. Iterates through all `*.svg` files in the icon directory
3. For each icon:
   - Extracts filename (becomes view ID)
   - Parses SVG attributes and contents
   - Normalizes viewBox (infers from width/height if missing)
   - Calculates horizontal translation offset
   - Creates a `<view>` element for fragment-based referencing
   - Wraps contents in `<g>` with positioning transform
4. Outputs combined sprite with all views and icons

## Input/Output

**Inputs:**
- `/static/img/icon/*.svg` - Individual SVG icon files

**Outputs:**
- `/static/img/icon/icons.svg` - Combined sprite file with `<view>` elements for each icon

**Output structure:**
```xml
<svg xmlns="http://www.w3.org/2000/svg">
  <view id="icon-name" viewBox="0 0 24 24" preserveAspectRatio="xMinYMin" />
  <g transform="translate(0, 0)"><!-- icon contents --></g>
  <view id="another-icon" viewBox="26.4 0 24 24" preserveAspectRatio="xMinYMin" />
  <g transform="translate(26.4, 0)"><!-- icon contents --></g>
  ...
</svg>
```

## Usage

Invoked by `sync.sh` when icon assets need rebuilding:

```bash
php build/build_icon_sprite_file.php
```

The script prints warnings to stdout for icons with invalid or missing viewBox attributes (and no width/height fallback), then skips those icons.

**Referencing icons in HTML/CSS:**
```html
<img src="/static/img/icon/icons.svg#arrow-right">
```

```css
.icon::before {
  content: url('/static/img/icon/icons.svg#checkmark');
}
```

<details className="generated-section">
<summary>See Also</summary>

- [LinkIcon.hs](/backend/link-icon-hs) - Haskell module that assigns icons to URLs
- [Config.LinkIcon](/backend/config-link-icon-hs) - URL-to-icon mapping rules and color constants
- [links.css (CSS)](/css/links) - CSS rendering of icon attributes from the sprite
- [links.css (Frontend)](/frontend/links-css) - Frontend documentation for link icon system
- [sync.sh](/backend/sync-sh) - Build orchestrator that invokes sprite generation
- [default.css](/css/default) - Main stylesheet that may reference icon styles
</details>
