
# color.js

**Path:** `js/color.js` | **Language:** JavaScript | **Lines:** ~557

> Color space conversion and transformation utilities for image colorization

---

## Overview

`color.js` provides a comprehensive color manipulation library supporting conversions between eight color spaces (RGB, HSV, HSL, XYZ, L\*a\*b\*, YCoCg, Oklab, Oklch) and a "colorize" transform that recolors images while preserving luminance.

The primary use case is the **colorize transform**, which takes a grayscale or arbitrary-color image and applies a reference hue while maintaining the original luminance structure. This is useful for theming images to match site aesthetics. The module defaults to Oklch color space for colorization because it provides perceptually uniform hue manipulation with good gamut coverage.

The architecture follows a hub-and-spoke pattern: RGB serves as the interchange format, with XYZ as an intermediate step for perceptual color spaces (Lab, Oklab). All conversions route through RGB, so converting Lab→HSL goes Lab→XYZ→RGB→HSL.

---

## Public API

### `Color.processColorValue(colorString, transforms, options) → string`

Main entry point. Applies a sequence of color transforms to an input color string.

```javascript
Color.processColorValue("#808080", [
    { type: Color.ColorTransform.COLORIZE, referenceColor: "#3366cc" }
]);
// → "#4a6aa3" (gray tinted blue)
```

**Parameters:**
- `colorString`: Hex (`#rgb` or `#rrggbb`) or `rgb()`/`rgba()` format
- `transforms`: Array of transform objects with `type`, `referenceColor`, optional `colorSpace`
- `options.output`: `"hex"` or `"rgba"` (defaults to match input format)

**Called by:** Image processing code (likely in rewrite handlers)
**Calls:** `rgbaFromString`, `fromRGB`, `rgbFrom`, `colorValueTransform_colorize`

---

### `Color.fromRGB(rgb, targetColorSpace) → object`

Converts RGB to any supported color space.

**Called by:** `processColorValue`, external callers needing color space conversion
**Calls:** `hsvFromRGB`, `hslFromRGB`, `labFromRGB`, `yccFromRGB`, `oklabFromRGB`, `oklchFromRGB`

---

### `Color.rgbFrom(color, sourceColorSpace) → {red, green, blue}`

Converts from any supported color space back to RGB.

**Called by:** `processColorValue`, external callers
**Calls:** `rgbFromHSV`, `rgbFromHSL`, `rgbFromLab`, `rgbFromYCC`, `rgbFromOklab`, `rgbFromOklch`

---

### `Color.rgbaFromString(colorString) → {red, green, blue, alpha}`

Parses hex or rgba() color strings into component values (0-255 for RGB; alpha is parsed with `parseInt`, so fractional values are truncated).

```javascript
Color.rgbaFromString("#ff8000")  // → {red: 255, green: 128, blue: 0, alpha: 1.0}
Color.rgbaFromString("rgba(255, 128, 0, 0.5)")  // → {red: 255, green: 128, blue: 0, alpha: 0}
```

---

### `Color.hexStringFromRGB(rgb) → string`

Converts RGB object to `#rrggbb` hex string.

---

### `Color.rgbaStringFromRGBA(rgba) → string`

Converts RGBA object to `rgba(r, g, b, a)` string with padded values.

---

## Internal Architecture

### Color Space Enum

```javascript
Color.ColorSpace = {
    RGB, HSV, HSL, XYZ, Lab, YCC, Oklab, Oklch
}
```

### Conversion Graph

```
                    ┌─────┐
          ┌─────────┤ RGB ├─────────┐
          │         └──┬──┘         │
          ▼            │            ▼
       ┌─────┐         │         ┌─────┐
       │ HSV │         │         │ HSL │
       └─────┘         │         └─────┘
                       │
          ┌────────────┼────────────┐
          │            │            │
          ▼            ▼            ▼
       ┌─────┐      ┌─────┐      ┌─────┐
       │ YCC │      │ XYZ │      │ XYZ │
       └─────┘      └──┬──┘      └──┬──┘
                       │            │
                       ▼            ▼
                    ┌─────┐      ┌───────┐
                    │ Lab │      │ Oklab │
                    └─────┘      └───┬───┘
                                     │
                                     ▼
                                 ┌───────┐
                                 │ Oklch │
                                 └───────┘
```

### Transform Pipeline

1. Parse input string → RGBA
2. Save original alpha
3. For each transform:
   - Convert to working color space
   - Apply transform
   - Convert back to RGB
4. Restore alpha
5. Format output string

---

## Key Patterns

### Lightness Remapping in Colorize

The colorize transform doesn't simply copy lightness—it remaps it to avoid crushing blacks or blowing out whites:

```javascript
let baseLightness = Math.max(Math.min(referenceColor.L, maxBaseValue), minBaseValue);
color.L = baseLightness + (1.0 - baseLightness) * color.L;
```

This maps the input lightness [0,1] to [baseLightness, 1], ensuring even dark input colors get lifted to maintain the reference color's character.

### Oklch Chroma Boosting

Oklch mode includes saturation enhancement to compensate for desaturation during lightness remapping:

```javascript
let maxChroma = Color.oklchFromRGB(Color.rgbFromOklch({ L: color.L, C: 1.0, h: color.h })).C;
color.C += chromaBoostFactor * (maxChroma - color.C) * (1.0 - color.L * antiClusteringFactor);
```

The `antiClusteringFactor` reduces boosting for lighter colors to prevent them from clustering at maximum saturation.

### Gamut Correction

After chroma boosting, colors may fall outside the sRGB gamut. The Oklch path includes gamut mapping:

```javascript
let maxLightness = Color.oklchFromRGB(Color.rgbFromOklch({ L: 1.0, C: color.C, h: color.h })).L;
if (color.L > maxLightness)
    color.C *= (1.0 - color.L) / (1.0 - maxLightness);
```

This reduces chroma for overly-bright colors to keep them representable.

---

## Configuration

### `Color.ColorTransformSettings`

Per-color-space tuning parameters for the colorize transform:

| Color Space | Parameter | Default | Effect |
|-------------|-----------|---------|--------|
| Oklch | `minBaseValue` | 0.62 | Minimum lightness floor |
| Oklch | `maxBaseValue` | 0.77 | Maximum lightness floor |
| Oklch | `chromaBoostFactor` | 0.75 | Saturation enhancement strength |
| Oklch | `antiClusteringFactor` | 0.75 | Reduces boost for light colors |
| HSL | `saturationBoostFactor` | 0.50 | HSL-specific saturation boost |
| HSL | `antiClusteringFactor` | 0.25 | HSL-specific clustering prevention |

The default color space is `Oklch`, specified in `ColorTransformSettings.colorize.defaultColorSpace`.

---

## Integration Points

### Global Object

The module exports a single global `Color` object. No events or callbacks.

### Input/Output Formats

- **Input:** `#rgb`, `#rrggbb`, `rgb(r,g,b)`, `rgba(r,g,b,a)`
- **Output:** Same format as input, or specified via `options.output`

### Internal RGB Format

All internal RGB representations use:
```javascript
{ red: 0-255, green: 0-255, blue: 0-255, alpha?: 0-1 }
```

### Color Space Object Formats

| Space | Properties |
|-------|------------|
| HSV | `{ hue: 0-1, saturation: 0-1, value: 0-1 }` |
| HSL | `{ hue: 0-1, saturation: 0-1, lightness: 0-1 }` |
| XYZ | `{ x, y, z }` (D65 illuminant) |
| Lab | `{ L: 0-1, a, b }` |
| YCC | `{ Y: 0-1, Co, Cg }` |
| Oklab | `{ L: 0-1, a, b }` |
| Oklch | `{ L: 0-1, C: chroma, h: radians }` |

---

## See Also

- [colors-css](colors-css) - CSS color variables that may be transformed
- [dark-mode-js](dark-mode-js) - Theme system that uses color transformations
- [special-occasions-js](special-occasions-js) - Uses colorize for holiday theming (Christmas red/green)
- [dark-mode-adjustments-css](dark-mode-adjustments-css) - CSS that implements filter-based color changes
- [color-scheme-convert](../php/color-scheme-convert) - PHP equivalent for build-time color transforms
