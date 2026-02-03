
# Image.hs

**Path:** `build/Image.hs` | **Language:** Haskell | **Lines:** ~303

> Image processing: dimensions, inversion detection for dark mode, and lazy loading setup

---

## Overview

Image.hs handles all image-related processing during the gwern.net build pipeline. Its three main responsibilities are: (1) detecting which images should be color-inverted in dark mode, (2) extracting image dimensions for performant HTML rendering, and (3) adding lazy loading attributes.

The dark mode inversion system uses ImageMagick to analyze image color saturation. Images with a mean HSL saturation below 0.09 (the `invertThreshold`) are marked with an `invert-auto` CSS class so the browser can invert them when dark mode is active. This heuristic targets monochrome/white-heavy images like diagrams, charts, and scanned documents that look better inverted.

Dimension extraction solves the "layout shift" problem: by hardcoding width/height into `<img>` tags at build time, browsers can allocate the correct space before downloading images. The module also handles aspect ratio calculation and responsive image sizing (capping at 1400px width).

---

## Public API

### `isImageFilename :: FilePath -> Bool`

Returns `True` for supported image extensions (bmp, gif, ico, jpg, png, svg, xcf). Strips hash anchors before checking. Rejects URLs starting with "http" to avoid false positives on Wikimedia Commons pages.

**Called by:** Various link processors, rewrite handlers
**Calls:** `anySuffix`

### `isVideoFilename :: FilePath -> Bool`

Returns `True` for video extensions (mp4, webm, avi).

**Called by:** `imageMagickDimensions`, `imageLinkHeightWidthSet`

### `invertImageInline :: Metadata -> Inline -> IO Inline`

Main entry point for dark mode processing. Examines an image and either:
- Uses the global metadata override (`invert` key in annotation)
- Skips if `invert-not` class is present
- Runs ImageMagick heuristic to detect if image should invert

Returns the image with `invert-auto` class added if appropriate.

```haskell
invertImageInline md x@(Image (htmlid, classes, kvs) xs (p,t)) =
  do let inverted = addLazyLoadingImage $ Image (htmlid, "invert-auto":classes, kvs) xs (p,t)
     case invertGlobalOverride md (T.unpack p) of
       Just True -> return inverted
       Just False -> return x
       Nothing -> ... -- run heuristic
```

**Called by:** Pandoc AST walkers in Typography.hs
**Calls:** `invertGlobalOverride`, `invertFile`, `addLazyLoadingImage`

### `addImgDimensions :: String -> IO String`

Parses HTML string, finds all `<img>` tags, and adds:
- `height` and `width` attributes (from ImageMagick)
- `data-aspect-ratio` for responsive layouts
- `loading="lazy"` for lazy loading
- `decoding="async"` for non-blocking decode

**Called by:** HTML post-processors
**Calls:** `staticImg`, `imageMagickDimensions`, TagSoup parsers

### `imageLinkHeightWidthSet :: Inline -> IO Inline`

For `<a>` links pointing to images, adds `image-height`, `image-width`, and `data-aspect-ratio` as data attributes. Enables popups.js to size image popups correctly without reflow.

**Called by:** Link processors
**Calls:** `imageMagickDimensions`, `sizeAspectRatioKV`

### `outlineImageInline :: Inline -> IO Inline`

Detects whether an image needs a CSS outline (border) by calling an external PHP script. Adds `outline` or `outline-not` class.

**Called by:** Image processors
**Calls:** `outlineImage`, PHP subprocess

---

## Internal Architecture

### Inversion Detection Flow

```
invertImageInline
    │
    ├── Check metadata override (invertGlobalOverride)
    │   └── If present, use that
    │
    ├── Check for "invert-not" class
    │   └── If present, skip
    │
    └── invertFile
        └── invertImage
            ├── Local file: invertImageLocal
            │   └── imageMagickColor → compare to threshold
            │
            └── Remote URL: download to temp, process, cleanup
                └── invertImagePreview (if new & inverted)
```

### Key Data Flow

1. **Input:** Pandoc `Inline` (Image or Link)
2. **Processing:** ImageMagick subprocess calls
3. **Output:** Modified `Inline` with added classes/attributes

### ImageMagick Commands

```bash
# Color saturation detection (HSL lightness channel mean)
convert $file -colorspace HSL -channel g -separate +channel -format "%[fx:mean]" info:

# Dimension extraction
identify -ping -format "%h %w\n" $file
```

---

## Key Patterns

### Threshold-Based Inversion

The `invertThreshold = 0.09` was empirically tuned. Images with HSL saturation below this are typically grayscale diagrams or white-background charts:

```haskell
invertThreshold :: Float
invertThreshold = 0.09

-- Applied as:
let invertp = c < invertThreshold  -- for remote
let invertp = c <= invertThreshold -- for local (note: <= vs <)
```

The slight inconsistency (`<` vs `<=`) appears intentional—local files are trusted more than remote downloads.

### Manual Preview for New Images

When a remote image triggers inversion AND was downloaded less than 24 hours ago, the system opens an inverted preview in the browser for human verification:

```haskell
invertImagePreview :: FilePath -> IO ()
invertImagePreview f = do
  utcFile <- getModificationTime f
  utcNow  <- getCurrentTime
  let age = utcNow `diffUTCTime` utcFile
  when (age < nominalDay) $ do
    f' <- emptySystemTempFile "inverted"
    void $ runShellCommand "./" Nothing "convert" ["-negate", f, f']
    void $ runShellCommand "./" Nothing "x-www-browser" [f']
```

### Responsive Image Sizing

Images wider than 1400px are scaled proportionally to fit within content width:

```haskell
sizeAspectRatioKV :: Int -> Int -> [(String,String)]
sizeAspectRatioKV width height =
  let imageWidth = width `min` 1400
      imageShrunk = width /= imageWidth
      imageShrinkRatio = (1400::Float) / (fromIntegral width :: Float)
      imageHeight = if not imageShrunk then height
                    else round (fromIntegral height * imageShrinkRatio)
```

### Video Poster Handling

For videos, dimensions are extracted from a poster image (`-poster.jpg` suffix):

```haskell
let f'' = if isVideoFilename f' then f' ++ "-poster.jpg" else f'
```

---

## Configuration

| Constant | Value | Purpose |
|----------|-------|---------|
| `invertThreshold` | 0.09 | HSL saturation cutoff for auto-inversion |
| Max width | 1400 | Responsive image cap (body width minus sidebar) |
| Default dimensions | 320x320 | Fallback for failed remote downloads (WP thumbnail size) |

### Override Mechanisms

1. **Per-image class:** `invert-not` prevents inversion, `invert` forces it
2. **Metadata override:** `invert: True/False` in annotation database
3. **Per-image loading:** `loading="eager"` prevents lazy loading

---

## Integration Points

### External Dependencies

- **ImageMagick:** `convert` (color analysis, inversion preview), `identify` (dimensions)
- **curl:** Remote image downloads
- **PHP:** `static/build/should_image_have_outline.php` for outline detection
- **x-www-browser:** Opens preview images for verification

### Module Dependencies

- **LinkMetadataTypes:** `Metadata` type for override lookups
- **Utils:** `addClass`, `printRed`, `anySuffix`, `isLocal`, `kvLookup`
- **Text.Pandoc:** `Inline(Image, Link)` types
- **Text.HTML.TagSoup:** HTML parsing for `addImgDimensions`

### CSS Classes Added

| Class | Meaning |
|-------|---------|
| `invert-auto` | Image should be inverted in dark mode |
| `invert-not` | Explicitly prevent inversion (manual) |
| `outline` | Image needs CSS border |
| `outline-not` | Image explicitly has no border |

### HTML Attributes Added

| Attribute | Source | Purpose |
|-----------|--------|---------|
| `width` | ImageMagick | Prevent layout shift |
| `height` | ImageMagick | Prevent layout shift |
| `data-aspect-ratio` | Calculated | CSS aspect-ratio support |
| `loading="lazy"` | Hardcoded | Defer off-screen images |
| `decoding="async"` | Hardcoded | Non-blocking decode |
| `image-height` | ImageMagick | For popups.js (on links) |
| `image-width` | ImageMagick | For popups.js (on links) |

---

## See Also

- [hakyll.hs](/backend/hakyll-hs) - Main build pipeline that integrates image processing
- [invertornot.py](/python/invertornot) - GPT-4-Vision inversion classification (obsoleted)
- [image-focus.js](/frontend/image-focus-js) - Client-side lightbox that uses image dimensions
- [should_image_have_outline.php](/php/should-image-have-outline) - PHP script for outline detection
- [compressPNG](/shell/compress-png) - PNG compression optimization
- [Typography.hs](/backend/typography-hs) - Calls `invertImageInline` during AST transformation
- [popups.js](/frontend/popups-js) - Consumes `image-height`/`image-width` attributes
- [LinkMetadata.hs](/backend/link-metadata-hs) - Provides inversion override metadata
