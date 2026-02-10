
# LinkIcon.hs

**Path:** `build/LinkIcon.hs` + `build/Config/LinkIcon.hs` | **Language:** Haskell | **Lines:** ~231 + ~800

> Assigns visual icons to links based on URL/domain patterns at compile-time

---

## Overview

LinkIcon is a compile-time link annotation system that decorates every `<a>` element with icon metadata. Given a URL, it determines which icon (if any) to display, the icon type (SVG or text), and optional hover color. This replaces an earlier CSS-based approach (links.css) and JavaScript implementation (links.js) that became increasingly complex and error-prone.

The system uses a priority-ordered rule chain: organizational overrides first (e.g., DeepMind, OpenAI), then progressively specific text icons (1-letter → 2-letter → 3-letter → 4-letter quad), then SVG icons, then file types, and finally color-only styling. First match wins. Icons are implemented either as Unicode text characters or SVG files from `/static/img/icons/`.

The design prioritizes simplicity: monochrome icons matching Gwern.net's aesthetic, typographical icons where possible, and explicit test coverage for every rule. Configuration lives in `Config/LinkIcon.hs` with ~700 domain patterns.

---

## Public API

### `linkIcon :: Inline -> Inline`

Main entry point. Transforms a Pandoc `Link` inline by adding `data-link-icon`, `data-link-icon-type`, and optionally `data-link-icon-color` attributes.

```haskell
linkIcon $ Link nullAttr [Str "foo"] ("https://arxiv.org/abs/2004.13637", "")
-- → Link (...[("link-icon","𝛘"),("link-icon-type","text"),("link-icon-color","#b31b1b")]...) ...
```

**Called by:** `Typography.hs`, `LinkMetadata.hs`, `XOfTheDay.hs`
**Calls:** `Config.LinkIcon.linkIconRules`, `addIcon`, `hasIcon`, `removeIconDuplicate`

### `addIcon :: Inline -> (Text, Text, Text) -> Inline`

Adds icon attributes to a Link if not already present. Tuple is `(icon, iconType, iconColor)`.

**Called by:** `linkIcon`

### `linkIconTest :: [(Text, Text, Text, Text)]`

Returns list of URL/icon mismatches between expected and actual. Empty list = all tests pass. Run during build via `Test.hs`.

**Called by:** `Test.hs`

### `linkIconPrioritize :: IO [(Int, Text)]`

Analyzes backlinks database to find domains lacking icons that appear frequently enough to warrant one. Returns `(count, domain)` pairs sorted by frequency.

```bash
# In sync.sh, run weekly:
ghci -istatic/build/ ./static/build/LinkIcon.hs -e 'linkIconPrioritize'
```

**Called by:** `sync.sh` (periodic maintenance task)
**Calls:** `LinkBacklink.readBacklinksDB`

---

## Internal Architecture

### Icon Type System

Icons have three components encoded in HTML attributes:

| Attribute | Purpose | Example Values |
|-----------|---------|----------------|
| `data-link-icon` | Icon content | `"pdf"`, `"𝛘"`, `"NBER"`, `"🤗"` |
| `data-link-icon-type` | Rendering mode | `"svg"`, `"text"`, `"text,tri,sans"` |
| `data-link-icon-color` | Hover color (hex) | `"#b31b1b"`, `""` |

Icon types (comma-separated modifiers):
- **svg**: Load from `/static/img/icons/{icon}.svg`
- **text**: Unicode character(s)
- **tri**: 3-character layout
- **quad**: 4-character 2×2 grid (rendered as tiny SVG since Feb 2025)
- **sans/serif/mono/italic/bold/overline**: Typography modifiers

### Rule Chain (Priority Order)

```
linkIconRules → first non-empty result from:
  1. linkIconRulesOverrides  -- Org affiliations (DeepMind, OpenAI, etc.)
  2. linkIconRulesSingle     -- 1-2 character text icons
  3. linkIconRulesDouble     -- 2-character text icons
  4. linkIconRulesTriple     -- 3-character (tri) text icons
  5. linkIconRulesQuad       -- 4-character (quad) text icons
  6. linkIconRulesSVG        -- SVG file icons
  7. linkIconRulesFiletypes  -- Extension-based (.pdf, .py, etc.)
  8. linkIconRulesColors     -- Color-only (no icon, just hover color)
```

### URL Matching Helpers

```haskell
u'  url domain  -- Infix match anywhere in URL
u'' url domain  -- Exact domain match (handles /doc/www/ archives)
aU' url domains -- Any infix match
aU'' url domains -- Any exact domain match
iE  url exts    -- Extension match
```

The `u''` function uses `isHostOrArchive` to handle local archive rewrites. When URLs are archived locally to `/doc/www/example.com/...`, the original URL is preserved in `data-url-original` and rules match against that.

---

## Key Patterns

### Archive-Aware Matching

Local archives rewrite URLs but preserve originals for icon matching:

```haskell
linkIcon x@(Link (_,cl,attributes) _ (u, _))
  ...
  where originalURL = case lookup "data-url-original" attributes of
                        Nothing   -> u
                        Just url -> url
```

### Redundant Icon Suppression

If anchor text exactly matches the text icon, the icon is suppressed:

```haskell
-- "the WSJ reported..." with WSJ link doesn't show "WSJ" icon twice
removeIconDuplicate :: Inline -> Inline
```

### Manual Override Escape Hatches

Authors can override icons in Markdown:

```markdown
[Link](url){.icon-not}                              -- Suppress icon
[Link](url){link-icon="custom" link-icon-type="svg"} -- Custom icon
```

### Validation at Test Time

The test suite validates:
- All type modifiers are recognized (`linkIconTypes` whitelist)
- Character counts match format (quad=4, tri=3, text=1-2)
- CSS hex colors are well-formed (#RGB or #RRGGBB, no gray/black/white)
- Test coverage for every URL pattern

```haskell
isValidIconType "NBER" "text,quad"  -- OK: 4 chars for quad
isValidIconType "NB"   "text,quad"  -- ERROR: only 2 chars
```

---

## Configuration

All patterns live in `Config/LinkIcon.hs`:

| Export | Purpose |
|--------|---------|
| `linkIconRules` | Main dispatcher, calls all rule functions |
| `linkIconTestUnitsText` | Test cases: `[(URL, icon, type, color)]` |
| `prioritizeLinkIconMin` | Minimum backlink count (4) for suggesting new icons |
| `prioritizeLinkIconBlackList` | Domains to never suggest icons for |
| `overrideLinkIcons` | Exact URL → icon overrides |
| `linkIconTypes` | Valid type modifier strings |

### Color Constants

```haskell
redAdobe       = "#f40f02"  -- PDF
blueG          = "#4285f4"  -- Google
orangeNGE      = "#f71a00"  -- Evangelion
brownAnthropic = "#d4a27f"  -- Claude
purpleHaskell  = "#5e5086"  -- Haskell
```

---

## Integration Points

### HTML Output

```html
<a href="https://arxiv.org/abs/1234"
   data-link-icon="𝛘"
   data-link-icon-type="text"
   data-link-icon-color="#b31b1b">paper</a>
```

### CSS (`/static/css/links.css`)

Renders icons based on `data-link-icon-type`:
- SVG: Loads sprite from `/static/img/icons/`
- Text: Styled inline with font modifiers

### Client-Side Override (`rewrite.js`)

The Gwern.net logo (Fraktur 𝔊) for local essay links is set client-side at runtime, not compile-time, to allow dynamic detection.

### LinkArchive Cooperation

When `LinkArchive.hs` rewrites a URL to local archive, it preserves the original in `data-url-original`. LinkIcon checks this attribute to apply rules against the canonical URL.

### Test Integration

`Test.hs` runs `linkIconTest` as part of the build. Any mismatch between `linkIconTestUnitsText` expected values and actual `linkIcon` output fails the build.

---

## Adding a New Icon

1. **Choose icon type**: SVG for logos, text for abbreviations
2. **Add rule** to appropriate function in `Config/LinkIcon.hs`:
   ```haskell
   linkIconRulesDouble u
     ...
     | u'' u "example.com" = ("Ex", "text,sans", "#123456")
   ```
3. **Add test case** to `linkIconTestUnitsText`:
   ```haskell
   , ("https://example.com/page", "Ex", "text,sans", "#123456")
   ```
4. **For SVG**: Add file to `/static/img/icons/example.svg` (dark-mode compatible)
5. **Add to /lorem-link**: One visual test case for browser rendering

---

## See Also

- [Config.LinkIcon](/backend/config-link-icon-hs) - URL-to-icon mapping rules, color constants, and test cases
- [build_icon_sprite_file.php](/php/build-icon-sprite-file) - Combines SVG icons into sprite sheet
- [links.css (CSS)](/css/links) - CSS rendering of icon attributes
- [links.css (Frontend)](/frontend/links-css) - Frontend documentation for link icon system
- [Typography.hs](/backend/typography-hs) - Parent module that calls linkIcon during Pandoc transforms
- [LinkMetadata.hs](/backend/link-metadata-hs) - Also applies linkIcon when building annotation popups
- [hakyll.hs](/backend/hakyll-hs) - Build pipeline that invokes icon processing
