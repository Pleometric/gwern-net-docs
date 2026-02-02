
# Config/LinkIcon.hs

**Path:** `build/Config/LinkIcon.hs` | **Language:** Haskell | **Lines:** ~1,389

> URL-to-icon mapping rules: ~700 domain patterns, color constants, and validation test cases

---

## Overview

Config/LinkIcon.hs is the massive data module containing all URL-to-icon mapping rules for Gwern.net's link decoration system. It maps URLs to a `(icon, iconType, color)` tuple through a priority-ordered chain of pattern-matching functions. The module contains approximately 700 distinct URL patterns organized by icon format: organizational overrides, single-character icons, double-character, triple-character (tri), quad-character (2x2 grid), SVG icons, file extensions, and color-only styling.

The design philosophy favors explicit exhaustive coverage over generalization. Each domain is hand-mapped with carefully chosen icons (Unicode characters or SVG filenames), typography settings (sans, serif, mono, italic, bold), and brand colors extracted from official sources. Comments document the reasoning behind each choice, including references to official logos and explanations when deviating from them.

The module also contains ~700 test cases in `linkIconTestUnitsText` providing full coverage of the rule set. Every icon rule has corresponding test URLs ensuring no regressions when adding new patterns or refactoring.

---

## Public API

### `linkIconRules :: Text -> (Text, Text, Text)`

Main dispatcher. Given a URL, returns `(icon, iconType, color)` tuple by running through all rule functions in priority order and returning the first non-empty match.

```haskell
linkIconRules "https://arxiv.org/abs/1234.5678"
-- → ("𝛘", "text", "#b31b1b")

linkIconRules "https://github.com/user/repo"
-- → ("github", "svg", "")
```

**Called by:** `LinkIcon.linkIcon`
**Calls:** All rule functions in priority order

### `linkIconTestUnitsText :: [(Text, Text, Text, Text)]`

Comprehensive test suite: `[(url, expectedIcon, expectedType, expectedColor)]`. Each rule should have at least one test case.

**Called by:** `Test.hs` via `LinkIcon.linkIconTest`

### `overrideLinkIcons :: [(Text, (Text, Text, Text))]`

Exact URL overrides for special cases where inline Markdown attributes aren't available.

```haskell
overrideLinkIcons = [("/index#abstract", ("","",""))]
```

### `linkIconTypes :: [Text]`

Whitelist of valid icon type modifiers for validation:
- `"text"`, `"svg"` - Primary types
- `"quad"`, `"tri"` - Character layouts
- `"sans"`, `"serif"`, `"mono"`, `"italic"`, `"bold"`, `"overline"` - Typography
- `""` - Color-only links

### `prioritizeLinkIconMin :: Int`

Threshold for `linkIconPrioritize` to suggest new icons. Domains must appear this many times (default: 4) before being flagged as needing an icon.

### `prioritizeLinkIconBlackList :: [Text]`

Domains to skip when suggesting new icons - dead sites, sites without usable logos, or overly obscure domains.

---

## Internal Architecture

### Rule Chain (Priority Order)

```haskell
linkIconRules u = head $ filter (/=("","","")) $ map (\f -> f u)
  [ linkIconRulesOverrides   -- Organizational affiliations
  , linkIconRulesSingle      -- 1-2 character: ψ, F, Q, etc.
  , linkIconRulesDouble      -- 2 character: MR, LW, TV, etc.
  , linkIconRulesTriple      -- 3 character (tri): SSC, ANN, etc.
  , linkIconRulesQuad        -- 4 character (quad): JAMA, NBER, etc.
  , linkIconRulesSVG         -- SVG icons: github, twitter, etc.
  , linkIconRulesFiletypes   -- Extensions: .pdf, .py, .mp4, etc.
  , linkIconRulesColors      -- Color-only: danluu.com, etc.
  ]
```

### URL Matching Helpers

```haskell
u' :: Text -> Text -> Bool   -- Loose infix match
u'' :: Text -> Text -> Bool  -- Strict domain match (archive-aware)
aU' :: Text -> [Text] -> Bool   -- Any infix
aU'' :: Text -> [Text] -> Bool  -- Any domain
iE :: Text -> [Text] -> Bool    -- Extension match
```

**Archive-awareness:** `u''` uses `Utils.isHostOrArchive` to match URLs even after local archive rewriting transforms `https://example.com/page` to `/doc/www/example.com/$HASH.html`.

### Color Constants

Named colors extracted from official brand guidelines:

```haskell
redAdobe       = "#f40f02"  -- PDF icon
redR           = "#1b61b1"  -- R programming
blueDM         = "#4185f4"  -- DeepMind
blueDS         = "#4d6bfe"  -- DeepSeek
blueFB         = "#1877f2"  -- Facebook/Meta
blueG          = "#4285f4"  -- Google
brownAnthropic = "#d4a27f"  -- Claude/Anthropic
greenNV        = "#77ba00"  -- Nvidia
orangeNGE      = "#f71a00"  -- Evangelion
purpleHaskell  = "#5e5086"  -- Haskell
yellowMoR      = "#ca9310"  -- HPMoR
yellowPG       = "#aa873b"  -- Project Gutenberg
```

---

## Key Patterns

### Organizational Overrides (Highest Priority)

Matches anywhere in URL to handle affiliations in Arxiv papers, anchors, etc:

```haskell
linkIconRulesOverrides u
  | u' u "deepmind"   = ("deepmind", "svg", blueDM)
  | u' u "deepseek"   = ("deepseek", "svg", blueDS)
  | u' u "facebook"   = ("facebook", "svg", blueFB)
  | u' u "#anthropic" = ("anthropic", "svg", brownAnthropic)
  | u' u "google"     = ("alphabet", "svg", blueG)
```

This ensures `arxiv.org/abs/1234#deepmind` or `arxiv.org/abs/1234?org=deepmind` gets the DeepMind icon, not Arxiv's.

### Domain Clustering

Related domains share icons:

```haskell
-- NGE fansites all get the Evangelion icon
aU' u ["onegeek.org", "eva-fan.com", "evaotaku.com", "khara.co.jp",
       "gainax.co.jp", "forum.evageeks.org", ...]
  = ("NGE", "text,tri", orangeNGE)

-- LessWrong ecosystem
aU'' u ["www.lesswrong.com", "sl4.org", "wiki.lesswrong.com",
        "www.greaterwrong.com"]
  = ("LW", "text", "#7faf83")
```

### Single-Character Icons

Unicode math/symbol characters for compact display:

| Pattern | Icon | Type | Notes |
|---------|------|------|-------|
| psyarxiv.com | ψ | text | Greek psi for psychology |
| arxiv.org | 𝛘 | text | Bold chi |
| haskell.org | 𝛌 | text | Bold lambda |
| unsongbook.com | ℵ | text | Alef (SSC novel) |
| meltingasphalt.com | ▲ | text | Kevin Simler |
| tinyletter.com | ✉ | text | Envelope |
| huggingface.co | 🤗 | text | Hugging face emoji |

### Quad Icons (2x2 Grid)

Four-character abbreviations rendered as tiny SVGs since Feb 2025:

```haskell
linkIconRulesQuad u
  | u'' u "jamanetwork.com" = ("JAMA", "text,sans,quad", "#d71635")
  | u'' u "www.nber.org"    = ("NBER", "text,quad", "#075dba")
  | u'' u "papers.ssrn.com" = ("SSRN", "text,quad", "#007398")
  | u'' u "www.pnas.org"    = ("PNAS", "text,quad", "#1f75b9")
```

### File Extension Detection

Extensions parsed and matched separately:

```haskell
linkIconRulesFiletypes u
  | iE u ["pdf", "PDF"]  = ("pdf", "svg", redAdobe)
  | iE u ["py"]          = ("code", "svg", "#2f6592")
  | iE u ["hs"]          = ("code", "svg", purpleHaskell)
  | iE u ["mp4", "avi"]  = ("file-video", "svg", "")
  | iE u ["epub"]        = ("EPUB", "text,quad,sans", "#87ba11")
```

### Color-Only Rules (Lowest Priority)

Some sites get hover colors without icons:

```haskell
linkIconRulesColors u
  | u'' u "danluu.com"              = ("", "", "#0000ee")  -- Default link blue!
  | u'' u "wellcomecollection.org"  = ("", "", "#ffce3c")  -- Yellow
  | u'' u "aeon.co"                 = ("", "", "#9d1d20")  -- Dark red
```

---

## Configuration

### Adding a New Domain

1. **Choose icon format** based on recognizability:
   - SVG: For well-known logos (GitHub, Twitter, Wikipedia)
   - Text: For abbreviations (NYT, BBC, HBR)
   - Quad: For 4-letter acronyms (PNAS, JAMA, IEEE)

2. **Find official brand color** from press kit or Wikipedia logo file

3. **Add to appropriate rule function**:
   ```haskell
   linkIconRulesDouble u
     ...
     | u'' u "example.com" = ("Ex", "text,sans", "#123456")
   ```

4. **Add test case(s)**:
   ```haskell
   linkIconTestUnitsText = [
     ...
     , ("https://example.com/article/123", "Ex", "text,sans", "#123456")
   ]
   ```

### Icon Naming Conventions

- SVG names match the icon concept: `"github"`, `"twitter"`, `"pdf"`
- Text icons use recognizable abbreviations: `"LW"`, `"SSC"`, `"NGE"`
- Single Unicode characters use the literal character: `"ψ"`, `"𝛘"`, `"∇"`

---

## Integration Points

### Parent Module (`LinkIcon.hs`)

Calls `linkIconRules` and applies results to Pandoc Link elements. Also handles:
- Icon suppression (`.icon-not` class)
- Duplicate detection (text matches icon)
- Archive URL unwrapping

### Test Suite (`Test.hs`)

Iterates `linkIconTestUnitsText`, comparing expected vs actual for each URL. Build fails on any mismatch.

### CSS Rendering (`links.css`)

- SVG icons load from `/static/img/icons/{name}.svg`
- Text icons styled with specified typography modifiers
- Quad icons rendered client-side as inline SVGs (since Feb 2025)
- Colors applied on hover

### Utils Module

Uses `Utils.isHostOrArchive` for archive-aware domain matching and `Utils.extension` for file type detection.

---

## See Also

- [LinkIcon.hs](/backend/link-icon-hs) - Parent module that applies these rules
- [build_icon_sprite_file.php](/php/build-icon-sprite-file) - Generates the SVG sprite sheet
- [links.css (CSS)](/css/links) - CSS rendering of icon attributes
- [links.css (Frontend)](/frontend/links-css) - Frontend documentation for link icon system
- [Utils.hs](/backend/utils-hs) - URL parsing utilities (isHostOrArchive, extension)
- [Typography.hs](/backend/typography-hs) - Calls LinkIcon during Pandoc transforms
- [Test.hs](/backend/test-hs) - Validates test cases at build time
