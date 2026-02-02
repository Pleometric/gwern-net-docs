
# Typography.hs

**Path:** `build/Typography.hs` | **Language:** Haskell | **Lines:** ~440

Pandoc AST transforms for text typography: citations, date ranges, rulers, title case.

---

## Overview

Typography.hs applies typographic transformations to Pandoc documents during the gwern.net build process. It walks the Pandoc AST to rewrite text elements - formatting citations like "Smith 2020" with semantic spans, annotating date ranges with duration subscripts, cycling horizontal ruler styles, and applying title case.

The module distinguishes between "permanent" transforms (safe to cache in the metadata database) and "temporary" transforms (time-sensitive, regenerated on each build). The main example of temporary transforms is date duration subscripts - "1984" becomes "1984<sub>40ya</sub>" showing years ago, which changes annually.

The implementation follows Pandoc's standard pattern: use `walk` or `walkM` to traverse the AST, pattern-match on specific `Inline` or `Block` constructors, and return transformed elements. State monads track things like ruler count for cycling.

---

## Public API

### typographyTransformTemporary :: Pandoc -> Pandoc

Main entry point for full typography processing. Applies all transforms including time-sensitive date annotations.

**Called by:** `hakyll.hs`, `LinkMetadata.hs`, `Annotation.hs`

### typographyTransformPermanent :: Pandoc -> Pandoc

Subset of transforms safe for permanent storage. Excludes date duration annotations since they change yearly.

```haskell
typographyTransformPermanent =
    walk imageCaptionLinebreak .
    parseRawAllClean .
    walk (linkLive . linkIcon) .
    walk mergeSpaces .
    linebreakingTransform .
    rulersCycle C.cycleCount .
    walk (citefyInline year) .
    walk imageCaptionLinebreak .
    walk mergeSpaces .
    parseRawAllClean
```

### typesetHtmlField :: String -> String

Typeset an HTML string for display. Parses HTML to Pandoc, applies transforms, renders back to HTML.

**Called by:** `Annotation.hs`, `XOfTheDay.hs`

### typesetHtmlFieldPermanent :: Bool -> String -> String

Like `typesetHtmlField` but with explicit permanent/temporary flag.

### linebreakingTransform :: Pandoc -> Pandoc

Adds `<wbr>` (zero-width space) hints for better browser line-breaking at slashes.

**Note:** Currently disabled (`breakSlashes = id`, `breakEquals = id`).

### titlecase' :: String -> String

Convert string to title case. Wraps the `titlecase` library with fixes for hyphens and apostrophes.

```haskell
titlecase' "end-to-end testing"
-- → "End-To-End Testing"

titlecase' "Foo's bar"
-- → "Foo's Bar"
```

**Called by:** `Annotation.hs`, `Blog.hs`, `LinkMetadata.hs`

### identUniquefy :: Pandoc -> Pandoc

De-duplicate link IDs by appending `-2`, `-3`, etc. For auto-generated pages where duplicate annotations appear.

**Called by:** `generateLinkBibliography.hs`, `generateBacklinks.hs`

### mergeSpaces :: [Inline] -> [Inline]

Consolidates fragmented Pandoc strings. Pandoc often produces `[Str "A", Space, Str "B"]` which breaks pattern matching; this merges them into `[Str "A B"]`.

### completionProgressInline :: String -> Inline

Convert completion status strings ("finished", "in progress", "draft") to annotated spans with percentage data attributes.

```haskell
completionProgressInline "finished"
-- → Span ("", ["completion-status"], [("progress-percentage", "100")]) [Str "finished"]
```

---

## Internal Architecture

### Transform Pipeline

The typography pipeline applies transforms in a specific order:

```
Input Pandoc
    ↓
parseRawAllClean          -- Clean anonymous spans/divs
    ↓
mergeSpaces               -- Consolidate fragmented strings
    ↓
imageCaptionLinebreak     -- Add linebreaks in figure captions
    ↓
citefyInline              -- Format citations with spans
    ↓
rulersCycle               -- Number horizontal rulers 1-3
    ↓
linebreakingTransform     -- Add <wbr> at slashes (disabled)
    ↓
mergeSpaces               -- Clean up again
    ↓
linkIcon / linkLive       -- Add link decorations
    ↓
parseRawAllClean          -- Final cleanup
    ↓
dateRangeDuration         -- [Temporary only] Add duration subscripts
```

### Citation Formatting (citefyInline)

Matches citation patterns and wraps them in semantic spans:

- Single author: "Smith 2020" → `<span class="cite"><span class="cite-author">Smith</span><span class="cite-date">2020</span></span>`
- Double author: "Smith & Jones 2020" → same structure
- Et al: "Smith et al 2020" → uses `cite-author-plural` class, hides "et al" in title attribute

Uses three regexes for the three patterns, with Unicode-aware lowercase character class for international surnames.

### Ruler Cycling (rulersCycle)

Uses `State` monad to count `HorizontalRule` elements and wrap each in a div with cycling class:

```haskell
rulersCycle modulus doc = evalState (walkM addHrNth doc) 0
 where addHrNth HorizontalRule = do
         modify (+1)
         count <- get
         let nth = (count - 1) `mod` modulus + 1
         return $ Div ("", ["horizontal-rule-nth-" ++ show nth], []) [HorizontalRule]
```

With `cycleCount = 3`, rulers cycle through classes `horizontal-rule-nth-1`, `horizontal-rule-nth-2`, `horizontal-rule-nth-3`.

---

## Key Patterns

### Pandoc AST Walking

The standard pattern for transforms:

```haskell
-- For stateless transforms: use walk
myTransform :: Pandoc -> Pandoc
myTransform = walk transformInline
  where transformInline :: Inline -> Inline
        transformInline (Str s) = ...  -- transform strings
        transformInline x = x          -- pass through unchanged

-- For stateful transforms: use walkM with State monad
myStatefulTransform :: Pandoc -> Pandoc
myStatefulTransform doc = evalState (walkM transform doc) initialState
```

### Type-Safe Inline Expansion

When a transform needs to return multiple `Inline` elements from a single one, wrap in an anonymous `Span`:

```haskell
citefyInline year x@(Str s) =
    let rewrite = go s
    in if [Str s] == rewrite then x
       else Span nullAttr rewrite  -- wrap multiple Inlines
```

The `parseRawAllClean` pass later removes these wrapper spans.

### Adding a New Typography Transform

1. Write a function `myTransform :: Inline -> Inline` (or `Block -> Block`)
2. Add it to the pipeline in `typographyTransformPermanent` or `typographyTransformTemporary`:
   ```haskell
   typographyTransformPermanent =
       walk myTransform .  -- insert here
       walk imageCaptionLinebreak .
       ...
   ```
3. If stateful, use `walkM` pattern with `State` monad
4. Add test cases to `Config/Typography.hs`

---

## Configuration

**Path:** `Config/Typography.hs`

| Setting | Default | Purpose |
|---------|---------|---------|
| `cycleCount` | 3 | Number of ruler style variants |
| `titleCaseTestCases` | 77 cases | Test cases for title case |
| `surnameFalsePositivesWhiteList` | ~100 words | Words that look like "Surname YYYY" but aren't citations (months, places) |
| `minRange` | 4 | Minimum years for date range annotation |
| `minDuration` | 11 | Minimum years ago for duration subscript |
| `minDateFirst` | 1501 | Earliest year to recognize as date |
| `maxDateSecond` | 2100 | Latest year to recognize as date |

---

## Integration Points

### Reads From
- `Config.Typography` - configuration constants
- `Config.Misc.currentYear` - current year for date calculations

### Called By
- `hakyll.hs` - main build pipeline
- `LinkMetadata.hs` - metadata processing
- `Annotation.hs` - annotation formatting
- `Blog.hs` - blog post processing
- `generateLinkBibliography.hs` - bibliography generation
- `generateBacklinks.hs` - backlink pages

### Calls
- `LinkIcon.linkIcon` - adds link type icons
- `LinkLive.linkLive` - adds live preview popups
- `Metadata.Date.dateRangeDuration` - date range formatting (actually defined in Date.hs, re-exported)
- `Utils.parseRawAllClean` - AST cleanup

### External Dependencies
- `Data.Text.Titlecase.titlecase` - title case library
- `Text.Regex.TDFA` - regex matching

---

## See Also

- [Config.Typography](/backend/config-typography-hs) - Configuration constants, test cases, and whitelists for typography transforms
- [typography.js](/frontend/typography-js) - Client-side typography processing (smart quotes, dashes, ellipses)
- [hakyll.hs](/backend/hakyll-hs) - Main build pipeline that calls typography transforms
- [preprocess-markdown.hs](/backend/preprocess-markdown-hs) - Markdown preprocessing before Pandoc parsing
- [Inflation.hs](/backend/inflation-hs) - Related Pandoc AST transforms for dollar amount adjustments
- [Columns.hs](/backend/columns-hs) - Other Pandoc AST transformations for list layout
- [LinkIcon.hs](/backend/link-icon-hs) - Link icon annotation called during typography pipeline
- [LinkLive.hs](/backend/link-live-hs) - Live preview popups called during typography pipeline
