
# Query.hs

**Path:** `build/Query.hs` | **Language:** Haskell | **Lines:** ~141

Utility module for extracting links, images, and URLs from Pandoc AST documents.

---

## Overview

Query.hs provides a collection of functions for querying Pandoc abstract syntax trees to extract structural elements - primarily links (URLs, anchor text, tooltips), images, and link identifiers. It serves as the foundation for any build tool that needs to analyze document content.

The module builds on Pandoc's `queryWith` and `query` functions from `Text.Pandoc.Walk`, which traverse the AST and accumulate results from pattern-matching functions. Most functions follow a simple pattern: walk the document, match on `Link` or `Image` inlines, and extract the relevant tuple of data.

A key design decision is that all link extraction first runs through `convertInterwikiLinks` (from the Interwiki module), normalizing shorthand wiki-style links to full URLs before extraction. The module also filters out "inflation URLs" (links starting with `$` or `₿`) which are handled specially elsewhere.

---

## Public API

### `parseMarkdownOrHTML :: Bool -> T.Text -> Pandoc`

Parse text as either Markdown (True) or HTML (False) into a Pandoc AST.

```haskell
parseMarkdownOrHTML True  "# Hello"        -- Markdown
parseMarkdownOrHTML False "<h1>Hello</h1>" -- HTML
```

**Called by:** `link-titler.hs`, `link-tooltip.hs`, `link-suggester.hs`, `generateBacklinks.hs`
**Calls:** `Text.Pandoc.readMarkdown`, `Text.Pandoc.readHtml`

---

### `extractLinks :: Bool -> T.Text -> [T.Text]`

Parse text and return all URLs. Convenience wrapper around `extractLinksWith (const True)`.

**Called by:** `link-extractor.hs`, `GenerateSimilar.hs`
**Calls:** `extractLinksWith`, `parseMarkdownOrHTML`

---

### `extractLinksWith :: (Inline -> Bool) -> Bool -> T.Text -> [T.Text]`

Parse text and return URLs matching a filter predicate. The boolean selects Markdown (True) or HTML (False) parsing.

```haskell
-- Extract only external links
extractLinksWith (\(Link _ _ (url,_)) -> "http" `T.isPrefixOf` url) True doc
```

**Called by:** Various link processing tools
**Calls:** `parseMarkdownOrHTML`, `extractURLsWith`

---

### `extractURLs :: Pandoc -> [T.Text]`

Extract all URLs from an already-parsed Pandoc AST. Note: may return duplicates.

**Called by:** `Metadata/Author.hs`, `preprocess-markdown.hs`, `Paragraph.hs`, `LinkAuto.hs`
**Calls:** `extractURLsWith`, `convertInterwikiLinks`

---

### `extractURLsWith :: (Inline -> Bool) -> Pandoc -> [T.Text]`

Extract URLs from Pandoc AST with a filter predicate on Link inlines.

**Called by:** `extractURLs`, `extractLinksWith`
**Calls:** `queryWith`, `extractURLWith`, `convertInterwikiLinks`

---

### `extractURLWith :: (Inline -> Bool) -> Inline -> [(T.Text, T.Text, T.Text)]`

Core extraction function. Given a filter predicate and an Inline, returns a list of `(url, anchorText, tooltip)` tuples.

```haskell
extractURLWith (const True) (Link _ [Str "GPT-3"] ("/papers/gpt3.pdf", "Language Models"))
-- → [("/papers/gpt3.pdf", "GPT-3", "Language Models")]
```

Errors on empty URLs. Skips inflation URLs (`$`, `₿`).

**Called by:** `extractURLsWith`
**Calls:** `isInflationURL`, `inlinesToText`

---

### `extractURL :: Inline -> [(T.Text, T.Text, T.Text)]`

Convenience wrapper: `extractURLWith (const True)`.

**Called by:** `generateBacklinks.hs`

---

### `extractURLsAndAnchorTooltips :: Pandoc -> [(T.Text, [T.Text])]`

Extract URLs paired with their anchor text AND tooltips (when both exist). Returns `(url, [anchorText])` or `(url, [anchorText, tooltip])`.

This is useful for similar-links suggestions where both the visible link text and the title might be valid search targets (e.g., "GPT-3" vs "Language Models are Few-Shot Learners").

```haskell
extractURLsAndAnchorTooltips doc
-- → [("/papers/gpt3.pdf", ["GPT-3", "Language Models are Few-Shot Learners"])]
```

Includes cleanup for anchor text that gets malformed by nested spans (removes `>...<` wrappers).

**Called by:** `link-titler.hs`, `GenerateSimilar.hs`, `link-suggester.hs`
**Calls:** `queryWith`, `convertInterwikiLinks`, `inlinesToText`

---

### `extractLinkIDsWith :: (Inline -> Bool) -> T.Text -> Pandoc -> [(T.Text, T.Text)]`

Extract URL and link ID pairs. The ID is the `id` attribute from the Link element's attributes tuple.

```haskell
-- Link: <a id="gpt3-link" href="/papers/gpt3.pdf">GPT-3</a>
extractLinkIDsWith (const True) "" doc
-- → [("/papers/gpt3.pdf", "gpt3-link")]
```

**Called by:** `generateLinkBibliography.hs`, `generateBacklinks.hs`
**Calls:** `queryWith`, `convertInterwikiLinks`

---

### `extractImages :: Pandoc -> [Inline]`

Extract all Image inlines from a document. Returns the full `Image` Inline elements, not just URLs.

Note: Does not distinguish between inline images and figure images (Images in their own Paragraph). In practice, most images on gwern.net are figures.

```haskell
extractImages (Pandoc nullMeta [Para [Image ("",["fig"],[]) [Str "Cat"] ("/cat.jpg","")]])
-- → [Image ("",["fig"],[]) [Str "Cat"] ("/cat.jpg","")]
```

**Called by:** `generateDirectory.hs`

---

### `extractLinksInlines :: Pandoc -> [Inline]`

Extract all Link inlines from a document. Returns full `Link` Inline elements.

**Called by:** `LinkMetadata.hs`

---

### `truncateTOCHTML :: T.Text -> T.Text -> [Block]`

Parse an HTML table of contents and return only the list items containing a specific anchor ID.

Used to extract a relevant subsection of a TOC for display in annotations.

```haskell
truncateTOCHTML "appendix" tocHtml
-- Returns the Appendix list item and its children (Fuzz Testing, Toys)
```

**Called by:** (Commented out in `Annotation/Gwernnet.hs`)
**Calls:** `parseMarkdownOrHTML`, `filterListBlocksContainingAnchor`, `idsRemove`

---

## Internal Architecture

### Core Query Pattern

All extraction functions follow this structure:

```
Input (Pandoc or Text)
    ↓
convertInterwikiLinks     -- Normalize !Wikipedia etc to full URLs
    ↓
queryWith extractFn       -- Walk AST, accumulate matches
    ↓
Output (list of tuples)
```

### Data Flow

```
Text Input
    ↓
parseMarkdownOrHTML ──→ Pandoc AST
    ↓
extractLinksWith/extractURLsWith
    ↓
convertInterwikiLinks (Interwiki.hs)
    ↓
queryWith (from Text.Pandoc.Walk)
    ↓
extractURLWith (pattern match on Link)
    ↓
[(url, anchorText, tooltip)]
```

### Key Data Structures

**Link Inline:** `Link (id, classes, attrs) [anchorInlines] (url, tooltip)`
- First tuple: element attributes (id string, class list, key-value attrs)
- Second element: anchor text as list of Inlines
- Third element: tuple of URL and tooltip/title

**Image Inline:** `Image (id, classes, attrs) [altInlines] (src, title)`
- Same structure as Link

---

## Key Patterns

### Filtered Extraction

The `*With` variants take a predicate function to filter which links are extracted:

```haskell
extractURLsWith :: (Inline -> Bool) -> Pandoc -> [T.Text]
extractURLsWith rule = queryWith (map (\(url,_,_) -> url) . extractURLWith rule)
                     . convertInterwikiLinks
```

This allows callers to extract only external links, only local links, only links with certain classes, etc.

### Inflation URL Filtering

Links starting with `$` or `₿` are inflation-adjusted display values, not real URLs:

```haskell
extractURLWith rule x@(Link _ anchorText (url, tooltip))
    | url == "" || isInflationURL url = []  -- Skip these
    | rule x = [(url, inlinesToText anchorText, tooltip)]
```

### TOC Filtering with List Queries

The `truncateTOCHTML` function demonstrates a more complex query pattern - finding list items that contain a specific anchor:

```haskell
filterListBlocksContainingAnchor :: T.Text -> [Block] -> [Block]
filterListBlocksContainingAnchor i blocks = concat $ query (listContainsAnchor i) blocks

listContainsAnchor :: T.Text -> Block -> [Block](Block)
listContainsAnchor i (BulletList lists) = map (containsAnchor i) lists
listContainsAnchor _ _ = []
```

---

## Configuration

No external configuration. Behavior is determined entirely by function parameters.

---

## Integration Points

### Imports From
- `Inflation.isInflationURL` - Filter out `$`/`₿` price links
- `Interwiki.convertInterwikiLinks` - Expand wiki shortcuts to full URLs
- `Utils.inlinesToText` - Convert Inline list to plain text

### Used By
| Module | Functions Used |
|--------|---------------|
| `link-titler.hs` | `extractURLsAndAnchorTooltips`, `parseMarkdownOrHTML` |
| `link-tooltip.hs` | `parseMarkdownOrHTML` |
| `link-suggester.hs` | `extractURLsAndAnchorTooltips`, `parseMarkdownOrHTML` |
| `link-extractor.hs` | `extractLinks` |
| `GenerateSimilar.hs` | `extractURLsAndAnchorTooltips`, `extractLinks` |
| `generateBacklinks.hs` | `extractLinkIDsWith`, `parseMarkdownOrHTML`, `extractURL` |
| `generateLinkBibliography.hs` | `extractLinkIDsWith` |
| `generateDirectory.hs` | `extractImages` |
| `LinkMetadata.hs` | `extractLinksInlines` |
| `LinkAuto.hs` | `extractURLs` |
| `Metadata/Author.hs` | `extractURLs` |
| `Paragraph.hs` | `extractURLs` |
| `preprocess-markdown.hs` | `extractURLs` |

### External Dependencies
- `Text.Pandoc` - AST types and parsing
- `Text.Pandoc.Walk` - `query`, `walk`, `queryWith`

---

## See Also

- [Utils.hs](/backend/utils-hs) - Core utility module providing `inlinesToText` and Pandoc helpers
- [Hakyll.hs](/backend/hakyll-hs) - Site generator that uses Query for link extraction
- [Interwiki.hs](/backend/interwiki-hs) - Interwiki link expansion (called before extraction)
- [LinkMetadata.hs](/backend/link-metadata-hs) - Uses extracted links for metadata lookup
- [Typography.hs](/backend/typography-hs) - Another Pandoc AST manipulation module
- [Paragraph.hs](/backend/paragraph-hs) - Uses `extractURLs` for link processing
