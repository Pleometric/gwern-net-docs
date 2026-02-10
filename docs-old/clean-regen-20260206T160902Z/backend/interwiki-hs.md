
# Interwiki.hs

**Path:** `build/Interwiki.hs` | **Language:** Haskell | **Lines:** ~221

> Expands shortcut links like `!W` to full Wikipedia URLs and manages WP namespace popup behavior

**Config:** `build/Config/Interwiki.hs` | **Lines:** ~6,000 (mostly redirect mappings)

---

## Overview

Interwiki.hs implements a link expansion system adapted from a Gitit plugin. It converts shorthand interwiki links (e.g., `[topic](!W)`) into full URLs (`https://en.wikipedia.org/wiki/Topic`). The module handles URL encoding edge cases, automatically strips possessive suffixes from anchor text, and bypasses Wikipedia redirects for consistency.

Beyond URL expansion, the module determines how Wikipedia links should behave in the popup system. Different WP namespaces (Article, Category, Special, File) have different popup capabilities—some support API-based annotations, some only work as live iframe embeds, and some (like `Special:`) cannot be embedded at all. The module tags links with CSS classes (`link-live`, `content-transform-not`) that control downstream popup behavior.

The module also provides utilities for checking Wikipedia article existence and disambiguation status via the WP API, used by link validation tooling.

---

## Public API

### `convertInterwikiLinks :: Pandoc -> Pandoc`

Document-level wrapper that walks the AST and expands all interwiki links. Provides document context for error reporting.

**Called by:** `hakyll.hs` (pandocTransform), `LinkMetadata.hs` (annotation parsing)
**Calls:** `convertInterwikiLinksInline`

---

### `convertInterwikiLinksInline :: Pandoc -> Inline -> Inline`

Core transformation. Given a `Link` inline, if the URL starts with `!`, looks up the prefix in `interwikiMap` and expands it.

```haskell
-- Input:  Link _ [Str "topic"] ("!W", "")
-- Output: Link _ [Str "topic"] ("https://en.wikipedia.org/wiki/Topic", "")
```

Also handles:
- Explicit article overrides via tooltip: `[display text](!W "Article_Name")`
- Automatic possessive stripping: `[George Washington's](!W)` → `George_Washington`
- Quotation mark stripping: `["The Article"](!W)` → `The_Article`
- WP redirect bypassing via `wpURLRedirectRewrites`
- Adding popup-control CSS classes via `wpPopupClasses`

**Called by:** `convertInterwikiLinks`
**Calls:** `wpPopupClasses`, `wpURLRewrites`, `wpURLRedirectRewrites`, `escapeWikiArticleTitle`

---

### `wpPopupClasses :: T.Text -> [T.Text]`

Returns CSS classes to control popup behavior based on URL/namespace:

| Namespace | Classes | Popup Behavior |
|-----------|---------|----------------|
| Article (default) | `["link-live"]` | API annotation + live embed |
| Category, File | `["content-transform-not", "link-live"]` | Live embed only |
| Special, `/w/index.php` | `["content-transform-not", "link-live-not"]` | No popup |
| Signpost articles | `["content-transform-not", "link-live"]` | Live embed only (broken HTML) |

**Called by:** `convertInterwikiLinksInline`
**Calls:** URI parsing utilities

---

### `isWPLive :: T.Text -> Bool`

Returns `True` if the URL can be embedded as a live link popup.

**Called by:** Frontend JavaScript (indirectly via class)

---

### `isWPAPI :: T.Text -> Bool`

Returns `True` if the URL can be annotated via the WP REST API.

**Called by:** `LinkMetadata.hs`

---

### `isWPDisambig :: T.Text -> IO (Maybe Bool)`

Queries the WP API to check if an article is a disambiguation page.

- `Nothing` → article doesn't exist
- `Just True` → disambiguation page
- `Just False` → regular article

```haskell
isWPDisambig "Python" -- Just True (disambiguation)
isWPDisambig "Python_(programming_language)" -- Just False
```

**Called by:** Link validation scripts

---

### `isWPArticle :: Bool -> T.Text -> IO Bool`

HEAD request to verify a WP URL exists (not 404). First argument controls verbose warnings.

**Called by:** Link validation scripts

---

### `toWikipediaEnURL :: T.Text -> T.Text`

Converts an article title to a full English Wikipedia URL.

```haskell
toWikipediaEnURL "Machine learning"
-- "https://en.wikipedia.org/wiki/Machine_learning"
```

---

### `escapeWikiArticleTitle :: T.Text -> T.Text`

URL-encodes an article title for Wikipedia, handling special cases:
- Spaces → underscores
- Unicode normalization (smart quotes → ASCII)
- Preserves `:`, `/`, `()`, `,`, `#`, `+`
- Strips `<span>` tags from smallcaps

---

## Internal Architecture

### Interwiki Map

```haskell
interwikiMap :: M.Map T.Text T.Text
interwikiMap = M.fromList $ wpInterwikiMap ++ customInterwikiMap

customInterwikiMap = [
  ("Hackage", "https://hackage.haskell.org/package/"),
  ("Hawiki",  "https://wiki.haskell.org/"),
  ("Hoogle",  "https://hoogle.haskell.org/?hoogle="),
  ("W",       "https://en.wikipedia.org/wiki/"),
  ("WP",      "https://en.wikipedia.org/wiki/")
]

wpInterwikiMap = [
  ("Wikipedia",  "https://en.wikipedia.org/wiki/"),
  ("Wikiquote",  "https://en.wikiquote.org/wiki/"),
  ("Wiktionary", "https://en.wiktionary.org/wiki/")
]
```

### Link Transformation Flow

```
Input: [George Washington's](!W)
                ↓
         T.head == '!' ?
                ↓ yes
         M.lookup "W" interwikiMap
                ↓
         wpURLRewrites "George Washington's"
                ↓
         Strip "'s" → "George Washington"
                ↓
         escapeWikiArticleTitle
                ↓
         "George_Washington"
                ↓
         wpURLRedirectRewrites (check redirectDB)
                ↓
         wpPopupClasses → ["link-live"]
                ↓
Output: Link (_, ["link-live"], _) _ ("https://en.wikipedia.org/wiki/George_Washington", "")
```

---

## Key Patterns

### Possessive/Quote Stripping

Automatically removes trailing `'s` and surrounding quotes from anchor text when deriving the URL, unless the title is in `quoteOverrides`:

```haskell
quoteOverrides = ["Antoine's", "Bloomingdale's", "Kinko's", ...]
```

This lets authors write natural prose like `[George Washington's](!W) first act` without manual overrides.

### Redirect Bypass

`Config/Interwiki.hs` contains ~5,800 redirect mappings to canonical URLs:

```haskell
redirectDB = [
  ("WP:RS", "Wikipedia:Reliable_sources"),
  ("Robots_exclusion_standard", "Robots.txt"),
  ("NLP", "Natural_language_processing"),
  ...
]
```

Benefits:
- Faster page loads (no server-side redirect)
- Consistent link IDs for LinkSuggester
- Cleaner link-checker reports

The redirect is applied via `fixedPoint` to handle redirect chains.

### Namespace Detection

Namespaces are detected by extracting the prefix before `:` in the article path:

```haskell
-- "Category:Buddhism" → "Category"
-- "Special:Random" → "Special"
-- "Regular_article" → "Regular_article" (not a namespace)
```

The colon is crucial—`SpecialPondicherry` is an article, `Special:Pondicherry` is a namespace.

---

## Configuration

### Config/Interwiki.hs Exports

| Export | Type | Purpose |
|--------|------|---------|
| `redirectDB` | `[(Text, Text)]` | ~5,800 WP redirect bypasses |
| `quoteOverrides` | `[Text]` | Titles where `'s` is part of the name |
| `testCases` | `[(Inline, Inline)]` | Input→expected output pairs |

### Adding New Interwikis

Edit `interwikiMap` in `Interwiki.hs`:

```haskell
customInterwikiMap = [
  ...
  ("NewSite", "https://example.com/wiki/"),
]
```

Then:
1. Add the link ID hash to the link ID blacklist
2. Check for a link-icon for the domain
3. Verify live-link behavior

### Adding Redirect Bypasses

Add to `redirectDB` in `Config/Interwiki.hs`:

```haskell
, ("Old_Title", "Canonical_Title")
```

The test suite validates:
- No circular redirects
- No duplicate keys
- Valid URL structure

---

## Integration Points

### Build Pipeline Position

```
Markdown → Pandoc AST
              ↓
         linkAuto (adds !W links to citations)
              ↓
    → convertInterwikiLinks ←
              ↓
         Typography transforms
              ↓
         LinkMetadata annotation lookup
```

Must run after `linkAuto` (which generates `!W` links) and before annotation lookup (which needs resolved URLs).

### CSS Classes Output

| Class | Meaning | Consumer |
|-------|---------|----------|
| `link-live` | Can embed as iframe | `popups.js` |
| `link-live-not` | Cannot embed | `popups.js` |
| `content-transform-not` | Skip API annotation | `extracts.js` |

### HTTP Requests

`isWPDisambig` and `isWPArticle` make live HTTP requests to Wikipedia:
- User-Agent: `gwern.net interwiki checker (+https://gwern.net/; mailto:gwern@gwern.net)`
- Accept: `application/json`

Used only in validation scripts, not during normal builds.

---

## Testing

```haskell
interwikiTestSuite :: [(Inline, Inline, Inline)]
-- Returns (input, actual, expected) for failures only

interwikiCycleTestSuite :: [(T.Text, T.Text)]
-- Returns cycle edges if redirectDB has cycles
```

Run via `Test.hs`. The test suite validates:
- All `testCases` transform correctly
- No circular redirects in `redirectDB`
- No duplicate redirect sources

---

## See Also

- [Config.Interwiki](/backend/config-interwiki-hs) - Wikipedia redirect database and test cases
- [LinkAuto.hs](/backend/link-auto-hs) - Generates `!W` links that this module expands
- [Config.LinkAuto](/backend/config-link-auto-hs) - Auto-linking patterns that produce interwiki links
- [Typography.hs](/backend/typography-hs) - Runs after interwiki expansion
- [hakyll.hs](/backend/hakyll-hs) - Build system that invokes interwiki processing
- [LinkMetadata.hs](/backend/link-metadata-hs) - Uses isWPAPI to decide annotation strategy
