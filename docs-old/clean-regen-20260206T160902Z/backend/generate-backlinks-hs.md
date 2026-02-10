
# generateBacklinks.hs

**Path:** `build/generateBacklinks.hs` | **Language:** Haskell | **Lines:** ~215

> Reverse citation generator: builds backlinks database and HTML snippets

**Helper module:** `build/LinkBacklink.hs` | **Lines:** ~130
**Config:** `build/Config/Misc.hs` (partial)

---

## Overview

generateBacklinks.hs computes reverse citations across the entire gwern.net corpus. For every URL that appears in the site, it records which pages link to it, then writes HTML snippet files that can be displayed as popups. This inverts the normal direction of hyperlinks: instead of "page A links to B", backlinks answer "what links to B?"

The system operates in two phases: first it parses all Markdown/HTML files and annotation abstracts to extract (target → caller) pairs, storing them in `metadata/backlinks.hs`. Then it generates one HTML file per target URL in `metadata/annotation/backlink/`, containing a formatted list of all callers with context transclusions.

Key design decisions:
- **Anchor-aware deduplication**: Links to `/page` and `/page#section` are grouped under the base page, but the anchor-specific callers are preserved separately for granular navigation
- **Author bibliography integration**: When a backlink comes from an annotation's author field (not the abstract), it's categorized separately as "authored by" rather than "mentioned in"
- **Context transclusion**: Non-author backlinks include a transclusion directive that loads surrounding context; author backlinks omit it because the "context" would just be the author field
- **Incremental updates**: The database persists between runs; new files are appended via stdin, merged with existing entries

---

## Public API

### `main :: IO ()`

Entry point. Reads file list from stdin, updates backlinks database, writes HTML snippets.

```bash
# Typical invocation from sync.sh:
find . -name "*.md" -o -name "*.html" | runghc generateBacklinks.hs
```

**Called by:** `sync.sh` (build orchestrator)
**Calls:** `main'`, `readBacklinksDB`, `writeBacklinksDB`, `readLinkMetadata`

### `parseFileForLinks :: Bool -> FilePath -> IO [(T.Text, T.Text)]`

Parses a single file, returning (target URL, caller URL) pairs. The `Bool` indicates Markdown (True) vs HTML (False).

```haskell
parseFileForLinks True "gpt-3.md"
-- → [("/scaling", "/gpt-3"), ("https://arxiv.org/...", "/gpt-3"), ...]
```

Respects `backlink: False` YAML frontmatter to skip index/newsletter pages.

**Called by:** `main'`
**Calls:** `parseMarkdownOrHTML`, `extractLinkIDsWith`, `backlinkBlackList`

### `parseAnnotationForLinks :: T.Text -> MetadataItem -> [(T.Text, T.Text)]`

Extracts links from annotation abstracts and author fields.

**Called by:** `main'`
**Calls:** `authorsLinkifyAndExtractURLs`, `parseMarkdownOrHTML`, `extractLinkIDsWith`

### `writeOutCallers :: Metadata -> T.Text -> [(T.Text, [T.Text])] -> IO ()`

Generates the HTML snippet for a single target URL, writing to `metadata/annotation/backlink/{urlencoded}.html`.

**Called by:** `main'`
**Calls:** `generateCaller`, `linkAutoFiltered`, `hasAnnotation`, `writeUpdatedFile`

---

## Internal Architecture

### Data Flow

```
┌─────────────────────────────────────────────────────────────────────────┐
│ Input Sources                                                            │
├─────────────────────────────────────────────────────────────────────────┤
│  *.md files ──────────────→ parseFileForLinks True  ──┐                  │
│  *.html files ────────────→ parseFileForLinks False ──┼──→ (target,caller)│
│  metadata.hs annotations ─→ parseAnnotationForLinks ──┘       pairs      │
└─────────────────────────────────────────────────────────────────────────┘
                                        │
                                        ▼
┌─────────────────────────────────────────────────────────────────────────┐
│ Database (metadata/backlinks.hs)                                         │
├─────────────────────────────────────────────────────────────────────────┤
│  Map Target [(AnchoredTarget, [Callers])]                                │
│                                                                          │
│  e.g. "/improvement" →                                                   │
│         [ ("/improvement#microsoft", ["/note/note", "/review/book"])     │
│         , ("/improvement", ["/index"]) ]                                 │
└─────────────────────────────────────────────────────────────────────────┘
                                        │
                                        ▼
┌─────────────────────────────────────────────────────────────────────────┐
│ Output (metadata/annotation/backlink/*.html)                             │
├─────────────────────────────────────────────────────────────────────────┤
│  <p><strong>Backlinks (N):</strong>...</p>                               │
│  <ul>                                                                    │
│    <li><a class="backlink-not id-not link-page">page-title</a>           │
│        (<a>full context</a>):                                            │
│        <blockquote class="include-block-context-expanded">               │
│          [backlink context]                                              │
│        </blockquote>                                                     │
│    </li>                                                                 │
│    ...                                                                   │
│  </ul>                                                                   │
└─────────────────────────────────────────────────────────────────────────┘
```

### Backlinks Type

```haskell
-- From LinkBacklink.hs:
type Backlinks = M.Map T.Text [(T.Text, [T.Text])]
--               ^base URL    ^anchored URL, callers

-- Example structure:
-- "/gpt-3" →
--   [ ("/gpt-3",          ["/scaling", "/index"])           -- whole-page links
--   , ("/gpt-3#prompts",  ["/prompt-engineering"])          -- section links
--   ]
```

The nested structure preserves anchor granularity while grouping under base URLs.

### Caller Generation Pipeline

```haskell
generateCaller md target (caller, callers) =
    -- 1. Look up metadata for target to generate anchor ID
    --    (so popup jumps to the specific link usage)
    selfIdent ← generateID md caller author date

    -- 2. Partition callers into authored vs cited
    callerDatesTitlesAuthored    ← filterIfAuthored md caller ...
    callerDatesTitlesAuthoredNot ← callers \\ authored

    -- 3. Generate sublists (authored items first, then citations)
    --    Each item has:
    --    - Link with .backlink-not .id-not (prevents recursive popups)
    --    - Title (from metadata or fallback to path)
    --    - "full context" link (for page targets)
    --    - BlockQuote with .include-block-context-expanded (transclusion)
```

### Output HTML Structure

```html
<!-- metadata/annotation/backlink/%2Fgpt-3.html -->
<p>
  <a class="icon-special" href="/design#backlink">
    <strong>Backlinks (42):</strong>
  </a>
</p>
<ul>
  <li>
    <p class="backlink-source">
      <a class="backlink-not id-not link-page" href="/scaling">
        <em>scaling</em>
      </a>
      (<a class="link-annotated-not" href="/scaling#gwern-gpt-3">full context</a>):
    </p>
    <blockquote class="backlink-context">
      <a class="backlink-not include-block-context-expanded collapsible link-annotated-not"
         data-target-id="gwern-gpt-3"
         href="/scaling">
        [backlink context]
      </a>
    </blockquote>
  </li>
  ...
</ul>
```

---

## Key Patterns

### Backlink-Not Class Cascade

Links within backlink snippets must not themselves trigger backlinks or popups:

```haskell
-- In generateCallerSublist:
hasAnnotationOrIDInline md $ Link ("", "backlink-not":"id-not":classes, []) ...
```

- `.backlink-not` — excluded from backlink database extraction
- `.id-not` — no ID generation (prevents duplicate anchors)
- Combined prevents infinite recursion when backlink pages link to each other

### Author Bibliography Detection

When an annotation's `author` field links to a person page, that's a "written by" relationship, not a citation:

```haskell
filterIfAuthored md caller titleCallers = filter isAuthored titleCallers
  where isAuthored (_,_,url) =
          case M.lookup url md of
            Nothing → False
            Just (_,authors,_,_,_,_,_) →
              caller `elem` extractedAuthorURLs authors
```

Author backlinks appear first in the list, without context transclusion (since the "context" would just be the author field).

### Context Transclusion Links

The `[backlink context]` placeholder triggers the transclusion system:

```haskell
Link ("",
      ["backlink-not",
       "include-block-context-expanded",  -- triggers block transclusion
       "collapsible"] ++ pageClasses,
      if selfIdent=="" then []
      else [("target-id", selfIdent)]  -- jump to specific link usage
     )
     [Str "[backlink context]"]
     (callerURL, "")
```

The frontend's `transclude.js` sees `include-block-context-expanded` and replaces the link with the actual surrounding paragraph from the caller page.

### Clean-Start Double-Run Workaround

```haskell
main = do
  priorBacklinksN ← listDirectory "metadata/annotation/backlink/"
  if priorBacklinksN > 0
    then main'
    else main' >> main'  -- Run twice on empty start
```

When starting fresh, some ordering dependency requires running twice. This is acknowledged as a hack ("for uninteresting reasons probably due to a bad architecture").

---

## Configuration

### Config.Misc.hs

#### `backlinkBlackList :: T.Text -> Bool`

URLs matching these patterns are excluded from backlink tracking:

```haskell
backlinkBlackList url
  | anyInfixT url ["/backlink/", "/link-bibliography/", "/similar/", "/private"] = True
  | anyPrefixT url ["$", "#", "!", "mailto:", "irc://", "₿", "/doc/www/", "https://example.com"] = True
  | otherwise = False
```

#### `sectionizeWhiteList :: [T.Text]`

Anchors that shouldn't be suggested for splitting into standalone pages:

```haskell
sectionizeWhiteList = ["/danbooru2021#danbooru2018", ...]
```

#### `sectionizeMinN :: Int`

Minimum backlink count before suggesting an anchor become its own page (default: 3).

### Per-Page Control

YAML frontmatter can disable backlink generation:

```yaml
---
backlink: False
---
```

Used for index pages, newsletters, and other link-heavy aggregation pages.

---

## Integration Points

### Database Files

| File | Format | Purpose |
|------|--------|---------|
| `metadata/backlinks.hs` | Haskell literal `[(Text, [(Text, [Text])])]` | Persistent backlinks database |
| `metadata/annotation/backlink/*.html` | HTML fragments | Per-URL popup content |

### LinkBacklink.hs Helper Functions

| Function | Purpose |
|----------|---------|
| `readBacklinksDB` | Parse `backlinks.hs` into `Map` |
| `writeBacklinksDB` | Serialize `Map` back to file |
| `getBackLink url` | Get HTML snippet path for URL |
| `getBackLinkCount url` | Count backlinks by grepping snippet |
| `getForwardLinks bdb url` | Invert: what does URL link to? |
| `suggestAnchorsToSplitOut` | Find popular anchors to refactor |

### Frontend Integration

The HTML snippets are fetched by `content.js` and displayed in popups:

```javascript
// content.js processes backlink snippets:
if (auxLinksLinkType == "backlinks") {
    auxLinksList.querySelectorAll("blockquote").forEach(bq => {
        bq.classList.add("backlink-context");
    });
    // Set data-backlink-target-url for context navigation
}
```

### Build System

```bash
# sync.sh calls:
find . -name "*.md" -o -name "*.html" | runghc generateBacklinks.hs

# Validation checks:
BACKLINKS_N=$(cat ./metadata/backlinks.hs | wc --lines)
[ "$BACKLINKS_N" -le 180000 ] && echo "$BACKLINKS_N"

BACKLINKS_FILES_N=$(find ./metadata/annotation/backlink/ -type f | wc --lines)
[ "$BACKLINKS_FILES_N" -le 28500 ] && echo "$BACKLINKS_FILES_N"
```

---

## Common Operations

### Regenerate All Backlinks

```bash
rm -rf metadata/annotation/backlink/
rm metadata/backlinks.hs
find . -name "*.md" -o -name "*.html" | runghc build/generateBacklinks.hs
# Run twice for clean start
find . -name "*.md" -o -name "*.html" | runghc build/generateBacklinks.hs
```

### Find Popular Anchors to Split Out

```haskell
-- In GHCi:
:l build/LinkBacklink.hs
suggestAnchorsToSplitOut
-- → [(15, "/gpt-3#prompt-engineering"), (12, "/spaced-repetition#anki"), ...]
```

### Check Backlinks for a URL

```haskell
:l build/LinkBacklink.hs
bdb ← readBacklinksDB
M.lookup "/gpt-3" bdb
-- → Just [("/gpt-3", ["/scaling", "/index"]), ("/gpt-3#prompts", ["/prompt-engineering"])]
```

### Get Forward Links (What Does X Link To?)

```haskell
bdb ← readBacklinksDB
getForwardLinks bdb "/scaling"
-- → ["/gpt-3", "/transformer", "/chinchilla", ...]
```

---

## Failure Modes

### "file does not exist" errors during validation

The backlinks database references a page that was deleted. Clean the database and regenerate, or manually edit `metadata/backlinks.hs`.

### Empty backlinks database after clean build

Must run `generateBacklinks.hs` twice on a clean start due to ordering dependency.

### Infinite/very large snippet files

Usually caused by missing `.backlink-not` class on internal links. Check for recursive transclusion.

### "empty target" error

```
generateBacklinks.writeOutCallers: empty target! This should never happen?
```

A link was extracted with an empty URL. Debug by checking recent file changes.

---

## See Also

- [LinkBacklink.hs](/backend/link-backlink-hs) - Helper module for reading/writing backlinks database
- [hakyll.hs](/backend/hakyll-hs) - Build system that integrates backlinks into page generation
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database providing author/title metadata for backlinks
- [sync.sh](/backend/sync-sh) - Build orchestrator that invokes generateBacklinks.hs
- [content.js](/frontend/content-js) - Frontend handling of backlink popup content
- [transclude.js](/frontend/transclude-js) - Implements include-block-context-expanded transclusion for backlinks
