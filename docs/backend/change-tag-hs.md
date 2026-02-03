
# changeTag.hs

**Path:** `build/changeTag.hs` | **Language:** Haskell (runghc script) | **Lines:** ~148

> CLI tool for batch add/remove operations on link tags across metadata databases

---

## Overview

changeTag.hs is a command-line utility for modifying the tag associations of annotated links in gwern.net's metadata system. Tags are hierarchical categories (like `economics/experience-curve` or `psychology/inner-monologue`) that organize the site's extensive link annotations.

The tool operates on the GTX metadata files (`me.gtx`, `full.gtx`, `half.gtx`, `auto.gtx`) which store link annotations. It handles the complexity of determining which file contains a given link's annotation and writes changes to the correct location. A key behavior is "promotion": when tagging a link that only exists in `auto.gtx` (auto-generated annotations), the tool moves it to `half.gtx` to mark it as human-curated.

The tool supports flexible argument parsing where links (identified by `/` or `http` prefixes) and tags can be intermixed in any order. It also provides a shortcut for bidirectional tag relationships—specifying two tags with no links will cross-link them via their index pages.

---

## Public API

This is a standalone CLI tool, not a library module.

### Command Line Interface

```bash
# Add a tag to a link
changeTag.hs "https://example.com/article" "economics/experience-curve"

# Remove a tag (hyphen prefix)
changeTag.hs "/doc/paper.pdf" "-psychology/deprecated-tag"

# Multiple links × multiple tags (cartesian product)
changeTag.hs "https://link1.com" "https://link2.com" "tag1" "tag2"

# Bidirectional tag relationship (two tags, no links)
changeTag.hs "ai/machine-learning" "cs/algorithms"

# Pipe URLs via stdin
echo "https://example.com" | changeTag.hs "psychology/cognition"
```

**Called by:** Manual invocation, potentially build scripts
**Calls:** `GTX.readGTXFast`, `GTX.writeGTX`, `LinkMetadata.annotateLink`, `Tags.guessTagFromShort`, `Tags.listTagsAll`

---

## Internal Architecture

### Data Flow

```
CLI args
    ↓
Parse into (links, tags)
    ↓
For each (link, tag) pair:
    ↓
Load all 4 GTX files
    ↓
Find which file contains the link
    ↓
Modify tag list in that file (or promote from auto→half)
    ↓
Write changed file(s)
```

### Key Data Structures

**MetadataList** (from LinkMetadataTypes):
```haskell
type MetadataList = [(String, MetadataItem)]
-- where MetadataItem is a 7-tuple:
-- (title, author, date, dateCreated, keyValues, tags, abstract)
```

### Core Functions

| Function | Purpose |
|----------|---------|
| `main` | Parse args, identify links vs tags, dispatch |
| `changeOneTag` | Process single link+tag pair |
| `changeAndWriteTags` | Determine correct GTX file, write update |
| `addNewLink` | Create annotation for unknown link, then retry |
| `changeTag` / `addTag` / `removeTag` | Pure list transformations |
| `mvItem` | Move entry between metadata lists (auto→half promotion) |

### GTX File Priority

The tool checks files in order: `me.gtx` → `full.gtx` → `half.gtx` → `auto.gtx`

- **me.gtx**: Personal/first-party annotations (highest priority)
- **full.gtx**: Complete hand-written annotations
- **half.gtx**: Partially curated annotations
- **auto.gtx**: Auto-generated annotations (lowest priority, disposable)

When modifying, only one file is written (the one containing the link), except for auto→half promotion which writes both.

---

## Key Patterns

### Tag Negation

Prefixing a tag with `-` removes it instead of adding:

```haskell
changeTag i ml tag = if head tag /= '-'
                     then addTag i ml tag
                     else removeTag i ml (tail tag)
```

### Auto-to-Half Promotion

Links in `auto.gtx` are considered ephemeral. Adding a tag implies human curation, so the entry is moved to `half.gtx`:

```haskell
if aP then
  let (autoNew,halfNew) = mvItem a p i
  in writeUpdatedGTX a "metadata/auto.gtx" autoNew >>
     writeUpdatedGTX a "metadata/half.gtx" (changeTag i halfNew t)
```

### Bidirectional Tag Links

When exactly two tags and zero links are provided, the tool creates mutual "see also" relationships:

```haskell
if length tags == 2 && null links then
  changeOneTag ("/doc/" ++ head tags ++ "/index") (tags !! 1) >>
  changeOneTag ("/doc/" ++ (tags !! 1) ++ "/index") (head tags)
```

### Tag Fuzzy Matching

Tags are resolved through `guessTagFromShort`, allowing abbreviated tag names:

```haskell
guessTagFromShort [] allTags $ filter (/=',') t
```

### Link Auto-Annotation

If a link has no existing annotation, the tool triggers `annotateLink` to create one (e.g., fetching metadata from Arxiv), then retries:

```haskell
addNewLink tag p = do
  md <- readLinkMetadata
  returnValue <- annotateLink md (Link nullAttr [] (T.pack p, T.pack ""))
  case returnValue of
    Right _ -> changeOneTag p tag  -- retry with new annotation
```

---

## Configuration

No external configuration. Paths are hardcoded:

| Path | Purpose |
|------|---------|
| `metadata/me.gtx` | Personal annotations |
| `metadata/full.gtx` | Complete annotations |
| `metadata/half.gtx` | Partial annotations |
| `metadata/auto.gtx` | Auto-generated annotations |
| `doc/` | Tag directory structure |

Uses `Config.Misc.root` for path normalization.

---

## Integration Points

### Input Sources
- Command line arguments
- stdin (piped URLs)

### Dependencies
- **GTX module**: Read/write metadata files
- **LinkMetadata module**: Annotation creation pipeline
- **Tags module**: Tag validation and fuzzy matching
- **Metadata.Format**: URL canonicalization

### Side Effects
- Modifies GTX files in `metadata/`
- May trigger network requests via `annotateLink` for new links

### Error Handling
- Validates tag existence (must have directory in `doc/`)
- Rejects malformed arguments (catches common typos like bare `gwt` or `t`)
- Distinguishes Temporary vs Permanent annotation failures

---

## See Also

- [Tags.hs](/backend/tags-hs) - Tag utilities and fuzzy matching via `guessTagFromShort`
- [Config.Tags](/backend/config-tags-hs) - Tag configuration with aliases and display names
- [guessTag.hs](/backend/guess-tag-hs) - CLI tool for testing tag expansion
- [GTX.hs](/backend/gtx-hs) - GTX file format and I/O operations
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database manager
- [Annotation.hs](/backend/annotation-hs) - URL-to-scraper dispatcher for new annotations
- [Hakyll.hs](/backend/hakyll-hs) - Site generator consuming tag data
