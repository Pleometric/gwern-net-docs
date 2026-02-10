
# GenerateSimilar.hs

**Path:** `build/GenerateSimilar.hs` | **Language:** Haskell | **Lines:** ~680

> Embedding-based semantic similarity system for generating "Similar Links" recommendations

---

## Overview

GenerateSimilar implements gwern.net's "Similar Links" feature, which uses OpenAI embeddings to find semantically related pages. When you see the "Similar Links" tab on an annotation popup, this module generated those recommendations.

The system works in three phases: (1) document formatting—converting metadata items into embedding-friendly plaintext, (2) embedding generation—calling the OpenAI API via a shell script, and (3) nearest-neighbor search—using an RP-tree (Random Projection tree) data structure for fast approximate lookups. The embeddings are stored in a binary cache (`metadata/embeddings.bin`) and rebuilt incrementally as new annotations are added.

A secondary capability is "sort by magic"—using embeddings to order lists by semantic similarity rather than date or alphabetically. This creates visually coherent clusters when displaying tag pages, making related items appear adjacent.

---

## Public API

### `singleShotRecommendations :: String -> IO T.Text`

Generate recommendations HTML for arbitrary HTML text (used during annotation editing).

**Called by:** `preprocess-markdown.hs`
**Calls:** `readLinkMetadata`, `readEmbeddings`, `embed`, `embeddings2Forest`, `findN`, `generateMatches`

### `readEmbeddings :: IO Embeddings`

Load the embedding database from `metadata/embeddings.bin`.

```haskell
type Embedding = (String,    -- URL/path
                  Integer,   -- Creation date (ModifiedJulianDay)
                  String,    -- Formatted document text
                  String,    -- Model version ID
                  [Double])  -- Embedding vector (512 dimensions)
type Embeddings = [Embedding]
```

**Called by:** `generateSimilar.hs`, `singleShotRecommendations`, `sortTagByTopic`
**Calls:** `DB.decodeFileOrFail`

### `writeEmbeddings :: Embeddings -> IO ()`

Persist embeddings to disk with corruption check.

**Called by:** `generateSimilar.hs`
**Calls:** `DB.encodeFile`, atomic rename via temp file

### `embed :: Embeddings -> Metadata -> Backlinks -> (String, MetadataItem) -> IO Embedding`

Create an embedding for a single document. Checks for existing embedding with same basename (handles renames). Appends backlink metadata for richer context.

**Called by:** `generateSimilar.hs`
**Calls:** `formatDoc`, `oaAPIEmbed`

### `missingEmbeddings :: Metadata -> Embeddings -> [(String, MetadataItem)]`

Find metadata entries that don't yet have embeddings.

**Called by:** `generateSimilar.hs`

### `pruneEmbeddings :: Metadata -> Embeddings -> Embeddings`

Remove stale embeddings that no longer have corresponding metadata entries.

**Called by:** `generateSimilar.hs`

### `writeOutMatch :: Metadata -> Backlinks -> (String, [String]) -> IO ()`

Write a similar-links HTML fragment to `metadata/annotation/similar/`.

**Called by:** `generateSimilar.hs`
**Calls:** `generateMatches`, `writeUpdatedFile`

### `sortTagByTopic :: Metadata -> String -> IO [FilePath]`

Sort all pages with a given tag by semantic similarity (for tag index pages).

**Called by:** `hakyll.hs` tag page generation
**Calls:** `sortSimilars`

### `sortSimilars :: Embeddings -> ListSortedMagic -> FilePath -> [FilePath] -> IO [FilePath]`

Sort a list of paths by embedding similarity, starting from a seed. Caches results in `metadata/listsortedmagic.hs`.

**Called by:** `sortTagByTopic`, `sortSimilarsStartingWithNewest`
**Calls:** `lookupNextAndShrink`

---

## Internal Architecture

### Data Flow

```
Metadata Item
    ↓
formatDoc (title, author, date, tags, abstract, backlinks → plaintext)
    ↓
oaAPIEmbed (shell out to embed.sh → OpenAI API)
    ↓
Embedding (URL, date, text, model, [Double])
    ↓
embeddings2Forest (build RP-tree index)
    ↓
findN / findNearest (k-NN search)
    ↓
generateMatches (filter, format as Pandoc → HTML)
```

### Key Data Structures

**Embedding tuple:** `(URL, CreationDate, FormattedText, ModelID, Vector)`
- URL: Path like `/doc/ai/gpt.pdf` or full URL
- CreationDate: Julian day integer for age-based expiry
- FormattedText: The plaintext sent to OpenAI (debugging)
- ModelID: e.g., `text-embedding-3-large`
- Vector: 512 Double values (truncated from 3072)

**RPForest:** Random Projection forest for approximate nearest neighbor search
```haskell
type Forest = RPForest Double (V.Vector (Embed DVector Double String))
```

**ListSortedMagic:** Cache mapping sets of URLs to their similarity-sorted order
```haskell
type ListSortedMagic = M.Map (S.Set FilePath) [FilePath]
```

### RP-Tree Configuration

The forest is built with tuned hyperparameters:
- `minLeafSize = 60`
- `nTrees = 2`
- `projectionVectorDimension = 10`

These were chosen via hyperparameter sweep (commented code shows grid search results achieving ~85% recall).

---

## Key Patterns

### Document Formatting

`formatDoc` converts metadata to embedding-friendly text:

```haskell
formatDoc (path, (title, author, date, dateModified, _, tags, abstract)) =
    -- "'Title' (URL), by Author (2024; updated 2024-06)."
    -- "Keywords: tag1, tag2."
    -- [abstract text]
    -- "References:\n1. /doc/foo.pdf Title..."
```

The format is designed to be comprehensible to language models without HTML parsing. URLs are extracted and listed as numbered references since `simplifiedDoc` strips links.

### Backlink Enrichment

Embeddings include reverse citations to improve similarity matching:

```haskell
let backlinksMetadata = "\n\nReverse citations:\n\n- " ++
    intercalate "\n- " (map formatBacklink backlinks)
```

### Incremental Updates

New embeddings are generated only for items missing from the cache. When an item is embedded, related items are expired to force regeneration:

```haskell
expireMatches :: [String] -> IO ()
expireMatches = mapM_ (removeFile . fst . getSimilarLink)
```

### Clustering for Display

`clusterIntoSublist` splits sorted lists at natural breakpoints (large distance jumps) for grouped display:

```haskell
clusterIntoSublist es list =
    let k = max 1 (round(sqrt(fromIntegral $ length list)) - 1)
        distances = pairwiseDistances listWithEmbeddings
        splitPoints = take k (sortByDistance distances)
    in splitAtIndices splitPoints list
```

---

## Configuration

From `Config/GenerateSimilar.hs`:

| Setting | Value | Purpose |
|---------|-------|---------|
| `bestNEmbeddings` | 20 | Max similar links to show |
| `maximumLength` | 32,700 | Max chars for embedding (≈8k tokens) |
| `minimumSuggestions` | 3 | Don't show section if fewer matches |
| `maxDistance` | 0.95 | L2 distance threshold for relevance |
| `minDistance` | 0.01 | Avoids self-matches |
| `embeddingsPath` | `metadata/embeddings.bin` | Cache location |
| `randSeed` | 23 | Deterministic RP-tree construction |
| `blackList` | `/index`, `/changelog`, etc. | Exclude pathological matches |

---

## Integration Points

### External Dependencies

**embed.sh:** Shell script calling OpenAI API
```bash
ENGINE="text-embedding-3-large"
ENGINE_DIMENSION="512"
curl "https://api.openai.com/v1/engines/$ENGINE/embeddings" \
    -d "{\"input\": \"$TEXT\", \"dimensions\": $ENGINE_DIMENSION}"
```

**rp-tree library:** `Data.RPTree` for approximate nearest neighbor
- Uses `metricL2` (L2-normalized Euclidean ≈ cosine similarity)
- `knn` function for k-nearest lookup

### File Outputs

- `metadata/embeddings.bin` — Binary cache (Data.Binary format)
- `metadata/annotation/similar/*.html` — Pre-rendered HTML fragments
- `metadata/listsortedmagic.hs` — Cached sort-by-similarity results
- `metadata/listname.hs` — LLM-generated cluster names

### Shared State

- Reads: `LinkMetadata` (annotation database), `Backlinks` (link graph)
- LLM integration: `tagguesser.py` generates cluster names from titles

---

## See Also

- [Config.GenerateSimilar](/backend/config-generate-similar-hs) - Configuration constants for embedding parameters and thresholds
- [generateSimilarLinks.hs](/backend/generate-similar-links-hs) - CLI entry point that uses GenerateSimilar functions
- [embed.sh](/shell/embed) - Shell script that calls OpenAI API for embedding generation
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database that provides content for embeddings
- [LinkBacklink.hs](/backend/link-backlink-hs) - Backlinks database used to enrich embedding context
- [sync.sh](/backend/sync-sh) - Build orchestrator that schedules similarity generation
- [extracts.js](/frontend/extracts-js) - Frontend displaying similar links in annotation popups
