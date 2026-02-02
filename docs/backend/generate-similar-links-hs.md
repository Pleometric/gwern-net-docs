
# generateSimilarLinks.hs

**Path:** `build/generateSimilarLinks.hs` | **Language:** Haskell | **Lines:** ~106

> CLI tool for generating embedding-based "similar links" recommendations as HTML fragments

---

## Overview

`generateSimilarLinks.hs` is the command-line entry point for gwern.net's embedding-based recommendation system. It generates "See also" HTML fragments containing similar articles based on semantic similarity of OpenAI embeddings.

The tool orchestrates the complete pipeline: reading existing embeddings and metadata, identifying pages that need embeddings, calling the OpenAI API to generate new embeddings, building an RP-tree spatial index for fast nearest-neighbor search, computing similarity rankings, reordering results for coherence, and writing out HTML fragments to `metadata/annotation/similar/`.

The system is designed for incremental updates—it only embeds new/modified pages, supports watch-mode for immediate updates during annotation workflows, and can operate in several modes: full rebuild, embedding-only, or update-missing-only. This makes it practical for a large corpus (thousands of documents) without re-embedding everything on each run.

---

## CLI Usage

### Basic Invocation

```bash
# Full pipeline: embed missing items, compute all similarities, write HTML
runghc -istatic/build/ ./static/build/generateSimilarLinks.hs

# Embed new items only (no similarity computation)
runghc -istatic/build/ ./static/build/generateSimilarLinks.hs --only-embed

# Update only items missing similarity files (faster than full rebuild)
runghc -istatic/build/ ./static/build/generateSimilarLinks.hs --update-only-missing-embeddings
```

### Command-Line Flags

- **`--only-embed`**: Generate embeddings for new items, save to database, then exit. Does not compute similarities or write HTML fragments. Used for incremental embedding during annotation work.

- **`--update-only-missing-embeddings`**: Embed new items (if any), then compute similarities only for URLs missing HTML fragments. Skips full rebuild of all existing similarity files.

- **(no arguments)**: Full rebuild—embed missing items, compute similarities for all annotated URLs, write all HTML fragments.

### Watch Mode Example

The tool supports an `inotifywait`-based daemon for automatic embedding:

```bash
# In crontab
@reboot screen -d -m -S "embed" bash -c \
  'cd ~/wiki/; while true; do \
     inotifywait ~/wiki/metadata/*.gtx -e attrib && \
     sleep 10s && \
     runghc -istatic/build/ ./static/build/generateSimilarLinks.hs --only-embed; \
   done'
```

This watches annotation database files (`.gtx`) and immediately embeds new annotations, making them available for similarity matching without waiting for nightly rebuild.

---

## Internal Architecture

### Data Flow

1. **Load databases** (metadata, backlinks, embeddings)
2. **Identify missing embeddings**: Compare metadata URLs with embedding database, prioritize by modification date
3. **Generate embeddings**: Call OpenAI API for missing items (batch limit: 2000 at once)
4. **Prune stale embeddings**: Remove embeddings for URLs no longer in metadata
5. **Build RP-tree forest**: Spatial index for fast k-nearest-neighbor search
6. **Compute similarities**: For each URL, find N nearest neighbors via tree search
7. **Reorder results**: Sort matches by pairwise distance for internal coherence
8. **Write HTML fragments**: Generate `metadata/annotation/similar/URL.html` files
9. **Expire reciprocal matches**: Delete similarity files for newly-matched items to trigger rebuild

### Key Data Structures

**Embedding tuple** (from `GenerateSimilar`):
```haskell
type Embedding = (FilePath, String, String, String, [Double])
-- (path, title, author, date, 1536-dim vector)
```

**Forest**: RP-tree spatial index built from embeddings for fast approximate nearest-neighbor search.

**Databases loaded**:
- `md :: Metadata` — annotation database (titles, abstracts, dates)
- `bdb :: Backlinks` — bidirectional link index
- `edb :: Embeddings` — OpenAI embedding vectors
- `sortDB :: ListSortedMagic` — cached sorted-by-similarity lists

### Processing Phases

**Phase 1: Embedding**
- Filter items without abstracts (can't embed empty content)
- Exclude index pages (`/index`)
- Prioritize recently modified items
- Batch at most 2000 embeddings per run (API cost control)
- Use `embed` from `GenerateSimilar` to create embeddings (includes backlinks context)

**Phase 2: Similarity Computation**
- Build RP-tree forest via `embeddings2Forest`
- For each URL, call `findN` to get top N nearest neighbors
- Re-sort matches with `sortSimilars` for pairwise coherence (not just distance to target)
- Filter out items already linked in abstract or body

**Phase 3: HTML Generation**
- Call `writeOutMatch` to generate HTML fragment
- Fragments stored in `metadata/annotation/similar/URLENCODED.html` (max 274 chars filename)
- Includes Google Scholar link fallback if few similar items found

---

## Key Patterns

### Incremental Embedding Strategy

The tool uses `missingEmbeddings` to identify annotation URLs not yet embedded, then `similaritemExistsP` to check for existing HTML fragments. This two-level check allows:
- New annotations to be embedded immediately
- Existing items with outdated similarity files to be regenerated
- Full rebuilds to refresh all similarity rankings

### Batching and Parallelization

```haskell
maxEmbedAtOnce = 2000  -- API cost control
let mdlMissingChunks = chunksOf 10 mdlMissing
mapM_ (mapM_ (...)) mdlMissingChunks
```

Processing happens in chunks of 10 for progress visibility. Full rewrites use `Control.Monad.Parallel.mapM_` for parallel HTML generation.

### Reciprocal Update Triggering

When a new embedding is created and matches existing items, those items should show the new page as a match too (similarity is symmetric). Rather than recompute immediately, the tool deletes their HTML fragments via `expireMatches`, forcing rebuild on next run:

```haskell
when (f `elem` todoLinks) $ expireMatches (snd nmatchesSorted)
```

### Reordering for Coherence

Raw nearest-neighbor results are reordered with `sortSimilars`, which minimizes pairwise distances among the result set. This produces more internally coherent recommendation lists—items that are not only similar to the target, but also similar to each other.

### Stale Embedding Cleanup

`pruneEmbeddings` removes embeddings for URLs no longer in the metadata database (typically renamed/deleted). Prevents false positives and database bloat.

---

## Configuration

Via `Config.GenerateSimilar`:

- **`bestNEmbeddings`**: Number of similar items to find (default ~10-20)
- **`iterationLimit`**: Max k-NN search iterations before giving up
- **`maxDistance`**: Maximum embedding distance threshold
- **`minimumSuggestions`**: Don't write HTML if fewer matches found
- **`blackList`**: URLs to exclude from recommendations
- **`embeddingsPath`**: Path to serialized embeddings database

Via `maxEmbedAtOnce` constant:
- Limits batch size to 2000 embeddings per run (API cost control)

---

## Integration Points

### Inputs

- **`metadata/full.gtx`** (via [link-metadata-hs](link-metadata-hs)): Annotation database with abstracts for embedding
- **`metadata/backlinks.hs`** (via [link-backlink-hs](link-backlink-hs)): Bidirectional link index for filtering
- **`metadata/embeddings.bin`**: Serialized OpenAI embedding vectors
- **`metadata/listsortedmagic.hs`**: Cached sorted-by-similarity lists

### Outputs

- **`metadata/annotation/similar/*.html`**: HTML fragments with similar link lists
- **`metadata/embeddings.bin`**: Updated embedding database (binary serialization)

### Called By

- **[sync-sh](sync-sh)**: Nightly rebuild phase
- **`preprocess-markdown.hs`**: Single-shot mode for immediate annotation updates
- **Watch daemon**: `inotifywait`-based embedding-on-write

### Calls

All core functionality from **[generate-similar-hs](generate-similar-hs)**:
- `embed`: Generate OpenAI embeddings with backlinks context
- `readEmbeddings` / `writeEmbeddings`: Binary serialization
- `embeddings2Forest`: Build RP-tree spatial index
- `findN`: k-NN search with iteration-based expansion
- `sortSimilars`: Pairwise distance minimization reordering
- `writeOutMatch`: HTML fragment generation
- `pruneEmbeddings`: Remove stale entries
- `expireMatches`: Delete HTML to force rebuild

Also uses:
- **[link-metadata-hs](link-metadata-hs)**: `readLinkMetadata`, `sortItemPathDateModified`
- **[link-backlink-hs](link-backlink-hs)**: `readBacklinksDB`, `getSimilarLink`

### HTML Fragment Integration

Generated HTML fragments are transcluded by the annotation system when popups/popovers are displayed. The similar links appear in a "See also" section at the bottom of annotations.

---

## Common Failure Points

### OpenAI API Errors

If embeddings fail (rate limit, network error, API key), the tool will error out. Embeddings are batched at 2000/run to avoid hitting rate limits too hard. Retry by re-running—already-embedded items are skipped.

### Insufficient Matches

If fewer than `minimumSuggestions` matches are found, no HTML is written. This is expected for very niche content with no similar pages.

### Corrupted Embedding Database

Binary deserialization can fail if `embeddings.bin` is corrupted. The `writeEmbeddings` function has a write-validate-rename pattern to prevent corruption, but if it happens, delete the file to force full rebuild.

### Watch Mode Pitfalls

The `inotifywait` daemon example watches for `attrib` events on `.gtx` files. If annotation writes don't trigger this event type (filesystem-dependent), use `-e modify` instead. The 10s sleep prevents race conditions with concurrent annotation writes.

### Filename Length Limits

Output paths are truncated to 274 characters to avoid filesystem limits:

```haskell
let f = take 274 $ "metadata/annotation/similar/" ++ urlEncode p ++ ".html"
```

Very long URLs will have truncated filenames, which can cause collisions (rare but possible).

---

## See Also

- [GenerateSimilar.hs](/backend/generate-similar-hs) - Core embedding and similarity logic used by this CLI tool
- [Config.GenerateSimilar](/backend/config-generate-similar-hs) - Configuration constants for thresholds and limits
- [hakyll.hs](/backend/hakyll-hs) - Build system that uses sortTagByTopic for tag page generation
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database providing content for embeddings
- [LinkBacklink.hs](/backend/link-backlink-hs) - Bidirectional link index used for filtering and context
- [sync.sh](/backend/sync-sh) - Build orchestrator that invokes generateSimilarLinks.hs
