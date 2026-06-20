---
title: "generateSimilarLinks.hs"
description: "CLI tool for generating embedding-based \"similar links\" recommendations as HTML fragments"
---

# generateSimilarLinks.hs

CLI tool for generating embedding-based "similar links" recommendations as HTML fragments

<div className="doc-meta">
  <div><strong>Path</strong><code>build/app/generateSimilarLinks.hs</code></div>
  <div><strong>Language</strong>Haskell</div>
  <div><strong>Lines</strong>186</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/app/generateSimilarLinks.hs">build/app/generateSimilarLinks.hs</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the Haskell build pipeline, generators, metadata code, or backend utility behavior around generateSimilarLinks.
</div>

## Overview

`generateSimilarLinks.hs` is the command-line entry point for gwern.net's embedding-based recommendation system. It generates "See also" HTML fragments containing similar articles based on semantic similarity of OpenAI embeddings.

The tool orchestrates the complete pipeline: reading existing embeddings and metadata, identifying pages that need embeddings, calling the OpenAI API to generate new embeddings, building a normalized `EmbeddingIndex`, computing exact cosine-distance rankings, reordering results for coherence, and writing out HTML fragments to `metadata/annotation/similar/`.

The system is designed for incremental updates: it embeds **missing** pages, can update missing similarity files, can rewrite all similarity files, or can stop after embedding only. This makes it practical for a large corpus without re-embedding everything on each run.

---

## CLI Usage

### Basic Invocation

```bash
# Default: embed missing items and fill missing/newly affected similarity files
generateSimilarLinks

# Embed new items only (no similarity computation)
generateSimilarLinks --only-embed

# Update only items missing similarity files (faster than full rebuild)
generateSimilarLinks --update-only-missing-embeddings

# Rewrite all similarity files
generateSimilarLinks --rewrite-all
```

### Command-Line Flags

- **`--only-embed`**: Generate embeddings for new items, save to database, then exit. Does not compute similarities or write HTML fragments. Used for incremental embedding during annotation work.

- **`--update-only-missing-embeddings`**: Alias for the default missing-only mode.

- **`--rewrite-all` / `--all`**: Embed missing items, then rewrite all annotated URLs that have embeddings.

- **(no arguments)**: Missing-only mode: embed missing items, compute similarities for newly embedded items and URLs missing similarity fragments, then expire reciprocal hits affected by new embeddings.

### Watch Mode Example

The tool supports an `inotifywait`-based daemon for automatic embedding:

```bash
# In crontab
@reboot screen -d -m -S "embed" bash -c \
  'cd ~/wiki/; while true; do \
     inotifywait ~/wiki/metadata/*.gtx -e attrib && \
     sleep 10s && \
     generateSimilarLinks --only-embed; \
   done'
```

This watches annotation database files (`.gtx`) and immediately embeds new annotations, making them available for similarity matching without waiting for nightly rebuild.

---

## Internal Architecture

### Data Flow

1. **Load databases** (metadata, backlinks, embeddings)
2. **Identify missing embeddings**: Compare metadata URLs with embedding database, considering annotated paths in metadata modified-date order
3. **Generate embeddings**: Call OpenAI API for missing items (batch limit: 500 per run)
4. **Prune stale embeddings**: Remove embeddings for URLs no longer in metadata
5. **Build EmbeddingIndex**: Normalized in-memory vector index
6. **Compute similarities**: For each selected URL, find nearest neighbors by exact cosine-distance scan
7. **Reorder results**: Sort matches by pairwise distance for internal coherence
8. **Write HTML fragments**: Generate `metadata/annotation/similar/URL.html` files
9. **Refresh reciprocal matches**: Newly embedded items can expire reciprocal hits, and the current run rewrites eligible reciprocal similarity rankings

### Key Data Structures

**Embedding tuple** (from `GenerateSimilar`):
```haskell
type Embedding = (String, Integer, String, String, [Double])
-- (url/path, ModifiedJulianDay age, embedded text, model id, vector)
```

**EmbeddingIndex**: In-memory index of normalized rows plus a path lookup map.

**Databases loaded**:
- `md :: Metadata` — annotation database (titles, abstracts, dates)
- `bdb :: Backlinks` — bidirectional link index
- `edb :: Embeddings` — OpenAI embedding vectors

### Processing Phases

**Phase 1: Embedding**
- Filter items without abstracts (can't embed empty content)
- Embed entries missing from the embeddings DB (no modification-time prioritization)
- Batch at most 500 embeddings per run (API cost control)
- Use `embed` from `GenerateSimilar` to create embeddings (includes backlinks context)

**Phase 2: Similarity Computation**
- Build an `EmbeddingIndex` via `embeddings2Index`
- For each selected URL, call `lookupPathK` through the CLI's `similarMatches` helper
- Re-sort matches with `seriateGreedy` for pairwise coherence
- Filter out items already linked in abstract or body

**Phase 3: HTML Generation**
- Call `writeOutMatch` to generate HTML fragment
- Fragments stored in `metadata/annotation/similar/URLENCODED.html` (max 274 chars filename)
- Search links (Google Scholar, optional Connected Papers when DOI is present, Google, and gwern.net site search) are appended when metadata has a title/abstract; not conditional on "few" results

---

<details className="generated-section">
<summary>Key Patterns</summary>

### Incremental Embedding Strategy

The tool uses `missingEmbeddings` to identify annotation URLs not yet embedded, then `similaritemExistsP` to check for existing HTML fragments. This two-level check allows:
- New annotations to be embedded immediately
- Existing items missing similarity files to be regenerated
- Full rebuilds to refresh all similarity rankings

### Batching and Parallelization

```haskell
maxEmbedAtOnce = 500  -- API cost control
let mdlMissingChunks = chunksOf 10 mdlMissing
mapM_ (mapM_ (...)) mdlMissingChunks
```

The current CLI runs selected writes sequentially after building a shared index. `maxEmbedAtOnce` bounds how many missing embeddings are generated in one run.

### Reciprocal Update Triggering

When a new embedding is created and matches existing items, those items should show the new page as a match too (similarity is symmetric). Rather than recompute immediately, the tool deletes their HTML fragments via `expireMatches`, forcing rebuild on next run:

```haskell
when (f `elem` todoLinks) $ expireMatches (snd nmatchesSorted)
```

### Reordering for Coherence

Raw nearest-neighbor results are reordered with `seriateGreedy`, which greedily minimizes pairwise distances among the result set. This produces more internally coherent recommendation lists: items that are not only similar to the target, but also similar to each other.

### Stale Embedding Cleanup

`pruneEmbeddings` removes embeddings for URLs no longer in the metadata database (typically renamed/deleted). Prevents false positives and database bloat.

---
</details>

<details className="generated-section">
<summary>Configuration</summary>

Via `Config.GenerateSimilar`:

- **`bestNEmbeddings`**: Number of similar items to find (20)
- **`maxDistance`**: Maximum cosine-distance threshold
- **`minimumSuggestions`**: Don't write HTML if the pre-pruning match list has fewer matches; later rendering may still prune already-linked items
- **`blackList`**: URLs to exclude from recommendations
- **`embeddingsPath`**: Path to serialized embeddings database

Via `maxEmbedAtOnce` constant:
- Limits batch size to 500 embeddings per run (API cost control)

---
</details>

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
- **Watch daemon**: `inotifywait`-based embedding-on-write

### Calls

All core functionality from **[generate-similar-hs](generate-similar-hs)**:
- `embed`: Generate OpenAI embeddings with backlinks context
- `readEmbeddings` / `writeEmbeddings`: Binary serialization
- `embeddings2Index`: Build normalized in-memory vector index
- `lookupPathK`: Exact k-nearest lookup by path
- `seriateGreedy`: Pairwise distance minimization reordering
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

If embeddings fail (rate limit, network error, API key), the tool will error out. Embeddings are batched at 500/run to avoid hitting rate limits too hard. Retry by re-running—already-embedded items are skipped.

### Insufficient Matches

If the pre-pruning match list has fewer than `minimumSuggestions` matches, no HTML is written. Later rendering prunes items already linked in the abstract, body, or backlinks, so the visible fragment can contain fewer final items.

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

<details className="generated-section">
<summary>See Also</summary>

- [GenerateSimilar.hs](/backend/generate-similar-hs) - Core embedding and similarity logic used by this CLI tool
- [Config.GenerateSimilar](/backend/config-generate-similar-hs) - Configuration constants for thresholds and limits
- [hakyll.hs](/backend/hakyll-hs) - Build system that uses sortTagByTopic for tag page generation
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database providing content for embeddings
- [LinkBacklink.hs](/backend/link-backlink-hs) - Bidirectional link index used for filtering and context
- [sync.sh](/backend/sync-sh) - Build orchestrator that invokes generateSimilarLinks.hs
</details>
