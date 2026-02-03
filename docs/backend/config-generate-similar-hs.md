
# Config.GenerateSimilar

**Path:** `build/Config/GenerateSimilar.hs` | **Language:** Haskell | **Lines:** ~60

> Configuration constants for embedding-based similar link recommendations

---

## Overview

Config.GenerateSimilar centralizes tuning parameters for the similar links recommendation system. The system uses OpenAI text embeddings to find semantically related pages, presenting "Similar Links" sections that help readers discover relevant content.

The module contains three categories of settings: (1) embedding pipeline parameters like token limits and result counts, (2) similarity thresholds that control which matches are shown, and (3) blacklists that exclude problematic URLs from recommendations. These values were empirically tuned through observation of recommendation quality.

The configuration is consumed primarily by [GenerateSimilar](generate-similar-hs) for computing recommendations and by [generateDirectory](generate-directory-hs) for displaying auto-inferred tags on directory pages.

---

## Public API

All exports are simple constants—no functions with complex signatures.

### `bestNEmbeddings :: Int`

Number of similar results to retrieve per query. Set to **20**.

**Used by:** `findN` in GenerateSimilar for k-nearest-neighbor lookup

---

### `maximumLength :: Int`

Maximum character count for text sent to OpenAI's embedding API. Set to **32,700** characters.

Based on OpenAI's estimate of ~4 characters per BPE token, this fits within text-embedding-ada-002's 8,191 token limit. The shell script handles truncation and retry if calls fail, so this can be set close to the theoretical maximum.

**Used by:** Text formatting before API calls

---

### `minimumSuggestions :: Int`

Minimum number of similar links required before showing the "Similar Links" section to readers. Set to **3**.

If fewer matches pass all filters, the section is suppressed entirely—one or two weak suggestions aren't worth the UI overhead.

**Used by:** `writeOutMatch` in GenerateSimilar

---

### `iterationLimit :: Int`

Maximum retry iterations for the k-NN search. Set to **6**.

Prevents infinite loops when the search keeps failing to find enough valid results.

**Used by:** `findN` recursive search

---

### `embeddingsPath :: String`

File path for the binary-serialized embeddings database. Set to **"metadata/embeddings.bin"**.

**Used by:** `readEmbeddings`, `writeEmbeddings`

---

### `minDistance, maxDistance :: Double`

Cosine similarity bounds for valid matches:
- **minDistance = 0.01** — Excludes self-matches and near-duplicates
- **maxDistance = 0.95** — Upper relevance threshold (note: this is a *distance*, not similarity, so lower = more similar)

These were empirically tuned. The comment notes a "cliff of relevancy" around 0.60 with text-embedding-ada-002, and warns that thresholds must be rechecked when changing embedding models.

**Used by:** `findNearest` filtering

---

### `blackList :: String -> Bool`

Predicate function that returns `True` for URLs that should be excluded from recommendations.

Excludes:
- Explicit blacklisted URLs (index, changelog, help pages)
- Document index pages (`/doc/*/index`)
- Newsletter archives (`/newsletter/20*`)
- Lorem ipsum test pages (`/lorem*`)

**Used by:** `findN` result filtering

---

### `blackListURLs :: [String]`

Explicit URL blacklist. Currently contains only **["/index", "/changelog", "/help"]**.

A large commented-out section preserves historical blacklist entries that exhibited pathological behavior (matching too many unrelated pages). The TODO suggests re-testing whether these are still needed.

---

### `minTagAuto :: Int`

Minimum auto-inferred tags to display on directory pages. Set to **3**.

If fewer than 3 tags are suggested, they're hidden—sparse tagging isn't useful.

**Used by:** `generateDirectory.hs` for directory page rendering

---

### `maxTitlesForTagGuessing :: Int`

Maximum page titles sent to the LLM for tag name inference. Set to **30**.

Beyond 30 titles, additional context provides diminishing returns while wasting tokens.

**Used by:** Tag guessing in GenerateSimilar

---

### `randSeed :: Word64`

Fixed random seed for reproducible RP-tree construction. Set to **23**.

Ensures embedding forest is deterministic across rebuilds.

**Used by:** RP-tree initialization

---

## Internal Architecture

This module has no internal architecture—it's purely declarative constants. The only logic is `blackList`, which combines explicit membership testing with prefix/suffix pattern matching.

---

## Key Patterns

### Empirically-Tuned Magic Numbers

Every constant includes a comment explaining its rationale. The similarity thresholds carry warnings about model-specific calibration—a good practice for ML-adjacent code.

### Defensive Blacklisting

The blacklist evolved from debugging pathological cases. One specific paper ("Estimating the effect-size of gene dosage...") is called out for somehow matching nearly every embedding, illustrating the kind of edge case this system encounters.

### Conservative Minimums

Both `minimumSuggestions` and `minTagAuto` use 3 as a threshold—a recurring "magic 3" that represents the minimum useful information density.

---

## Configuration

These constants are compile-time configuration. To change values, edit this file and rebuild. There's no runtime configuration mechanism.

| Constant | Value | Purpose |
|----------|-------|---------|
| `bestNEmbeddings` | 20 | Results per query |
| `maximumLength` | 32,700 | Max chars to embed |
| `minimumSuggestions` | 3 | Min results to show |
| `iterationLimit` | 6 | Max search retries |
| `minDistance` | 0.01 | Lower similarity bound |
| `maxDistance` | 0.95 | Upper similarity bound |
| `minTagAuto` | 3 | Min tags for display |
| `maxTitlesForTagGuessing` | 30 | Max titles for LLM |
| `randSeed` | 23 | RP-tree seed |

---

## Integration Points

### Consumers

| Module | Usage |
|--------|-------|
| [GenerateSimilar](generate-similar-hs) | All embedding/similarity settings |
| [generateDirectory](generate-directory-hs) | `minTagAuto` for tag display |

### Data Dependencies

- **embeddings.bin** — Binary-serialized embedding vectors at `metadata/embeddings.bin`

---

## See Also

- [GenerateSimilar.hs](/backend/generate-similar-hs) - Main recommendation engine that consumes these configuration values
- [generateSimilarLinks.hs](/backend/generate-similar-links-hs) - CLI tool that uses GenerateSimilar with this config
- [generateDirectory.hs](/backend/generate-directory-hs) - Uses minTagAuto for directory page tag display
- [Gtx.hs](/backend/gtx-hs) - Annotation format that stores similar links data
- [embed.sh](/shell/embed) - Shell script for OpenAI API calls, uses maximumLength indirectly
