---
sidebar_position: 2
---

# embed.sh

**Path:** `build/embed.sh` | **Language:** Bash | **Lines:** ~1,628

Generates neural network embeddings for text using OpenAI's API for semantic similarity search.

---

## Overview

This script provides a command-line interface to OpenAI's embedding API, converting text strings into high-dimensional vector representations (embeddings). These embeddings are used by gwern.net's "similar links" feature to find semantically related content across the site.

The script uses the `text-embedding-3-large` model with dimensionality reduced to 512 for efficiency while retaining most semantic information. It handles API errors gracefully, automatically truncating inputs that exceed token limits, and returns embeddings as JSON arrays prefixed with the model name for versioning.

Because embeddings are model-specific and incomparable across different models, the script always outputs the model name/version on the first line, followed by the embedding vector on subsequent lines.

## Key Commands/Variables

- **`ENGINE="text-embedding-3-large"`**: OpenAI embedding model (1536-dimensional base)
- **`ENGINE_DIMENSION="512"`**: Reduced dimensionality for performance/cost tradeoff
- **`OPENAI_API_KEY`**: Environment variable for API authentication
- **`TEXT="$*"`**: Input text to embed (from arguments or stdin)
- **`TEXT_LENGTH`**: Character count, used for automatic truncation on errors
- **`TRUNCATED`**: Tracks how many characters removed (max 500)
- **`curl ... /v1/engines/$ENGINE/embeddings`**: OpenAI API endpoint
- **`jq --raw-output '.model, .data[0].embedding'`**: Parses JSON response

## Usage

```bash
./embed.sh <text>
# or
echo "text" | ./embed.sh
```

**Arguments:**
- Text to embed (via command line arguments or stdin)

**Output format:**
```
text-embedding-3-large
[
  -0.016606577,
  -0.015486566,
  ...
]
```

**Examples:**
```bash
$ embed.sh "foo bar"
text-embedding-3-large
[
  -0.016606577,
  -0.015486566,
  -0.01253444,
  ...
]

$ echo "Sample document text" | embed.sh > embedding.json
```

**Error handling:**
- Automatically truncates text in 100-character increments if input exceeds token limits
- Exits with error if API quota exceeded
- Maximum truncation: 500 characters

**Dependencies:**
- `curl`: HTTP client for API requests
- `jq`: JSON parsing
- `OPENAI_API_KEY`: Valid OpenAI API key in environment

---

## See Also

- [GenerateSimilar.hs](/backend/generate-similar-hs) - Uses embeddings for recommendation system
- [extracts.js](/frontend/extracts-js) - Frontend integration for similar links
- [sync.sh](/backend/sync-sh) - Build process that generates embeddings
- [LinkMetadata.hs](/backend/link-metadata-hs) - Metadata system storing embedding references
- [Annotation.hs](/backend/annotation-hs) - Annotation system using similar links
- [upload.sh](/shell/upload) - File upload that may trigger embedding generation
