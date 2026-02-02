---
sidebar_position: 7
---

# tagguesser.py

**Path:** `build/tagguesser.py` | **Language:** Python | **Lines:** ~58

Suggests taxonomic tags for clusters of link annotations based on their titles using GPT-4.

---

## Overview

Gwern.net organizes thousands of annotated links into hierarchical tag taxonomies (like `psychology/smell`, `ai/scaling`, `fiction/science-fiction`). When restructuring annotations or identifying implicit clusters, this script uses GPT-4o-mini to suggest concise, meaningful tag names for groups of related articles.

The script is used as part of the `sort-by-magic` workflow, which reorders annotation lists using pairwise nearest-neighbor embeddings to surface hidden thematic clusters. Once clusters are identified, `tagguesser.py` generates tag suggestions by analyzing the titles (not full abstracts—just enough to identify the common theme).

Input format is newline-delimited: first line is the parent tag (e.g., `psychology`), second line is a space-separated blacklist of already-used tags, and remaining lines are article titles. Titles are shuffled before processing to reduce ordering effects.

The output is a single suggested tag (or a short list of candidates) in gwern.net tag format: lowercase, alphanumeric-only, hyphen-separated, singular, command-line and URL-safe.

## Key Functions

- **`shuffle_input(input_text)`**: Randomizes title order to prevent GPT-4 from being biased by input order
- **Main prompt construction**: Instructs GPT-4o-mini to generate 5 candidate tags, then select the best one
- **Tag format constraints**: Lowercase, hyphen-separated, singular, no special characters, 1-2 words max
- **Blacklist handling**: Prevents reusing tags from previous runs
- **Conservative strategy**: Uses GPT-4o-mini for cost efficiency (will switch to GPT-4 when caching is available)

**Example tag output format:**
- Valid: `olfactory-art`, `scaling`, `dnm-archive`, `long-now`, `spacex-critique`
- Invalid: `Scaling`, `olfactory_art`, `olfactories`, `fiction/sci-fi`, `very-long-tag-name-here`

## Command Line Usage

```bash
# From stdin (typical usage)
echo -e 'psychology/smell\nJohn Smith\nArchaeometric Identification of a Perfume from Roman Times\nEau De Cleopatra: Mendesian Perfume\nThe Odor Value Concept in Olfactory Art\nThe Aesthetics of Smelly Art\nArithmetic By Smell' | OPENAI_API_KEY="sk-XXX" python tagguesser.py
# Output: olfactory-art

# From command-line argument
OPENAI_API_KEY="sk-XXX" python tagguesser.py "parent-tag\nblacklist-tag1 blacklist-tag2\nTitle 1\nTitle 2\nTitle 3"

# Integration with sort-by-magic workflow
# 1. sort-by-magic reorders links by embedding similarity
# 2. Human identifies cluster boundaries
# 3. Extract titles for cluster
# 4. Pass to tagguesser.py
# 5. Review suggested tag, create new tag directory if appropriate
```

**Input format:**
```
parent-tag
blacklisted-tag-1 blacklisted-tag-2 blacklisted-tag-3
Article Title 1
Article Title 2
Article Title 3
...
```

**Requirements:**
- Python 3
- `openai` Python package
- Valid OpenAI API key in `$OPENAI_API_KEY` environment variable

**Design rationale:**
- **Shuffling titles**: Prevents GPT from anchoring on first/last titles
- **Blacklist**: Avoids generating the same tag repeatedly when subdividing clusters
- **Parent tag context**: Helps generate more specific tags (e.g., `olfactory-art` instead of just `art` under `psychology`)
- **1-2 words max**: Keeps tags concise and scannable
- **Singular form**: Consistency with gwern.net tag conventions

## See Also

- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database manager
- [GTX.hs](/backend/gtx-hs) - Annotation storage format
- [Tags.hs](/backend/tags-hs) - Tag validation and processing
- [title-cleaner.py](/python/title-cleaner) - Cleans scraped titles before tag analysis
- [paragraphizer.py](/python/paragraphizer) - Companion LLM-based text processing
- [italicizer.py](/python/italicizer) - Companion LLM-based formatting
