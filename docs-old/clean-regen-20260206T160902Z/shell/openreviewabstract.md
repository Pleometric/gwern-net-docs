---
sidebar_position: 1
---

# openReviewAbstract.sh

**Path:** `build/openReviewAbstract.sh` | **Language:** Bash | **Lines:** ~62

Web scraper for extracting paper metadata from OpenReview conference submissions.

---

## Overview

`openReviewAbstract.sh` is a specialized scraper that extracts structured metadata from OpenReview.net paper submissions. OpenReview is a prominent platform for peer-reviewed machine learning conference papers (NeurIPS, ICML, ICLR, etc.). This script parses the embedded JSON data from OpenReview pages to extract title, authors, publication date, abstract, TL;DR summary, and keywords.

The script handles two different JSON schema variations that OpenReview uses (older `.value` nested structure and newer direct property access), normalizes the output, and performs cleanup operations like joining hyphenated line breaks and converting LaTeX markup to more readable forms.

This tool is used during the manual annotation workflow when adding research paper links to gwern.net. It provides clean, properly formatted metadata that can be directly inserted into annotation files.

## Key Commands/Variables

**External Dependencies:**
- `curl` - HTTP client for fetching pages
- `tidy` - HTML normalizer (puts JSON on single line for easier extraction)
- `jq` - JSON parser for extracting structured fields

**Main Pipeline:**
1. **Fetch & normalize**: `curl --silent --location "$@" | tidy -quiet`
2. **Extract JSON**: `grep -F "pageProps"` finds the embedded Next.js props object
3. **Parse metadata**: `jq` extracts title, authors, date, TL;DR, abstract, keywords
4. **Cleanup**: `sed` operations for null handling, line joining, LaTeX conversion

**Field Mapping:**
- Title: `.props.pageProps.forumNote.content.title[.value]`
- Authors: `.props.pageProps.forumNote.content.authors[.value]` (joined with ", ")
- Date: `.props.pageProps.forumNote.tmdate` (Unix timestamp → YYYY-MM-DD)
- TL;DR: `.props.pageProps.forumNote.content."TL;DR"`
- Abstract: `.props.pageProps.forumNote.content.abstract[.value]`
- Keywords: `.props.pageProps.forumNote.content.keywords` (joined with ", ")

**Cleanup Operations:**
- `s/^null$//'` - Empty fields become empty lines
- `:again; /[[:alpha:]]-$/ { N; s/-\n//; b again; }` - Join hyphenated line breaks
- `s/\\mbox{\([[:graph:]]*\)}/\\emph{\1}/g` - Convert LaTeX `\mbox{}` to `\emph{}`
- `s/\\citep\?{\([[:graph:]]*\)}/\(\\texttt{\1}\)/g` - Convert LaTeX citations to plain text

## Usage

**Basic invocation:**
```bash
./openReviewAbstract.sh 'https://openreview.net/forum?id=BkjLkSqxg'
```

**Output format** (6 lines, some may be empty):
```
LipNet: End-to-End Sentence-level Lipreading
Yannis M. Assael, Brendan Shillingford, Shimon Whiteson, Nando de Freitas
2016-12-16

LipNet is the first end-to-end sentence-level lipreading model...
Computer vision, Deep learning
```

**Common workflow:**
```bash
# Scrape metadata
./openReviewAbstract.sh 'https://openreview.net/forum?id=bwq6O4Cwdl' > metadata.txt

# Copy-paste into annotation file editor
cat metadata.txt
```

**Arguments:**
- Takes a single OpenReview URL as argument
- URL format: `https://openreview.net/forum?id={PAPER_ID}`
- Supports redirects via `curl --location`

**Exit behavior:**
- Does not use `set -e`, so errors are not fatal
- Invalid URLs or missing fields result in "null" output (cleaned to empty lines)

## See Also

- [Annotation/OpenReview.hs](/backend/annotation-openreview-hs) - Haskell module that calls this script
- [Annotation.hs](/backend/annotation-hs) - Main annotation dispatcher that routes to scrapers
- [LinkMetadata.hs](/backend/link-metadata-hs) - Database manager for storing scraped annotations
- [preprocess-annotation.sh](/shell/preprocess-annotation) - Markdown preprocessing for annotation editing
- [Annotation/Arxiv.hs](/backend/annotation-arxiv-hs) - Similar scraper for arXiv papers
- [Annotation/Biorxiv.hs](/backend/annotation-biorxiv-hs) - Similar scraper for bioRxiv papers
