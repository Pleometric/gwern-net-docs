---
sidebar_position: 6
---

# substack-check.sh

**Path:** `build/substack-check.sh` | **Language:** Bash | **Lines:** ~171

Filters URL lists to domains that are hosted on Substack, including custom domains.

---

## Overview

`substack-check.sh` reads a list of URLs and writes out only URLs whose domains appear to be served by Substack.

It uses a staged strategy:

1. Fast domain suffix match (`*.substack.com` or `substack.com`)
2. Parallel DNS CNAME checks for `substack-custom-domains`
3. Parallel HTTP `HEAD` fallback checking `x-served-by: Substack`

This catches both canonical Substack domains and many custom-domain setups.

## Inputs and Outputs

- **Input:** newline-delimited URLs, from stdin or a file argument
- **Output:** newline-delimited matching URLs (default: `substack-urls.txt`)

Usage:

```bash
./substack-check.sh urls.txt out.txt
cat urls.txt | ./substack-check.sh
```

## Runtime Configuration

- `PARALLEL_DNS` (default `50`): CNAME check concurrency
- `PARALLEL_HEAD` (default `10`): HTTP fallback concurrency

These are read from environment variables and can be overridden per run.

## Dependencies

The script hard-fails if required tools are missing:

- `dig`
- `curl`
- `xargs`
- `sed`
- `wc`
- `grep`

## Detection Logic

### Phase 1: Direct Domain Classification

Extract host with a regex and mark obvious Substack domains as matches immediately.

### Phase 2: Parallel CNAME Resolution

For unknown domains, check:

- `CNAME domain`
- `CNAME www.domain` (fallback)

If either resolves to `substack-custom-domains`, mark as match.

### Phase 3: HTTP Header Fallback

For remaining unknown domains, send `curl --head --location` and look for:

- `x-served-by: Substack`

This catches apex/custom domains where CNAME checks are inconclusive.

### Phase 4: Emit Filtered URL List

Write only URLs whose extracted domain was marked as Substack-hosted.

## Notes

- Uses `set -e`, so command failures stop execution unless explicitly handled.
- The script header still uses the historical label `substack-filter.sh`, but the file name in-tree is `substack-check.sh`.
- Designed for investigative/maintenance workflows (eg, mirror targeting, URL-history analysis).

---

## See Also

- [sync.sh](/backend/sync-sh) - Broader build/deploy workflow
- [link-archive](/shell/link-archive) - URL processing and archival pipeline
- [upload](/shell/upload) - Asset pipeline script with external tool orchestration

