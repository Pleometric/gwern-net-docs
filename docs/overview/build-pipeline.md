---
title: Build Pipeline
description: A guided path through sync.sh orchestration, Hakyll, Pandoc transforms, generated content, validation, and deployment flow.
sidebar_position: 2
---

# Build Pipeline

This is the build-time path through the reference. It pairs with [Architecture at a Glance](/overview/architecture-at-a-glance), which explains the broad build/runtime split, and [Page Lifecycle](/overview/page-lifecycle), which follows a single page through the whole system.

## Reading Path

| Step | Page | Why it matters |
|------|------|----------------|
| 1 | [Page Lifecycle](/overview/page-lifecycle) | End-to-end walkthrough from Markdown source to browser runtime |
| 2 | [sync.sh](/backend/sync-sh) | Master orchestration script for build, generation, validation, and deployment |
| 3 | [hakyll.hs](/backend/hakyll-hs) | Hakyll entry point and Pandoc transform pipeline |
| 4 | [bash.sh](/backend/bash-sh) | Shared shell helpers used by the build scripts |
| 5 | [test.hs](/backend/test-hs) | Validation and regression checks |

## Generated Content

| Area | Start here |
|------|------------|
| Directory pages | [generateDirectory.hs](/backend/generate-directory-hs) |
| Link bibliographies | [generateLinkBibliography.hs](/backend/generate-link-bibliography-hs) |
| Backlinks | [generateBacklinks.hs](/backend/generate-backlinks-hs) |
| Similar links | [generateSimilarLinks.hs](/backend/generate-similar-links-hs) |

## Adjacent Systems

- [Templates](/templates/default) define the HTML structure that Hakyll fills.
- [PHP asset pipeline](/php/asset) builds CSS, JavaScript, icons, and includes.
- [Nginx configuration](/nginx/gwern-net-conf) documents the production server layer.
- [Annotation and Metadata](/overview/annotation-metadata) covers the metadata inputs consumed during the build.
- [Popup System](/overview/popup-system) covers the runtime layer that uses the generated annotation and transclusion data.
