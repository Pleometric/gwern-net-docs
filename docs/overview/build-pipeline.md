---
title: gwern.net Build Pipeline Hub
description: Start here for gwern.net's build pipeline, including sync.sh orchestration, Hakyll, Pandoc transforms, generated content, validation, and deployment flow.
keywords:
  - gwern.net build pipeline
  - Hakyll
  - Pandoc
  - sync.sh
  - static site generation
sidebar_position: 2
---

# Build Pipeline Hub

Start here to understand how gwern.net source files become the deployed static site.

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
