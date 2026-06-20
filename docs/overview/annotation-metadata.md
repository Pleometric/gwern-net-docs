---
title: Annotation and Metadata
description: A guided path through GTX databases, Haskell metadata processing, scrapers, generated fragments, and frontend annotation loading.
sidebar_position: 3
---

# Annotation and Metadata

This is the data path behind annotated links. It connects the [Build Pipeline](/overview/build-pipeline), where metadata is read and generated, to the [Popup System](/overview/popup-system), where annotation fragments are shown to readers.

## Reading Path

| Step | Page | Why it matters |
|------|------|----------------|
| 1 | [LinkMetadata.hs](/backend/link-metadata-hs) | Central metadata lookup, annotation creation, and fragment generation |
| 2 | [Annotation.hs](/backend/annotation-hs) | Dispatcher that routes URLs to source-specific metadata extractors |
| 3 | [GTX.hs](/backend/gtx-hs) | Parser and writer for the line-based metadata database format |
| 4 | [LinkMetadataTypes.hs](/backend/link-metadata-types-hs) | Shared metadata types used across the backend |
| 5 | [annotations.js](/frontend/annotations-js) | Browser-side annotation data loading and caching |

## Scrapers and Cleanup

| Area | Start here |
|------|------------|
| arXiv | [annotation-arxiv.hs](/backend/annotation-arxiv-hs) |
| BioRxiv and MedRxiv | [annotation-biorxiv.hs](/backend/annotation-biorxiv-hs) |
| Local gwern.net pages | [annotation-gwernnet.hs](/backend/annotation-gwernnet-hs) |
| OpenReview | [annotation-openreview.hs](/backend/annotation-openreview-hs) |
| PDFs | [annotation-pdf.hs](/backend/annotation-pdf-hs) |
| Author cleanup | [Metadata.Author.hs](/backend/metadata-author-hs) |
| Title cleanup | [Metadata.Title.hs](/backend/metadata-title-hs) |
| Abstract formatting | [Metadata.Format.hs](/backend/metadata-format-hs) |

## Where It Surfaces

- [Popup System](/overview/popup-system) shows how metadata appears in popups and popovers.
- [annotation-blockquote templates](/templates/annotation-blockquote-inside) define rendered annotation HTML.
- [generateSimilar.hs](/backend/generate-similar-hs) reuses metadata for embedding-based recommendations.
- [Functional Taxonomy](/overview/component-taxonomy#2-annotation--metadata) lists the broader set of annotation and metadata files.
