---
slug: /
title: gwern.net Codebase Documentation
description: Unofficial technical documentation for the gwern.net Hakyll, Pandoc, Haskell, JavaScript, annotation, popup, and static-site architecture.
sidebar_position: 1
---

:::info Source Version
These docs are based on gwern.net commit [`406d3e423`](https://github.com/gwern/gwern.net/commit/406d3e423) (2026-06-20)
:::

# gwern.net Codebase Documentation

This is an unofficial map of the [gwern.net](https://gwern.net) codebase: how Markdown essays move through the Hakyll/Pandoc build, how link metadata becomes annotations, and how the frontend turns static HTML into interactive pages.

## Quick Start

| If you want to... | Start here |
|-------------------|------------|
| Get the system model | [Architecture at a Glance](/overview/architecture-at-a-glance) |
| Follow one page from source to browser | [Page Lifecycle](/overview/page-lifecycle) |
| Find the file responsible for a feature | [Functional Taxonomy](/overview/component-taxonomy) |

If you are new to the codebase, read the [architecture overview](/overview/architecture-at-a-glance) first, then use [page lifecycle](/overview/page-lifecycle) for the end-to-end flow and [functional taxonomy](/overview/component-taxonomy) as the map of individual files.

## Guided Topics

These are shorter paths through the larger reference when you already know what subsystem you care about.

| Topic | Start here | Covers |
|-------|------------|--------|
| Build pipeline | [Build Pipeline](/overview/build-pipeline) | Hakyll, Pandoc, generated content, validation, and deployment |
| Annotation metadata | [Annotation and Metadata](/overview/annotation-metadata) | GTX records, metadata scrapers, annotation fragments, and frontend loading |
| Popup system | [Popup System](/overview/popup-system) | Hover previews, mobile popovers, Extracts dispatch, transclusion, and annotation display |

## What is gwern.net?

gwern.net is a long-form essay site by pseudonymous writer [Gwern Branwen](https://gwern.net/about), with frontend work by [Said Achmiz](https://github.com/achmizs). It is known for long, heavily cited essays and for a site design that treats the web page as an extended reading environment rather than a simple document.

The codebase is interesting because many of the reading features are custom infrastructure: hover popups for citations and local pages, inline transclusion, margin sidenotes, generated bibliographies, annotation databases, and link archiving.

The backend is built around [Hakyll](https://jaspervdj.be/hakyll/) and [Pandoc](https://pandoc.org/), with Haskell modules handling metadata, transforms, validation, generated pages, and deployment support. The frontend is vanilla JavaScript built around an event system that coordinates popups, transclusion, theming, layout, and other runtime behavior.

## Why This Exists

Gwern has written about the site's design in [About This Website](https://gwern.net/about), [Design Graveyard](https://gwern.net/design-hierarchical), and [Typography](https://gwern.net/design-typography). This reference is meant to complement those essays by organizing the source code itself: what the major modules do, where features enter the pipeline, and how the build-time and runtime systems fit together.

Use it as a companion while reading the [upstream source](https://github.com/gwern/gwern.net), or as a routing layer when asking an agent to inspect a subsystem. These docs are source-derived but unofficial; the codebase changes often, and the source remains the authority.

### How These Docs Were Created

To create the docs, I first built a rudimentary map with notes I wrote while reading the codebase. I then fed this map to multiple Claude Opus 4.5 agents, with each cohort documenting a different part of the codebase. I manually reviewed the output and did a secondary fact-checking step with GPT 5.2 and Claude again.

:::warning Disclaimer
Every now and then I find a new mistake or a not-quite-accurate description. These docs are by no means authoritative, comprehensive, or fully correct. If you find any problems or inaccuracies, feel free to contact me and I'll correct them as soon as possible.
:::

---

## Documentation Structure

The overview section has three layers: [Architecture at a Glance](/overview/architecture-at-a-glance) for the system model, [Page Lifecycle](/overview/page-lifecycle) for the chronological flow, and [Functional Taxonomy](/overview/component-taxonomy) for the full file map with importance scores.

| Category | Description |
|----------|-------------|
| Overview | High-level architecture, page lifecycle, and functional taxonomy |
| [Build Pipeline](/overview/component-taxonomy#1-build-pipeline) | Core build scripts (sync.sh, hakyll.hs, bash.sh) |
| [Annotation & Metadata](/overview/component-taxonomy#2-annotation--metadata) | Link metadata system, scrapers, and processing |
| [Popup System](/overview/component-taxonomy#3-popup-system) | Popups, popovers, and extract handling |
| [Link Processing](/overview/component-taxonomy#4-link-processing) | Archives, icons, auto-linking, and interwiki |
| [Content Rendering](/overview/component-taxonomy#5-content-rendering) | Core JS framework, transclusion, and DOM rewriting |
| [Typography & Layout](/overview/component-taxonomy#6-typography--layout) | Text transforms, sidenotes, columns, and images |
| [Theming & UI](/overview/component-taxonomy#7-theming--ui) | Dark mode, reader mode, colors, and CSS |
| [Backend Utilities](/overview/component-taxonomy#8-utilities--infrastructure) | Haskell helper modules |
| [Frontend Utilities](/overview/component-taxonomy#8-utilities--infrastructure) | JavaScript helper modules |
| [Tags & Navigation](/overview/component-taxonomy#8-utilities--infrastructure) | Tag management and directory generation |
| [Content Features](/overview/component-taxonomy#8-utilities--infrastructure) | Blog, X-of-the-day |
| [PHP Asset Pipeline](/overview/component-taxonomy#8-utilities--infrastructure) | CSS/JS bundling and asset generation |
| [Python Utilities](/overview/component-taxonomy#8-utilities--infrastructure) | LLM-based text processing tools |
| [Shell Utilities](/overview/component-taxonomy#8-utilities--infrastructure) | Embedding, archiving, uploading scripts |
| [HTML Templates](/overview/component-taxonomy#8-utilities--infrastructure) | Hakyll/Pandoc templates |
| [Server & Nginx](/overview/component-taxonomy#8-utilities--infrastructure) | Server configuration |

## Quick Links

| If you want to... | Start here |
|-------------------|------------|
| Understand the JS architecture | [initial.js](/frontend/initial-js) |
| Understand popups | [popups.js](/frontend/popups-js) |
| Understand annotations | [LinkMetadata.hs](/backend/link-metadata-hs) |
| Understand the build | [sync.sh](/backend/sync-sh) |
