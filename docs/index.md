---
slug: /
title: gwern.net Codebase Documentation
description: Unofficial technical documentation for the gwern.net Hakyll, Pandoc, Haskell, JavaScript, annotation, popup, and static-site architecture.
keywords:
  - gwern.net
  - Hakyll
  - Pandoc
  - Haskell
  - link annotations
  - hover popups
sidebar_position: 1
---

:::info Source Version
These docs are based on gwern.net commit [`406d3e423`](https://github.com/gwern/gwern.net/commit/406d3e423) (2026-06-20)
:::

# gwern.net Codebase Documentation

Unofficial technical documentation for the [gwern.net](https://gwern.net) codebase, developed by [Gwern Branwen](https://gwern.net/about) and [Said Achmiz](https://github.com/achmizs).

## Quick Start

| Want to... | Start here |
|------------|------------|
| Get oriented | [Architecture at a Glance](/overview/architecture-at-a-glance) |
| Follow a page through the full system | [Page Lifecycle](/overview/page-lifecycle) |
| Browse all files by function | [Functional Taxonomy](/overview/component-taxonomy) |

If you are new to the codebase, read the [architecture overview](/overview/architecture-at-a-glance) first, then use [page lifecycle](/overview/page-lifecycle) for the end-to-end flow and [functional taxonomy](/overview/component-taxonomy) as the map of individual files.

## Guided Topics

These are shorter paths through the larger reference when you already know what subsystem you care about.

| Topic | Start here | Covers |
|-------|------------|--------|
| Build pipeline | [Build Pipeline](/overview/build-pipeline) | Hakyll, Pandoc, generated content, validation, and deployment |
| Annotation metadata | [Annotation and Metadata](/overview/annotation-metadata) | GTX records, metadata scrapers, annotation fragments, and frontend loading |
| Popup system | [Popup System](/overview/popup-system) | Hover previews, mobile popovers, Extracts dispatch, transclusion, and annotation display |

## What is gwern.net?

gwern.net is a long-form essay site by pseudonymous writer Gwern Branwen, covering topics ranging from statistics and psychology to AI, genetics, and internet culture. The site has been actively maintained since January 2009 (~17 years) and is known for its rigorous, heavily-cited research essays that often run tens of thousands of words.

What makes gwern.net technically interesting is its sophisticated custom infrastructure. The site features hover popups that show link previews and citations without leaving the page, inline transclusion (embedding content from other pages), margin sidenotes, automatic bibliography generation, and a preemptive link archiving system that mirrors external links locally to prevent link rot.

The backend is built with [Hakyll](https://jaspervdj.be/hakyll/), a static site generator written in Haskell, combined with [Pandoc](https://pandoc.org/) for Markdown processing. Custom Haskell modules transform the content during build time, handling everything from typography fixes to annotation scraping. The frontend, written by [Said Achmiz](https://github.com/achmizs), is a custom vanilla JavaScript framework (no React, Vue, etc.) built around a pub/sub event system that coordinates popups, transclusion, theming, and dozens of other features.

## Why This Documentation?

Gwern himself has written extensively about the site's design and implementation. See [About This Website](https://gwern.net/about), [Design Graveyard](https://gwern.net/design-hierarchical), and [Typography](https://gwern.net/design-typography) for his own explanations. This documentation is meant to complement those writings by providing a structured reference to the actual source code.

I started this project out of sheer curiosity. I've been reading Gwern's site for a long time and always found his website-as-process approach to writing admirable, so I wanted to understand how it all worked under the hood.

The site's [codebase is available on GitHub](https://github.com/gwern/gwern.net). It's quite large and has been in active development by Gwern and Said Achmiz since January 2009 (~17 years). They're constantly updating the repo, so things change all the time. Thankfully, the code itself is filled with comments, which helped guide me as I tried to document everything.

My intent here is simply to make source code exploration easier and give curious readers a sense for how things work. These docs are also meant to serve as a map for any agents you might want to throw at the codebase, so you can ask questions yourself.

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
