---
sidebar_position: 0
---

# Architecture at a Glance

Every gwern.net article starts as a Markdown file. By the time a reader sees it in their browser, that file has been transformed into a richly interactive page with hover popups showing citation previews, automatic bibliographies, margin notes, and dozens of other enhancements.

This transformation happens in two distinct phases: **build time** (when the site is compiled) and **runtime** (when the page loads in your browser).

## The Two Phases

<div style={{textAlign: 'center'}}>

```
┌───────────────────────────────────────────────────────────┐
│                       Build Time                          │
│  Markdown → Pandoc AST → Haskell Transforms → HTML + JSON │
│  (sync.sh orchestrates: Hakyll, scrapers, validators)     │
└───────────────────────────────────────────────────────────┘
  ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓
┌───────────────────────────────────────────────────────────┐
│                        Runtime                            │
│  GW.notificationCenter → Content Events → Module Handlers │
│  (popups, transclusion, sidenotes, typography, theming)   │
└───────────────────────────────────────────────────────────┘
```

</div>

## Build Time: From Markdown to HTML

**(For a more detailed walkthrough following a single page from Markdown to rendered HTML, see [Page Lifecycle](/overview/page-lifecycle).)**

When the site is built, a master script called [sync.sh](/backend/sync-sh) coordinates everything in the following steps:

**1. Parsing**

The Markdown file is fed into [Pandoc](https://pandoc.org/), a document converter. Pandoc first creates an AST (Abstract Syntax Tree), a structured representation of the document that understands "this is a heading, this is a paragraph, this is a link" rather than just raw text.

**2. Transformation**

This is where the magic happens. The AST passes through dozens of custom Haskell modules, each one making specific enhancements:

- Adding metadata to links (title, author, date, abstract)
- Auto-linking certain terms to their definitions
- Fixing typography (smart quotes, proper dashes)
- Archiving external links locally to prevent link rot
- Generating citation IDs and bibliographies

**3. Output**

Finally, the transformed AST is converted to HTML. The build also generates JSON fragments containing annotation data that the browser will fetch later for popups.

**Key files:**
- [hakyll.hs](/backend/hakyll-hs) - The site generator that runs Pandoc and coordinates transforms
- [LinkMetadata.hs](/backend/link-metadata-hs) - Manages the database of link annotations
- [Typography.hs](/backend/typography-hs) - Handles text polish (title case, smart quotes)

## Runtime: Making Pages Interactive

Once the HTML reaches your browser, a custom JavaScript framework takes over. Unlike sites built with React or Vue, gwern.net uses a lightweight homegrown system centered around something called the "notification center."

**The Notification Center**

`GW.notificationCenter` is a messaging system that lets different parts of the code talk to each other without being directly connected. It uses a pattern called "pub/sub" (publish/subscribe): one piece of code publishes an event ("hey, new content just loaded!"), and other pieces that subscribed to that event respond ("great, I'll add popups to the links" or "I'll convert footnotes to sidenotes").

This keeps the code modular. The popup system doesn't need to know about the sidenote system, they just both listen for the same "content loaded" event.

**What Happens When a Page Loads**

1. The page fires a `GW.contentDidLoad` event
2. Various modules respond in phases:
   - **Transclude**: Embedded content from other pages is fetched and inserted
   - **Rewrite**: DOM transformations run (adding popup triggers, processing images, etc.)
   - **Event Listeners**: Interactive behaviors are attached (hover handlers, click handlers)

**Key files:**
- [initial.js](/frontend/initial-js) - Sets up the GW namespace and notification center
- [popups.js](/frontend/popups-js) - The hover popup system
- [transclude.js](/frontend/transclude-js) - Handles embedding content from other pages
- [rewrite.js](/frontend/rewrite-js) - Contains 80+ DOM transformation handlers

## Core Features

**Annotations**: Every link can have rich metadata attached (title, author, date, abstract, tags). During build time, scrapers fetch this information from sources like arXiv, Wikipedia, and PDFs. At runtime, this data appears in hover popups.

**Transclusion**: Pages can embed content from other pages inline. This Zettelkasten-inspired feature lets you reference related content without the reader leaving the page.

**Sidenotes**: Footnotes are converted to margin notes on wide screens, keeping annotations visible alongside the text they reference.

**Link Archiving**: External links are automatically mirrored locally. If the original page disappears, readers can still access a cached copy.

