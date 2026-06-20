---
title: Popup System
description: A guided path through Extracts, Popups, Popovers, annotation rendering, transclusion, and content handlers.
sidebar_position: 4
---

# Popup System

This is the runtime path through hover previews, citation annotations, local-page embeds, and mobile popovers. Read [Architecture at a Glance](/overview/architecture-at-a-glance) first if you want the build/runtime model, or [Annotation and Metadata](/overview/annotation-metadata) first if you want the data that powers citation previews.

## Reading Path

| Step | Page | Why it matters |
|------|------|----------------|
| 1 | [initial.js](/frontend/initial-js) | Defines the global event system and startup conventions |
| 2 | [extracts.js](/frontend/extracts-js) | Coordinates provider selection, content filling, titles, and type-specific handlers |
| 3 | [popups.js](/frontend/popups-js) | Desktop hover popup provider |
| 4 | [popovers.js](/frontend/popovers-js) | Mobile/click popover provider |
| 5 | [extracts-content.js](/frontend/extracts-content-js) | Content handlers for local pages, annotations, backlinks, and widgets |

## Related Runtime Pieces

| Area | Start here |
|------|------------|
| Annotation data | [annotations.js](/frontend/annotations-js) |
| Annotation-specific extracts | [extracts-annotations.js](/frontend/extracts-annotations-js) |
| Content transclusion | [transclude.js](/frontend/transclude-js) |
| DOM rewriting | [rewrite.js](/frontend/rewrite-js) |
| Options and user settings | [extracts-options.js](/frontend/extracts-options-js) |

## Template and Backend Inputs

- [Annotation and Metadata](/overview/annotation-metadata) explains the backend data that powers citation previews.
- [pop-frame-title-standard](/templates/pop-frame-title-standard) defines the standard title bar template.
- [annotation-blockquote-inside](/templates/annotation-blockquote-inside) and [annotation-blockquote-outside](/templates/annotation-blockquote-outside) define annotation display fragments.
- [Functional Taxonomy](/overview/component-taxonomy#3-popup-system) lists the broader popup-related file set.
