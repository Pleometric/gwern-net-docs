---
title: gwern.net Popup System Hub
description: Start here for gwern.net's popup and popover system, including Extracts, Popups, Popovers, annotation rendering, transclusion, and content handlers.
keywords:
  - gwern.net popups
  - hover previews
  - popovers
  - Extracts.js
  - transclusion
sidebar_position: 4
---

# Popup System Hub

Start here to understand the runtime system that displays hover previews, citation annotations, local-page embeds, and mobile popovers.

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

- [Annotation and Metadata Hub](/overview/annotation-metadata) explains the backend data that powers citation previews.
- [pop-frame-title-standard](/templates/pop-frame-title-standard) defines the standard title bar template.
- [annotation-blockquote-inside](/templates/annotation-blockquote-inside) and [annotation-blockquote-outside](/templates/annotation-blockquote-outside) define annotation display fragments.
