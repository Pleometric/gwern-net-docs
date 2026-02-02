---
sidebar_position: 2
---

# idealconditionsdonotexistandwillneverhappen.html

**Path:** `template/idealconditionsdonotexistandwillneverhappen.html` | **Language:** HTML | **Lines:** ~9

Easter egg template featuring a Clippy-style motivational meme about perfectionism.

## Overview

This template is a humorous anti-procrastination easter egg that can be displayed to users as a random popup or notification. It presents a Microsoft Clippy-inspired dialog box with a C. S. Lewis quote about not waiting for ideal conditions, reminding readers that "ideal conditions do not exist and will never happen."

The template uses an HTML image map to make different parts of the image clickable, creating an interactive experience similar to old Windows dialog boxes. Users can click different areas to access resources about procrastination, dismiss the notification, or acknowledge the message. This is intended to be popped up randomly after a period of inactivity or activity as a playful nudge against perfectionism and procrastination.

The design is minimal and self-contained, consisting solely of a `<figure>` element with an image and mapped clickable regions. It's designed to be transcluded into other pages or shown in popups, not used as a standalone page template.

## Key Variables/Blocks

This template has no Hakyll/Pandoc template variables - it's pure static HTML with hardcoded content.

### Image Map Areas

- **Main quote area** (coords: 34,113,822,470): Links to the C. S. Lewis essay "Learning in War-Time" (1939 PDF)
  - Alt text: "Quote about not procrastinating due to perfectionism."
  - Title: "'Learning in War-Time', C. S. Lewis 1939"

- **"I'm aware" button** (coords: 43,483,414,579): Links to LessWrong article on beating procrastination
  - Destination: `https://www.lesswrong.com/posts/RWo4LwFzpHNQCTcYt/how-to-beat-procrastination`
  - Title: "Yes, I know. Trying to fix it."
  - Alt: "Link to LessWrong article on dealing with akrasia ['How To Beat Procrastination', Luke Muehlhauser 2011-02-05]"

- **"Wow, rude" button** (coords: 43,583,416,674): Dismisses notification (JavaScript void)
  - Alt: "Dismiss notification"
  - Title: "I choose to remain in denial and wait for ideal circumstances to do the thing."

### Source Image

- Path: `/doc/fiction/humor/2024-11-10-bpdohwhatajoy-tumblr-clippymeme-idealconditionsdonotexistandwillneverhappen-766797325352960000.jpg`
- Alt text: "[MICROSOFT CLIPPY:] Hey, it looks like you're waiting for ideal conditions to do that thing you've been wanting to do. Need I remind you that ideal conditions do not exist and will never happen? / [I'm aware] / [Wow, rude]"
- Title: "Ideal conditions do not exist and will never happen. —C. S. Lewis"
- Class: `image-focus-not` (prevents image from triggering focus/zoom behavior)

## Usage

This template is not used in the standard Hakyll build process. Instead, it's likely:

1. **Transcluded into pages**: Used with the transclusion system (see `transclude.js`) to embed as a snippet
2. **Popup content**: Loaded dynamically as popup content via the `popups.js` system
3. **Easter egg trigger**: Possibly triggered by inactivity detection or random chance after user has been on site for a certain duration

Example transclusion (hypothetical):
```html
<a href="/static/template/idealconditionsdonotexistandwillneverhappen.html"
   class="include-annotation">Motivational Clippy</a>
```

The template shares thematic similarities with `unfortunatelytheclockisticking.html` - both are existential reminder memes that can be used as easter eggs or motivational interrupts.

---

## See Also

- [unfortunatelytheclockisticking.html](/templates/unfortunatelytheclockisticking) - Similar easter egg template (birthday cat mortality reminder)
- [default.html](/templates/default) - Main page template (this easter egg bypasses it)
- [transclude.js](/frontend/transclude-js) - Content transclusion system for embedding
- [popups.js](/frontend/popups-js) - Popup windowing system that may display this
- [rewrite.js](/frontend/rewrite-js) - DOM transformation that handles randomization
- [gwern.net.conf](/nginx/gwern-net-conf) - Server config that serves the easter egg
