---
sidebar_position: 3
---

# unfortunatelytheclockisticking.html

**Path:** `template/unfortunatelytheclockisticking.html` | **Language:** HTML | **Lines:** ~168

Easter egg template featuring the "Birthday Cat" mortality reminder meme with elaborate image map.

## Overview

This template is an elaborate easter egg featuring the "Unfortunately, the clock is ticking" meme - a birthday cat image combined with a Windows notification-style dialog about mortality, the passage of time, and diminishing possibilities. The template uses HTML image maps extensively to create a rich interactive experience where nearly every part of the image links to relevant content, often with humorous or existential commentary.

The meme's core message is memento mori: reminding users that time is passing, opportunities are finite, and procrastination has real costs. Like its sibling template `idealconditionsdonotexistandwillneverhappen.html`, this is designed to be shown as a popup or easter egg, potentially triggered after periods of browsing inactivity.

What makes this template exceptional is its density of interactivity - it includes 100+ clickable areas with randomized link destinations, philosophical commentary, memes, and self-referential humor. The sofa alone has 65 randomized link targets. This represents Gwern's characteristic style of layering depth and humor into seemingly simple elements.

## Key Variables/Blocks

This template contains no Hakyll/Pandoc variables - it's pure static HTML with extensive JavaScript-like features through the `display-random-1` class system.

### Major Image Map Regions

#### Dialog Box Text (Sequential top-to-bottom)

- **"Unfortunately,"** → Clock of the Long Now
- **"The clock is ticking"** → Clock of the Long Now
- **"the hours are going by."** → Site subscript (Big Now)
- **"The past increases,"** → `/changelog` (site change history)
- **"the future recedes."** → Vernor Vinge's Technological Singularity essay
- **"Possibilities decreasing,"** → "Your Life in Weeks" (Wait But Why)
- **"regrets"** → `/sunk-cost`
- **"mounting."** → `/screwfly` (on lordosis)
- **"Do you understand?"** → `/doc/psychology/cognitive-bias/illusion-of-depth/index`

#### Interactive Elements

- **Cat** (randomized, 2 options): Links to `/review/cat#are-cats-domesticated` or 10-hour Cat video
- **Candle** → "Do not go gentle into that good night" (Dylan Thomas poem)
- **Cat food can** → "It Was All For The Tuna" (indie game)
- **Pillow/Space Invader** → Space Invaders Google Easter Egg

#### Dialog Box Buttons

**"I Understand" button** (randomized, 14 options):
- `/archiving`
- `/search`
- `/doc/index`
- Dhammapada (Wikisource)
- `/subculture`
- "Meditations on Moloch" (Slate Star Codex)
- "The Tail End" (Wait But Why)
- Hamming's "You and Your Research"
- David Foster Wallace "This Is Water"
- Randy Pausch "Last Lecture"
- Why the Lucky Stiff retrospective
- Paul Graham "Life is Short"
- Bertrand Russell "In Praise of Idleness"
- Seneca "On the Shortness of Life"

**"remain ignorant" button** (randomized, 17 options):
- `/404`
- Rickroll
- "Dare To Be Stupid" (Weird Al)
- Classic internet memes (All Your Base, Hampster Dance, Badger Badger, Nyan Cat, Zombo.com, YTMND, etc.)
- Wikipedia Random Article
- Neal.fun Internet Artifacts

**Close "X" button** (randomized, 9 options):
- Redirects to `/index` or `/404`
- Links to `about:blank`
- Does nothing (disabled link)
- "It was me, Dio!" image (JoJo's Bizarre Adventure meme)
- Microsoft Sydney chatbot reference (ominous threatening text)
- Evil Jinni: Hides image but leaves popup (malicious compliance)
- Good Jinni: Actually closes popup properly

#### The Sofa (90+ randomized links)

This region contains an absurd variety of links with deadpan commentary, including:
- Philosophy (Default Mode Network, Oblomovism, Thrownness, Heidegger, Shakespeare)
- Science (Tardigrades, house dust mites, microbiota, entropy, friction coefficients)
- Meta-humor (Simulation hypothesis, texture resolution, SCP Foundation)
- Procrastination themes (Temporal discounting, FOMO, hedonic treadmill, nostalgia)
- Internet culture (XKCD comics, "touch grass" meme, 404 page, `/everything`)
- Self-referential jokes (Debug coordinates, image map hover time tracking)

### CSS Classes

- `image-focus-not`: Prevents image zoom behavior
- `invert-not`: Prevents dark mode inversion
- `display-random-1`: Enables randomization of child `display-entry` elements
- `disable-the-not-chosen`: Hides non-selected randomized options

## Usage

Like `idealconditionsdonotexistandwillneverhappen.html`, this is an easter egg template not used in standard Hakyll compilation. Potential uses:

1. **Random popup trigger**: JavaScript could randomly display this as a popup after X minutes of browsing
2. **Manual link**: Secret links on certain pages transclude this template
3. **Idle detection**: Shown when user has been inactive for a long period
4. **Special occasions**: Birthday-themed content shown on user's birthday (if detectable)

Hypothetical JavaScript trigger:
```javascript
// After 15 minutes of inactivity, 5% chance to show
if (idleTime > 900000 && Math.random() < 0.05) {
    Popups.spawnPopup('/static/template/unfortunatelytheclockisticking.html');
}
```

The randomization system depends on JavaScript initialized in `rewrite.js` or similar, which processes `display-random-1` classes to randomly select one child `display-entry` element and hide the others.

---

## See Also

- [idealconditionsdonotexistandwillneverhappen.html](/templates/idealconditionsdonotexistandwillneverhappen) - Sister template (Clippy/C.S. Lewis quote)
- [default.html](/templates/default) - Main page template (this easter egg bypasses it)
- [popups.js](/frontend/popups-js) - Popup system that may display this
- [rewrite.js](/frontend/rewrite-js) - DOM transformation that handles randomization
- [transclude.js](/frontend/transclude-js) - Content transclusion for embedding
- [gwern.net.conf](/nginx/gwern-net-conf) - Server config that serves the easter egg
