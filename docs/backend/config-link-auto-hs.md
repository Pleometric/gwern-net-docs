
# Config.LinkAuto

**Path:** `build/Config/LinkAuto.hs` | **Language:** Haskell | **Lines:** ~1,041

> Configuration data for the auto-linking system: regex patterns and URL mappings

---

## Overview

Config.LinkAuto is a pure data module that defines the dictionary of regular expression patterns and their corresponding URLs for gwern.net's auto-linking system. When the site is compiled, the LinkAuto module scans documents for text matching these patterns and automatically converts the first occurrence of each match into a hyperlink.

The module contains approximately 1,000 regex→URL pairs covering technical terms (AI/ML model names like "GPT-3", "BERT", "StyleGAN"), statistical concepts ("GWAS", "MCMC", "PGS"), notable people (researchers, authors, historical figures), media (anime, books, websites), and scientific terminology (genetics, psychology, neuroscience). Each pattern uses Haskell's regex-tdfa syntax with alternations and character classes to match multiple surface forms of the same concept.

The patterns are sorted by length in descending order at compile time (`customSorted`) so that longer, more specific matches take priority over shorter generic ones—"BigGAN" matches before "GAN", and "reinforcement learning" matches before "learning". This design prevents greedy short patterns from capturing text that should be linked to more specific definitions.

---

## Public API

### `custom :: [(T.Text, T.Text)]`

The raw list of (regex, URL) pairs defining auto-link patterns.

**Called by:** `LinkAuto.customDefinitions`, `Test.hs` (validation)
**Calls:** (pure data)

### `customSorted :: [(T.Text, T.Text)]`

The same list as `custom`, sorted by descending pattern length for priority matching.

**Called by:** `LinkAuto.customDefinitions`
**Calls:** `Data.List.sortBy`, `Data.Text.length`

### `linkAutoTests :: [([Inline], Pandoc)]`

Test cases pairing input Inline fragments with expected Pandoc output after auto-linking.

**Called by:** `LinkAuto.linkAutoTest`
**Calls:** (pure data using Pandoc constructors)

---

## Internal Architecture

### Pattern Structure

Each entry is a 2-tuple of `(regex, url)`:

```haskell
("(GPT-3|Brown[  ]et[  ]al[  ]2020)", "https://arxiv.org/abs/2005.14165#openai")
```

Patterns use regex-tdfa syntax:
- `(A|B|C)` — alternation for synonyms/variants
- `[Xx]` — case-insensitive character class
- `.?` — optional character (often for plurals)
- `[ -]` — space or hyphen variants
- `\\. ` — escaped literal periods

URLs can be:
- Absolute Wikipedia/arXiv/external URLs
- Site-relative paths like `/modafinil` or `/doc/...`

### Sorting Logic

```haskell
customSorted = sortBy (\a b -> compare (T.length $ fst b) (T.length $ fst a)) custom
```

Longest patterns first ensures specificity. Without this, "GAN" would match inside "BigGAN" before "BigGAN" gets a chance.

### Test Data

The `linkAutoTests` list contains regression tests as `([Inline], Pandoc)` pairs:
1. Input: Pandoc Inline fragments (Str, Link, Space, etc.)
2. Expected: Complete Pandoc document after auto-linking

Tests verify:
- Links inside existing Link nodes are NOT auto-linked (would create invalid nested links)
- First occurrence only gets linked
- Multiple distinct terms in same text all get their respective links
- Case sensitivity behaves as specified

---

## Key Patterns

### Pattern Categories

| Category | Examples | Count (approx) |
|----------|----------|----------------|
| ML/AI models | GPT-3, BERT, StyleGAN, AlphaGo, MuZero | ~150 |
| Statistics | GWAS, PGS, MCMC, Bayesian, regression | ~100 |
| People | Borges, Feynman, Nietzsche, von Neumann | ~150 |
| Genetics | SNP, QTL, heritability, epistasis | ~75 |
| Psychology | Big Five, ADHD, schizophrenia | ~50 |
| Computing | Bitcoin, Tor, Emacs, Haskell | ~75 |
| Anime/Media | Evangelion, Cowboy Bebop, Gainax | ~50 |
| Misc concepts | Efficient market, Fermi problem | ~100+ |

### Pattern Design Considerations

**1. Variant Coverage**
```haskell
("(ADHD|[Aa]ttention[ -][Dd]eficit [Hh]yperactivity [Dd]isorder)s?", "...")
```
Matches "ADHD", "attention-deficit hyperactivity disorder", "Attention Deficit Hyperactivity Disorder", etc.

**2. Disambiguation by Specificity**
```haskell
("StyleGAN2-ADA.?|Karras et al 2020", "...2006.06676...")
("(StyleGANs?|CelebA[ -]HQ|FFHQ)", "...1812.04948...")
("StyleGAN2s?", "...1912.04958...")
```
Multiple StyleGAN entries, each mapping to the correct paper.

**3. Citation Patterns**
```haskell
("(GPT-3|Brown[  ]et[  ]al[  ]2020)", "https://arxiv.org/abs/2005.14165#openai")
```
Matches both the common name and the academic citation format.

---

## Configuration

### Performance Warning

The file contains a hard limit check in `LinkAuto.hs`:

```haskell
if length C.customSorted > 1007 then error "..."
```

Too many patterns cause exponential slowdown. The comment in the config notes:
> "From now on, delete at least one rewrite for every added rewrite."

### Adding New Patterns

1. Add tuple to `custom` list
2. Ensure pattern is unique enough (won't match inside other terms inappropriately)
3. Use longest/most-specific form as the base
4. Include common variants via alternation
5. Consider removing an obsolete entry to stay under the limit

---

## Integration Points

### Used By

- **[LinkAuto](link-auto-hs)** — Runtime consumer that applies patterns to documents
- **[Test.hs](test-hs)** — Validates all patterns are unique and URLs are well-formed

### Validation

Test.hs performs several checks:
1. `isUniqueAll` — No duplicate regex patterns
2. `isURLAnyT` — All URLs pass basic URL validation
3. Regex compilation test — All patterns compile successfully

### URL Formats

The URLs use gwern.net conventions:
- `#openai`, `#deepmind`, `#google` — Organization tag fragments for arXiv links
- `/doc/...` — Local document paths
- `/modafinil`, `/nicotine` — Short-URL essay references
- Wikipedia/external links for general concepts

---

## See Also

- [LinkAuto.hs](/backend/link-auto-hs) - The module that applies these patterns
- [Interwiki.hs](/backend/interwiki-hs) - Expands interwiki links generated by auto-linking
- [Config.Interwiki](/backend/config-interwiki-hs) - Wikipedia redirect mappings
- [Typography.hs](/backend/typography-hs) - Text transformations that run before/after auto-linking
- [hakyll.hs](/backend/hakyll-hs) - Build system that orchestrates transforms
- [Test.hs](/backend/test-hs) - Validation of pattern correctness
