
# LinkAuto.hs

**Path:** `build/LinkAuto.hs` | **Language:** Haskell | **Lines:** ~261

> Automatic hyperlink insertion for predefined terms, phrases, and citations

**Config:** `build/Config/LinkAuto.hs` | **Lines:** ~1,041

---

## Overview

LinkAuto transforms a Pandoc document by searching for predefined regex patterns (terms, names, concepts, citations) and converting the first occurrence of each match into a hyperlink. This automates the tedious work of manually linking common terms like "BERT", "GPT-3", or "reinforcement learning" throughout the site, particularly in auto-generated content like abstracts and annotations.

The system is designed for correctness over greed: regexes are sorted longest-first to prefer specific matches (e.g., "BigGAN" before "GAN"), delimited with word boundaries to avoid substring matches (e.g., "GAN" shouldn't match inside "StyleGAN"), and only the **first** occurrence of each term gets linked. Subsequent occurrences receive a `link-auto-skipped` class for debugging but render as plain text.

A multi-phase optimization pipeline makes this tractable on ~1,000+ regex patterns: the system compiles document text to plain text once, filters out patterns that can't possibly match, removes patterns whose target URLs already exist in the document (avoiding redundant links), and uses a divide-and-conquer binary search with master regexes to prune the pattern space before AST traversal.

---

## Public API

### `linkAuto :: Pandoc -> Pandoc`

Main entry point. Transforms a Pandoc document, inserting `link-auto` links for the first occurrence of each matched pattern.

**Called by:** `Typography.hs` (in the Pandoc AST pipeline), annotation processing
**Calls:** `linkAutoFiltered`, `customDefinitions`

### `linkAutoHtml5String :: String -> String`

Convenience wrapper for processing raw HTML strings. Parses HTML to Pandoc, runs `linkAuto`, renders back to HTML.

**Called by:** Annotation/metadata processing code
**Calls:** `linkAuto`, Pandoc read/write functions

### `linkAutoFiltered :: ([(T.Text, T.Text)] -> [(T.Text, T.Text)]) -> Pandoc -> Pandoc`

Runs LinkAuto with a custom filter on the pattern list. Allows removal of specific patterns or target URLs.

**Called by:** `linkAuto`, potentially custom processing
**Calls:** `filterMatches`, `filterDefinitions`, `defineLinks`, `annotateFirstDefinitions`, `cleanupNestedLinks`

### `linkAutoTest :: [([Inline], Pandoc, Pandoc)]`

Test suite: returns triples of (input, expected, actual) for any failing test cases. Empty list = all tests pass.

**Called by:** Test harness
**Calls:** `linkAutoInline2Doc`, references `C.linkAutoTests`

---

## Internal Architecture

### Processing Pipeline

```
Input Document
      │
      ▼
┌─────────────────────────────────────────┐
│  1. Extract plain text (simplifiedDoc)  │
│  2. Extract all existing URLs           │
└─────────────────────────────────────────┘
      │
      ▼
┌─────────────────────────────────────────┐
│  3. filterDefinitions: remove patterns  │
│     whose target URL already exists     │
│     (avoids redundant links)            │
└─────────────────────────────────────────┘
      │
      ▼
┌─────────────────────────────────────────┐
│  4. filterMatches: binary-search prune  │
│     patterns that can't match plain     │
│     text (O(log R) master regex tests)  │
└─────────────────────────────────────────┘
      │
      ▼
┌─────────────────────────────────────────┐
│  5. defineLinks: walk AST, replace Str  │
│     nodes with Link nodes on matches    │
└─────────────────────────────────────────┘
      │
      ▼
┌─────────────────────────────────────────┐
│  6. annotateFirstDefinitions: mark      │
│     first link with link-auto-first,    │
│     subsequent with link-auto-skipped   │
└─────────────────────────────────────────┘
      │
      ▼
┌─────────────────────────────────────────┐
│  7. cleanupNestedLinks: flatten any     │
│     accidentally nested <a><a></a></a>  │
└─────────────────────────────────────────┘
      │
      ▼
Output Document
```

### Key Data Structures

**Pattern Tuple:** `(T.Text, R.Regex, T.Text)`
- First: original regex string (for master regex construction)
- Second: compiled regex with word-boundary delimiters
- Third: target URL

**Config List:** `C.customSorted :: [(T.Text, T.Text)]`
- Sorted by regex length (descending) for longest-first matching
- ~1,000 entries covering ML terms, people, concepts, tools, etc.

### Regex Compilation

Patterns from Config are wrapped with POSIX character class delimiters:

```haskell
"[[:punct:][:blank:]]" `T.append` pattern `T.append` "[[:punct:][:blank:]]"
```

This ensures word-boundary matching: "GAN" matches " GAN," but not "StyleGAN".

---

## Key Patterns

### Longest-First Priority

Patterns are sorted by length to ensure specific terms match before general ones:

```haskell
customSorted = sortBy (\a b -> compare (T.length $ fst b) (T.length $ fst a)) custom
```

Example ordering: "DistilBERT" > "BERT" > "GAN"

### Binary-Search Filter Optimization

For documents >10k characters, `filterMatches` uses a divide-and-conquer approach:

```haskell
filterMatch :: Bool -> [(T.Text, R.Regex, T.Text)] -> [(T.Text, R.Regex, T.Text)]
filterMatch _ []  = []
filterMatch _ [d] = [d | matchTest (masterRegex [d]) plain]
filterMatch skipCheck ds
  | skipCheck = concatMap (filterMatch False . return) ds
  | not (matchTest (masterRegex ds) plain) = []  -- prune entire branch
  | otherwise = concatMap (filterMatch False . return) ds
```

This trades O(R x Nodes) for O(R + Nodes) with a master-regex prepass, yielding >30x speedup.

### First-Only Linking

A stateful pass tracks seen URLs and converts duplicate links to annotated spans:

```haskell
annotateFirstDefinitions :: Pandoc -> Pandoc
annotateFirstDefinitions doc = evalState (walkM addFirstDefn doc) S.empty
  where addFirstDefn (Link a@(_,classes,_) il c@(t,_))
          | "link-auto" `elem` classes = do
              st <- get
              if S.member t st
                then return $ addClass "link-auto-skipped" $ Span nullAttr il
                else do put (S.insert t st)
                        return $ addClass "link-auto-first" $ Span nullAttr [Link a il c]
```

### Self-Link Suppression

To disable auto-linking on a page (e.g., /modafinil shouldn't link "modafinil" to itself), add a hidden link:

```markdown
[null](/modafinil){.display-none} <!-- LinkAuto override: disable self-linking -->
```

The `filterDefinitions` pass sees this URL exists and removes the pattern.

---

## Configuration

### Config/LinkAuto.hs Pattern Categories

The ~1,000 patterns cover:

| Category | Examples | Count (approx) |
|----------|----------|----------------|
| ML/AI Models | GPT-3, BERT, StyleGAN, AlphaGo, MuZero | ~150 |
| ML Concepts | reinforcement learning, CNN, transformer | ~100 |
| People | Gwern topics (researchers, authors, historical) | ~200 |
| Statistics | GWAS, meta-analysis, Bayesian, regression | ~80 |
| Genetics | SNP, polygenic score, heritability | ~50 |
| Drugs/Nootropics | modafinil, caffeine, nicotine | ~40 |
| Anime/Media | Evangelion, Cowboy Bebop, Studio Ghibli | ~60 |
| Software/Tech | Bitcoin, Tor, Git, Wikipedia | ~100 |
| Academic | arXiv, citation formats like "Brown et al 2020" | ~50 |
| Misc Concepts | efficient markets, Fermi estimate | ~170 |

### Pattern Format

```haskell
("(regex|alternation)", "https://target-url.com/")
```

Common regex features used:
- Alternation: `(GPT-3|Brown et al 2020)`
- Case flexibility: `[Rr]einforcement [Ll]earning`
- Optional parts: `(Alpha ?Zero|Alpha0)`
- Character classes: `[Aa]synchronous`

### Size Limit

A hard cap prevents runaway compile times:

```haskell
customDefinitions subsetter =
  if length C.customSorted > 1007
  then error "LinkAuto.hs: 'C.customSorted' too long..."
  else customDefinitionsR $ definitionsValidate $ subsetter C.customSorted
```

### Validation

Uniqueness constraints are enforced at compile time:

```haskell
definitionsValidate defs
  | nubOrd (map fst defs) /= map fst defs = error "...keys not unique..."
  | nubOrd (map snd defs) /= map snd defs = error "...values not unique..."
```

---

## Integration Points

### Dependencies

- **Utils.hs:** `addClass`, `frequency`, `simplifiedDoc`, `safeHtmlWriterOptions`
- **Query.hs:** `extractURLs` (for existing-URL detection)
- **Typography.hs:** `mergeSpaces` (normalize whitespace before matching)
- **Config/LinkAuto.hs:** Pattern definitions

### CSS Classes Applied

| Class | Meaning |
|-------|---------|
| `link-auto` | Base class for all auto-generated links |
| `link-auto-first` | First occurrence of a term (gets displayed) |
| `link-auto-skipped` | Subsequent occurrences (usually hidden) |

### Where in Pipeline

LinkAuto should run:
- **After** interwiki link expansion (so `[foo](!W)` becomes a real URL first)
- **Before** most other transforms (to catch raw text before formatting changes it)

### Known Limitations

1. **Header nodes:** Links in headers break HTML validation (known bug)
2. **RawInline/RawBlock:** Not processed; raw HTML bypasses the system
3. **Split formatting:** A term split across multiple Inline nodes won't match
   - `Str "reinforcement", Emph [Str "learning"]` won't match "reinforcement learning"
   - `Emph [Str "reinforcement learning"]` will match (all inside one node)

---

## See Also

- [Config.LinkAuto](/backend/config-link-auto-hs) - Regex patterns and URL mappings for auto-linking
- [Interwiki.hs](/backend/interwiki-hs) - Expands interwiki links generated by LinkAuto
- [Typography.hs](/backend/typography-hs) - Pandoc AST pipeline where LinkAuto is invoked
- [hakyll.hs](/backend/hakyll-hs) - Build system that runs the LinkAuto transform
- [Annotation.hs](/backend/annotation-hs) - Annotations often processed through linkAutoHtml5String
- [rewrite.js](/frontend/rewrite-js) - Client-side equivalent for dynamic content
