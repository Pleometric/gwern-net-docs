# gwernnet.cabal

**Path:** `build/gwernnet.cabal` | **Language:** Cabal | **Lines:** ~177

Cabal build configuration defining the gwern.net Haskell library and executables.

---

## Overview

gwernnet.cabal is the central build configuration for all Haskell code in the gwern.net build system. It defines:

- A shared library (`gwernnet`) containing 50+ modules for site generation
- 9 executable tools for various build and maintenance tasks
- Common dependencies and compiler options shared across all components

The configuration uses Cabal 3.0 features like common stanzas to reduce duplication and ensure consistent settings across all build targets.

---

## Package Metadata

```cabal
cabal-version: 3.0
name:          gwernnet
version:       2026.1.21.0
license:       CC0-1.0
author:        Gwern Branwen
maintainer:    Gwern Branwen
build-type:    Simple
synopsis:      Gwern.net build + maintenance tools (internal)
category:      Web
```

The version follows a date-based scheme: `YYYY.M.DD.patch`

---

## Library Modules

The library exposes 50+ modules organized by function:

### Core Build
| Module | Purpose |
|--------|---------|
| `Annotation` | Main annotation/metadata system |
| `LinkMetadata` | Link metadata processing |
| `LinkMetadataTypes` | Type definitions for metadata |
| `Typography` | Pandoc typography transforms |
| `Utils` | Common utilities |

### Annotation Scrapers
| Module | Purpose |
|--------|---------|
| `Annotation.Arxiv` | arXiv paper metadata |
| `Annotation.Biorxiv` | bioRxiv/medRxiv metadata |
| `Annotation.Gwernnet` | Internal site metadata |
| `Annotation.OpenReview` | OpenReview paper metadata |
| `Annotation.PDF` | PDF document metadata |

### Link Processing
| Module | Purpose |
|--------|---------|
| `LinkArchive` | Archive.org integration |
| `LinkAuto` | Automatic link annotation |
| `LinkBacklink` | Backlink generation |
| `LinkID` | Link identification |
| `LinkIcon` | Link type icons |
| `LinkLive` | Live preview popups |

### Text Processing
| Module | Purpose |
|--------|---------|
| `Paragraph` | Paragraph transforms |
| `Columns` | List column layout |
| `Inflation` | Dollar inflation adjustment |
| `Text.Regex` | Vendored regex module |

### Metadata Handling
| Module | Purpose |
|--------|---------|
| `Metadata.Author` | Author name processing |
| `Metadata.Date` | Date parsing/formatting |
| `Metadata.Format` | Format detection |
| `Metadata.Title` | Title processing |

### Configuration Modules
All `Config.*` modules contain constants, test cases, and whitelists:
- `Config.GenerateSimilar`, `Config.Inflation`, `Config.Interwiki`
- `Config.LinkArchive`, `Config.LinkAuto`, `Config.LinkID`
- `Config.LinkIcon`, `Config.LinkLive`, `Config.LinkSuggester`
- `Config.Metadata.Author`, `Config.Metadata.Format`, `Config.Metadata.Title`
- `Config.Misc`, `Config.Paragraph`, `Config.Tags`, `Config.Typography`
- `Config.XOfTheDay`

### Other Modules
| Module | Purpose |
|--------|---------|
| `Blog` | Blog post generation |
| `Cycle` | Cycle detection utilities |
| `GenerateSimilar` | Similar link recommendations |
| `GTX` | GTX file handling |
| `Image` | Image processing |
| `Interwiki` | Interwiki link expansion |
| `Query` | Database queries |
| `Tags` | Tag system |
| `Test` | Test utilities |
| `Unique` | Unique ID generation |
| `XOfTheDay` | Daily featured content |

---

## Executables

### hakyll

Main site generator using Hakyll static site framework.

```cabal
executable hakyll
  main-is: hakyll.hs
  build-depends: gwernnet
  ghc-options: -threaded -rtsopts
```

### preprocess-markdown

Markdown preprocessing before Pandoc.

```cabal
executable preprocess-markdown
  main-is: preprocess-markdown.hs
  build-depends: gwernnet
```

### generateLinkBibliography

Creates bibliography pages from link metadata.

```cabal
executable generateLinkBibliography
  main-is: generateLinkBibliography.hs
  ghc-options: -threaded -rtsopts
```

### generateDirectory

Generates directory/index pages.

```cabal
executable generateDirectory
  main-is: generateDirectory.hs
  ghc-options: -threaded -rtsopts
```

### generateBacklinks

Creates backlink pages showing what links to each page.

```cabal
executable generateBacklinks
  main-is: generateBacklinks.hs
  ghc-options: -threaded -rtsopts
```

### checkMetadata

Validates metadata consistency.

```cabal
executable checkMetadata
  main-is: checkMetadata.hs
```

### guessTag

Suggests tags for content.

```cabal
executable guessTag
  main-is: guessTag.hs
```

### changeTag

Batch tag modification.

```cabal
executable changeTag
  main-is: changeTag.hs
  ghc-options: -threaded -rtsopts
```

### generateSimilarLinks

Creates "similar links" recommendations.

```cabal
executable generateSimilarLinks
  main-is: generateSimilarLinks.hs
```

---

## Common Stanza

All components share settings via the `gwern-common` stanza:

```cabal
common gwern-common
  default-language: Haskell2010
  hs-source-dirs: .

  ghc-options:
      -O2
      -Wall
      -Wno-missing-home-modules

  build-depends:
      base, HTTP, aeson, array, async, ...
```

### Compiler Options

| Option | Purpose |
|--------|---------|
| `-O2` | Optimization level 2 |
| `-Wall` | Enable all warnings |
| `-Wno-missing-home-modules` | Suppress module location warnings |
| `-threaded` | Enable threading (some executables) |
| `-rtsopts` | Enable runtime options (some executables) |

---

## Key Dependencies

### Core
| Package | Purpose |
|---------|---------|
| `pandoc==3.1.3` | Document conversion |
| `pandoc-types` | Pandoc AST types |
| `hakyll==4.16.2.0` | Static site generator |

### Web/Network
| Package | Purpose |
|---------|---------|
| `HTTP` | HTTP client |
| `http-conduit==2.3.8` | Streaming HTTP |
| `http-types` | HTTP type definitions |
| `network-uri` | URI parsing |

### Data/Text
| Package | Purpose |
|---------|---------|
| `aeson` | JSON parsing |
| `text` | Text type |
| `bytestring` | Binary data |
| `containers` | Maps, sets |
| `vector` | Efficient arrays |

### Regex
| Package | Purpose |
|---------|---------|
| `regex-base` | Regex abstraction |
| `regex-tdfa` | TDFA regex engine |

### Other
| Package | Purpose |
|---------|---------|
| `titlecase` | Title case conversion |
| `tagsoup` | HTML parsing |
| `edit-distance` | String similarity |
| `rp-tree` | Similarity search |
| `cryptohash` | Hashing |

---

## Version Pins

Some dependencies have exact version pins to ensure reproducible builds:

```cabal
hakyll==4.16.2.0
http-conduit==2.3.8
pandoc==3.1.3
```

---

## Building

```bash
cd build/
cabal build all          # Build everything
cabal build hakyll       # Build just hakyll
cabal run hakyll -- ...  # Run with arguments
```

---

## See Also

- [hakyll.hs](/backend/hakyll-hs) - Main site generator
- [sync.sh](/backend/sync-sh) - Build orchestration script
- [Text.Regex](/backend/text-regex-hs) - Vendored regex module
- [LinkMetadata.hs](/backend/link-metadata-hs) - Core metadata system
