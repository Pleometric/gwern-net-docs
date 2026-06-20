# .hlint.yaml

**Path:** `build/.hlint.yaml` | **Language:** YAML | **Lines:** 47

> HLint configuration for gwern.net's Haskell build code.

---

## Overview

`build/.hlint.yaml` configures HLint checks for the Haskell build tree. It records ignored hints and local lint preferences so automated linting matches the conventions used by the gwern.net build scripts and modules.

The file is part of the build tooling rather than the site runtime. It should be read together with Haskell modules such as [Test.hs](/backend/test-hs) and [sync.sh](/backend/sync-sh), which coordinate checks during local development or full synchronization.

