---
title: ".hlint.yaml"
description: "HLint configuration for gwern.net's Haskell build code."
---

# .hlint.yaml

HLint configuration for gwern.net's Haskell build code.

<div className="doc-meta">
  <div><strong>Path</strong><code>build/.hlint.yaml</code></div>
  <div><strong>Language</strong>YAML</div>
  <div><strong>Lines</strong>47</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/.hlint.yaml">build/.hlint.yaml</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the Haskell build pipeline, generators, metadata code, or backend utility behavior around .hlint.
</div>

## Overview

`build/.hlint.yaml` configures HLint checks for the Haskell build tree. It records ignored hints and local lint preferences so automated linting matches the conventions used by the gwern.net build scripts and modules.

The file is part of the build tooling rather than the site runtime. It should be read together with Haskell modules such as [Test.hs](/backend/test-hs) and [sync.sh](/backend/sync-sh), which coordinate checks during local development or full synchronization.

