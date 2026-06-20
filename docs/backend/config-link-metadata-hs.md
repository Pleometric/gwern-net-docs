---
title: "Config.LinkMetadata"
description: "Configuration constants and test fixtures for LinkMetadata behavior."
---

# Config.LinkMetadata

Configuration constants and test fixtures for LinkMetadata behavior.

<div className="doc-meta">
  <div><strong>Path</strong><code>build/Config/LinkMetadata.hs</code></div>
  <div><strong>Language</strong>Haskell</div>
  <div><strong>Lines</strong>141</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/Config/LinkMetadata.hs">build/Config/LinkMetadata.hs</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing the Haskell build pipeline, generators, metadata code, or backend utility behavior around Config.LinkMetadata.
</div>

## Overview

`Config.LinkMetadata` centralizes constants that were split out of the main link metadata implementation. It defines annotation CSS classes, prefetch limits, metadata validation strings, URL prefix exceptions, and file-transclusion test fixtures.

The module is consumed by link metadata code that needs shared thresholds and lists without embedding those details directly in the larger `LinkMetadata.hs` implementation.

## Key Constants

- `annotationClasses` and `positiveAnnotationClasses` define the annotation-related CSS classes used by link metadata output.
- `maxPrefetchBytes` limits prefetching for large URLs.
- `partialAnnotationIgnoredTagCount`, `partialAnnotationBacklinkThreshold`, and `partialAnnotationSimilarThreshold` tune partial-annotation scoring.
- `annotationURLWarningLength`, `annotationURLPreviewLength`, and `missingTitleAbstractMinLength` set deterministic thresholds used by metadata checks/output.
- `futureYearSlack` allows a small number of future years before dates are considered suspicious.
- URL/file lists such as `documentPreviewableExtensions`, `codePreviewableExtensions`, and `fileViewableExtensions` drive file-transclusion behavior.

## Tests

`fileTranscludesTest` builds expected Pandoc blocks for representative local files, external archive URLs, image/audio/video assets, code files, and ignored archive formats. It is parameterized by the file-transclusion function so the tests can exercise the production renderer.

