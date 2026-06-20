---
title: "download-title.sh"
description: "This script is a simple utility that fetches a URL and parses its HTML to extract the element text."
sidebar_position: 1
---

# download-title.sh

This script is a simple utility that fetches a URL and parses its HTML to extract the element text.

<div className="doc-meta">
  <div><strong>Path</strong><code>build/download-title.sh</code></div>
  <div><strong>Language</strong>Bash</div>
  <div><strong>Lines</strong>53</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/download-title.sh">build/download-title.sh</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing shell automation, compression, upload/download helpers, linting, or preprocessing around download-title.
</div>

## Overview

This script is a simple utility that fetches a URL and parses its HTML to extract the `<title>` element text. It's used during the build process to automatically generate human-readable titles for linked resources, particularly when creating annotations or metadata entries.

The script handles typical web page quirks like HTML entity encoding and whitespace normalization, returning clean title text suitable for use in metadata databases. It uses a Perl-based HTML parser for robust extraction, uses `curl` for fetching, and only checks for `xmllint` availability (it does not perform xmllint validation).

## Key Commands/Variables

- **`extract_title()`**: Main function that fetches URL, parses HTML, decodes entities, and extracts title
- **`curl --max-filesize 100000000 --silent --location`**: Downloads URL content with 100MB size limit
- **`HTML::TreeBuilder`**: Perl module for parsing HTML DOM tree
- **`HTML::Entities::decode_entities`**: Decodes HTML entities like `&amp;` to `&`
- **`timeout 20s`**: Limits download time to 20 seconds per URL
- **`TEMP_FILE`**: Temporary file for storing downloaded HTML

## Usage

```bash
./download-title.sh <URL> [URL2] [URL3] ...
```

**Arguments:**
- One or more URLs to fetch titles from

**Examples:**
```bash
$ ./download-title.sh "http://catb.org/~esr/writings/taoup/html/ch05s01.html"
The Importance of Being Textual

$ ./download-title.sh https://blog.nationalmuseum.ch/en/2024/06/the-dream-of-an-alpine-waterway/
The dream of an alpine waterway – Swiss National Museum - Swiss history blog
```

**Exit codes:**
- `1`: Not enough arguments provided
- `2`: `xmllint` not installed

**Dependencies:**
- `curl`: HTTP client for downloading pages
- `xmllint`: Presence check only (from `libxml2-utils` or `libxml2` package)
- `perl` with `HTML::Entities` and `HTML::TreeBuilder` modules
- `timeout`: Command timeout utility
- `file`, `iconv`: File type detection and encoding conversion

---

<details className="generated-section">
<summary>See Also</summary>

- [Metadata/Title.hs](/backend/metadata-title-hs) - Backend title processing in Haskell
- [LinkMetadata.hs](/backend/link-metadata-hs) - Uses titles in annotation database
- [Annotation.hs](/backend/annotation-hs) - Scraping system that may call this script
- [sync.sh](/backend/sync-sh) - Build orchestrator that coordinates metadata generation
- [upload.sh](/shell/upload) - File upload script that extracts titles
- [gwsed.sh](/shell/gwsed) - Site-wide string replacement for updating URLs
</details>
