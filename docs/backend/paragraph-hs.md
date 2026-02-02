
# Paragraph.hs

**Path:** `build/Paragraph.hs` | **Language:** Haskell | **Lines:** ~75

> LLM-powered paragraph splitting for research paper abstracts

---

## Overview

Paragraph.hs addresses a common readability problem: research paper abstracts are typically written as dense, single-paragraph walls of text. While they follow a logical structure (background → methods → results → conclusion), the lack of visual separation makes them hard to skim. This module uses GPT-4o to intelligently split run-on paragraphs into multiple topic-organized paragraphs.

The implementation is deliberately conservative. It only processes text that: (1) exceeds a minimum length threshold, (2) doesn't already contain paragraph breaks, and (3) isn't on a manual whitelist of URLs that shouldn't be modified. The LLM call is delegated to a Python script (`paragraphizer.py`) that handles the OpenAI API interaction, while Haskell handles input validation, output verification, and integration with the broader annotation system.

A notable feature is URL confabulation detection: since LLMs can hallucinate URLs when adding hyperlinks, the module spawns browser tabs for any URLs not already in the metadata database, allowing manual verification of link validity.

---

## Public API

### `processParagraphizer :: Metadata -> FilePath -> String -> IO String`

Main entry point. Takes annotation text and returns the same text with paragraph breaks inserted.

**Called by:** `Annotation.PDF`, `Annotation.Arxiv`, `Annotation.OpenReview`, `Annotation.Biorxiv`
**Calls:** `paragraphized`, `paragraphizer.py` (via shell), `cleanAbstractsHTML`, `checkURLs`

**Logic flow:**
1. Return early if text is empty or too short (\<768 chars)
2. Return early if already paragraphized (has `\n\n`, block elements, or multiple `<p>` tags)
3. Strip HTML tags and convert to Markdown
4. Call `paragraphizer.py` with cleaned text
5. Parse LLM output back to HTML via Pandoc
6. Run URL confabulation check
7. Return cleaned HTML

### `paragraphized :: FilePath -> String -> Bool`

Predicate: returns `True` if text already has multiple paragraphs or is whitelisted.

**Called by:** `processParagraphizer`, `warnParagraphizeGTX`
**Calls:** None (pure)

Checks for:
- URL in whitelist (`Config.Paragraph.whitelist`)
- Double-newlines (Markdown paragraph separator)
- Block elements (`<ul>`, `<ol>`, `<blockquote>`, `<figure>`, `<table>`, etc.)
- Multiple `<p>` tags

### `warnParagraphizeGTX :: FilePath -> IO ()`

Diagnostic utility. Scans a GTX database and prints URLs of annotations that could benefit from paragraphization.

**Called by:** `sync.sh` (manual invocation via ghci)
**Calls:** `readGTXFast`, `paragraphized`

### `checkURLs :: Metadata -> Pandoc -> IO ()`

Experimental confabulation detector. Finds URLs in output that aren't in the metadata database and aren't local, then opens them in Chromium for manual review.

**Called by:** `processParagraphizer`
**Calls:** `extractURLs`, `runShellCommand` (spawns chromium via forkIO)

---

## Internal Architecture

### Data Flow

```
Input text (HTML)
       │
       ▼
┌──────────────────┐
│ Pre-checks       │ ← Already paragraphized? Too short? Whitelisted?
└──────────────────┘
       │ (pass)
       ▼
┌──────────────────┐
│ HTML → Markdown  │ ← Strip <p> tags, normalize whitespace
└──────────────────┘
       │
       ▼
┌──────────────────┐
│ paragraphizer.py │ ← GPT-4o API call (external process)
└──────────────────┘
       │
       ▼
┌──────────────────┐
│ Markdown → HTML  │ ← Pandoc parsing/rendering
└──────────────────┘
       │
       ▼
┌──────────────────┐
│ URL verification │ ← Open unknown URLs in browser
└──────────────────┘
       │
       ▼
Output text (HTML)
```

### Python Script: paragraphizer.py

The actual LLM interaction happens in `build/paragraphizer.py`. Key details:

- **Model:** `gpt-4o-mini` (previously `gpt-4o`)
- **Temperature:** 0 (deterministic)
- **Prompt:** Extensive few-shot prompt with ~40 examples covering various abstract types
- **Validation:**
  - Must add at least one `\n` (paragraph break)
  - Output can't exceed 2× input length + 1000 chars
  - All numbers from input must appear in output (prevents silent rewording of statistics)

---

## Key Patterns

### Conservative Application

The module errs heavily on the side of doing nothing:

```haskell
processParagraphizer md p a =
      if length a < C.minLength || paragraphized p a then return a
      else -- ... proceed with LLM call
```

This "fail-safe to no-op" pattern is appropriate for automated text modification where the cost of a bad rewrite exceeds the benefit of a good one.

### External Process Delegation

Rather than embedding OpenAI client code in Haskell, the module shells out to Python:

```haskell
(status,_,mb) <- runShellCommand "./" Nothing "python3"
    ["static/build/paragraphizer.py", a'']
```

This keeps the Haskell code simple and lets the Python ecosystem handle API interaction, while Haskell handles validation and integration.

### Asynchronous URL Verification

URL checking uses `forkIO` to spawn browser tabs without blocking:

```haskell
checkURLs md p = let urls = filter (\u -> not (isLocal u || M.member (T.unpack u) md)) $ extractURLs p in
                   mapM_ (\u -> forkIO $ void $ runShellCommand "./" (Just [("DISPLAY", ":0")]) "chromium" [T.unpack u]) urls
```

This is an interesting design: rather than programmatically verifying URLs (which would be unreliable), it presents them to a human for review.

---

## Configuration

### Config.Paragraph

| Setting | Type | Default | Purpose |
|---------|------|---------|---------|
| `minLength` | `Int` | 768 | Minimum character count to trigger processing |
| `whitelist` | `[String]` | ~30 URLs | URLs that should never be paragraphized |

The whitelist contains URLs for documents where the abstract format is intentionally unusual (e.g., PDFs with complex formatting, sites with pre-formatted content).

---

## Integration Points

### Annotation Scrapers

The module is called by all major annotation scrapers:

- `Annotation.PDF` → processes PDF abstracts after extraction
- `Annotation.Arxiv` → processes arXiv abstracts
- `Annotation.OpenReview` → processes OpenReview abstracts
- `Annotation.Biorxiv` → processes bioRxiv/medRxiv abstracts

### Environment Requirements

- `OPENAI_API_KEY` environment variable (for `paragraphizer.py`)
- `DISPLAY=:0` (for Chromium URL verification on headless systems)
- Working directory must be gwern.net root (uses `Config.Misc.cd`)

### Build System

From `sync.sh`:
```bash
ghci -istatic/build/ ./static/build/Paragraph.hs -e 'warnParagraphizeGTX "metadata/full.gtx"'
```

This is a manual diagnostic command, not part of the regular build.

---

## See Also

- [paragraphizer.py](/python/paragraphizer) - Python script that makes the GPT-4o API call
- [Annotation.hs](/backend/annotation-hs) - Dispatcher that calls annotation scrapers
- [Annotation/Arxiv.hs](/backend/annotation-arxiv-hs) - Scraper that uses processParagraphizer
- [Annotation/Biorxiv.hs](/backend/annotation-biorxiv-hs) - Scraper that uses processParagraphizer
- [Annotation/OpenReview.hs](/backend/annotation-openreview-hs) - Scraper that uses processParagraphizer
- [LinkMetadata.hs](/backend/link-metadata-hs) - Metadata database that stores results
- [GTX.hs](/backend/gtx-hs) - Database format containing annotations to check
