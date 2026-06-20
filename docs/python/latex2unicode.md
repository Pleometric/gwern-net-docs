---
title: "latex2unicode.py"
description: "latex2unicode.py addresses a major pain point in scientific web publishing: rendering TeX math expressions without heavyweight JavaScript libraries or font dependencies."
sidebar_position: 2
---

# latex2unicode.py

latex2unicode.py addresses a major pain point in scientific web publishing: rendering TeX math expressions without heavyweight JavaScript libraries or font dependencies.

<div className="doc-meta">
  <div><strong>Path</strong><code>build/latex2unicode.py</code></div>
  <div><strong>Language</strong>Python</div>
  <div><strong>Lines</strong>368</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/build/latex2unicode.py">build/latex2unicode.py</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing Python helper scripts for metadata cleanup, text processing, PDFs, dates, or generated content around latex2unicode.
</div>

## Overview

`latex2unicode.py` addresses a major pain point in scientific web publishing: rendering TeX math expressions without heavyweight JavaScript libraries or font dependencies. While full LaTeX rendering (via KaTeX or MathJax) is necessary for complex equations, simple expressions like `$X$` or `$n^2$` can be converted to native HTML like `<em>X</em>` or `<em>n</em><sup>2</sup>`, saving megabytes of assets and rendering instantly.

The script uses GPT-4.1-mini to convert inline TeX from ArXiv-style abstracts into Unicode symbols, HTML tags (`<sup>`, `<sub>`, `<em>`), and minimal CSS. It handles Greek letters (α, β, Δ), mathematical operators (∫, ∑, ∇), special fonts (𝒪, ℝ, ℂ), and complex structures like superimposed sub/superscripts using a custom `.subsup` CSS class.

The conversion rules are extensive and nuanced: use FRACTION SLASH (⁄) for small fractions like `1/2` → `1⁄2`, but BIG SOLIDUS (⧸) for symbolic fractions `a/b` → `_a_⧸_b_`. Skip complex structures like matrices, TikZ diagrams, or deeply nested fractions—those require full LaTeX rendering.

## Key Functions

- **Prompt construction**: 250+ line prompt with detailed conversion rules and examples
- **OpenAI API call**: Uses `gpt-4.1-mini` with system prompt as "skilled mathematician & tasteful typographer"
- **Output handling**: Strips trailing newlines to preserve inline text flow

## Command Line Usage

```bash
# Convert inline TeX from stdin
echo 'O(1)' | OPENAI_API_KEY="sk-XXX" python latex2unicode.py
# Output: 𝒪(1)

# Convert from command line argument
OPENAI_API_KEY="sk-XXX" python latex2unicode.py '\\frac{1}{2}'
# Output: 1⁄2

# Complex example with subscripts/superscripts
echo '\\int_a^b f(x) dx' | OPENAI_API_KEY="sk-XXX" python latex2unicode.py
# Output: ∫<span class="subsup"><sub><em>a</em></sub><sup><em>b</em></sup></span> <em>f</em>(<em>x</em>) <em>dx</em>

# Natural language input also works (bonus feature)
echo 'asymptotically square root n' | OPENAI_API_KEY="sk-XXX" python latex2unicode.py
# Output: 𝒪(√<em>n</em>)
```

**When NOT to convert:**
- Block equations or complex structures (matrices, TikZ)
- Custom operators or redefined commands
- Expressions needing special positioning (overrightarrow, underline)

---

<details className="generated-section">
<summary>See Also</summary>

- [Typography.hs](/backend/typography-hs) - Backend Pandoc filters for processing mathematical content
- [popups.js](/frontend/popups-js) - Frontend that displays the converted math expressions
- [clean-pdf.py](/python/clean-pdf) - Related LLM-based text cleaning tool
- [Annotation.hs](/backend/annotation-hs) - Annotation system that may use converted math
- [sync.sh](/backend/sync-sh) - Build orchestrator that processes mathematical content
- [rewrite.js](/frontend/rewrite-js) - Frontend text processing for math rendering
</details>
