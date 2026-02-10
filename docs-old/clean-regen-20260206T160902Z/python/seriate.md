# seriate.py

**Path:** `build/seriate.py` | **Language:** Python | **Lines:** ~188

LLM-powered semantic sorting that arranges list items in "logical order" via iterative reordering.

---

## Overview

seriate.py implements *seriation*—a generalized form of sorting where items are arranged in a meaningful order that may not follow a strict comparison function. While traditional sorting puts "ACB" → "ABC", seriation might order "Dog, Horse, Kitty" → "Kitty, Dog, Horse" based on semantic similarity (perhaps size, phylogeny, or another contextual relationship).

The script uses OpenAI's GPT API to iteratively reorder input until it reaches a fixed point (the order stops changing). It then validates that the output is a permutation of the input—no items lost or added.

This enables automatic organization of lists, paragraphs, or other content where "logical order" is context-dependent and hard to specify programmatically.

---

## Usage

```bash
# From command line arguments
OPENAI_API_KEY="sk-XXX" python seriate.py "item1" "item2" "item3"

# From stdin (clipboard example)
OPENAI_API_KEY="sk-XXX" xclip -o | python seriate.py

# From pipe
echo "red, blue, green" | OPENAI_API_KEY="sk-XXX" python seriate.py
```

**Requirements:**
- `OPENAI_API_KEY` environment variable set
- OpenAI Python package (`pip install openai`)

---

## How It Works

### Seriation Algorithm

```
Input list
    ↓
┌─────────────────────────┐
│ Ask LLM to seriate once │
└─────────────────────────┘
    ↓
┌─────────────────────────┐
│ Result == Previous?     │──Yes──→ Done (fixed point)
└─────────────────────────┘
    ↓ No
┌─────────────────────────┐
│ Result seen before?     │──Yes──→ Error (cycle detected)
└─────────────────────────┘
    ↓ No
┌─────────────────────────┐
│ Iterations < 5?         │──No───→ Use current result
└─────────────────────────┘
    ↓ Yes
    Loop back
    ↓
Validate permutation
    ↓
Output
```

### Fixed Point Iteration

The script repeatedly asks the LLM to reorder until the output equals the input (fixed point). This handles cases where:
- Initial reordering is suboptimal
- Multiple passes reveal better arrangements
- The model gradually converges to the "right" order

### Permutation Validation

```python
def is_permutation(a, b):
    return sorted(a) == sorted(b)
```

After seriation, the script verifies that output characters (when sorted) match input characters—a simple check that nothing was lost or invented.

---

## The Prompt

The LLM receives a detailed prompt with examples covering:

| Type | Example |
|------|---------|
| Magnitude | `mm; km; cm; m` → `mm; cm; m; km` |
| Colors | `red, purple, blue...` → rainbow order |
| Value | `gold platinum silver copper` → by value |
| Process | `land \| take off \| taxi...` → flight sequence |
| Hierarchy | `PhD, Bachelor's, High School...` → education levels |
| Temporal | `Renaissance, Bronze Age...` → chronological |
| Spatial | `Mars, Earth, Jupiter...` → distance from sun |
| Alphabetical | Items without obvious semantic order |
| Multi-level | Nested lists, paragraphs |

The prompt emphasizes:
- **Seriate, don't modify**: Only reorder, preserve exact text
- **Stable seriation**: When ambiguous, keep original order
- **Return empty for non-lists**: Single items or non-seriatable input → `""`

---

## Key Functions

### seriate_once(target)

Single-pass seriation via GPT API:

```python
def seriate_once(target):
    prompt = """Task: 'seriate' inputs...""" + target + "</input>\n<output>"

    completion = client.chat.completions.create(
        model="gpt-5-mini",
        timeout=60,
        messages=[
            {"role": "developer", "content": "You are an analyst and editor..."},
            {"role": "user", "content": prompt}
        ],
    )
    return completion.choices[0].message.content\
        .removeprefix("<output>")\
        .removesuffix("</output>")
```

### is_permutation(a, b)

Character-level permutation check:

```python
def is_permutation(a, b):
    return sorted(a) == sorted(b)
```

---

## Configuration

| Setting | Value | Purpose |
|---------|-------|---------|
| Model | `gpt-5-mini` | Fast, capable model for reordering |
| Timeout | 60s | API call timeout |
| Max iterations | 5 | Prevent infinite loops |

---

## Error Handling

**Cycle Detection:**
```python
if result in seen:
    print(f"ERROR: cycle detected at iteration {i+1}", file=sys.stderr)
    sys.exit(1)
```

If the model produces an output it's seen before (but isn't the current input), there's a cycle—seriation will never converge.

**Permutation Check:**
```python
if not is_permutation(original, current):
    print(f"ERROR: not a permutation", file=sys.stderr)
    sys.exit(1)
```

Catches cases where the model accidentally dropped or invented content.

---

## Use Cases

### Content Organization

Automatically sort:
- Lists of related items (books, tools, concepts)
- Paragraph sequences in notes
- Image captions or gallery items
- Bibliography entries by relevance

### LLM Writing Pipeline

The script suggests a seriation-first approach:
1. **Seriate** raw notes (constrained to reordering only)
2. **Summarize** to create structure/TOC
3. **Rewrite** the organized content

This prevents LLMs from inadvertently omitting or rewriting content during organization.

---

## Theoretical Background

The script's docstring explains:

> This sort of distance minimization is known as 'seriation' (or 'ordination'), and can be seen as a generalization of regular sorting

References:
- [Wikipedia: Seriation (archaeology)](https://en.wikipedia.org/wiki/Seriation_(archaeology))
- [Wikipedia: Ordination (statistics)](https://en.wikipedia.org/wiki/Ordination_(statistics))
- [JSS: Seriation article](https://www.jstatsoft.org/article/view/v025i03)
- [Gwern.net: Unsort](https://gwern.net/unsort) (inverse problem)

---

## Dependencies

- **OpenAI Python SDK**: `pip install openai`
- **OPENAI_API_KEY**: Environment variable with valid API key

---

## See Also

- [Annotation.hs](/backend/annotation-hs) - Metadata that might benefit from seriation
- [embed.sh](/shell/embed) - Another LLM-powered text utility (embeddings)
- [Tags.hs](/backend/tags-hs) - Tag organization that could use seriation
