---
sidebar_position: 3
---

# text2epositive.py

**Path:** `build/text2epositive.py` | **Language:** Python | **Lines:** ~162

Text style transfer tool that rewrites English to eliminate negations, promoting "E-positive" or "abs-E" language that states what *is* rather than what *isn't*.

---

## Overview

`text2epositive.py` implements a linguistic experiment inspired by [LessWrong's E-positive writing](https://www.lesswrong.com/posts/W8CxEFCnYNdrHkuoB/abs-e-or-speak-only-in-the-positive): rewriting text to avoid negative constructions. Instead of saying "not green," say "blue." Instead of "can't handle," say "built for fewer."

The tool uses OpenAI's `o3-mini` model to perform style transfer while preserving meaning. It targets explicit negations ("not", "won't", "can't") and implicit ones ("nowhere", "nothing", "indigestible", "immortal"). The rewrite should maintain the original semantics and language structure as much as possible, only changing negations to positive statements.

The script handles nuance carefully: quotes, source code, URLs, and mathematical expressions remain untouched. Complex double negatives are resolved ("I can't not go" → "I must attend"). The system prompt positions it as an editor focused on "clarity and specificness."

## Key Functions

- **`get_api_key()`**: Retrieves OpenAI API key from environment
- **`read_input()`**: Accepts text from command line argument or stdin
- **`create_prompt(target)`**: Constructs the transformation prompt with extensive examples
- **`process_text(client, prompt)`**: Sends request to OpenAI API with error handling
- **`main()`**: Orchestrates the pipeline

## Command Line Usage

```bash
# Basic negation removal
echo "The sky is not green" | OPENAI_API_KEY="sk-XXX" python text2epositive.py
# Output: The sky is blue.

# Imperative sentences
echo "Don't touch the wires." | OPENAI_API_KEY="sk-XXX" python text2epositive.py
# Output: Stay away from the wires.

# Complex negations
echo "Grass is indigestible." | OPENAI_API_KEY="sk-XXX" python text2epositive.py
# Output: Grass provides 0 calories to humans.

# From file
cat security-notice.txt | OPENAI_API_KEY="sk-XXX" python text2epositive.py

# Direct argument
OPENAI_API_KEY="sk-XXX" python text2epositive.py "It is not warm."
# Output: It is cool.
```

**Preserved constructs:**
- Quotes and citations (untouched)
- Code blocks, URLs, mathematical notation
- Cases where rewriting would change meaning

**Transformation examples:**
- "not new" → "old"
- "cannot handle" → "built for fewer"
- "nowhere to be found" → "We looked but couldn't find"
- "immortal" → "ages slowly"

## See Also

- [italicizer.py](/python/italicizer) - Companion LLM-based title formatting
- [paragraphizer.py](/python/paragraphizer) - LLM-based text reformatting for abstracts
- [title-cleaner.py](/python/title-cleaner) - LLM-based title cleanup
- [date-guesser.py](/python/date-guesser) - LLM-based date extraction
- [Metadata/Format.hs](/backend/metadata-format-hs) - Rule-based text cleanup
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation database
