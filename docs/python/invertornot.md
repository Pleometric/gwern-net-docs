---
sidebar_position: 4
---

# invertornot.py

**Path:** `build/invertornot.py` | **Language:** Python | **Lines:** ~96

Experimental GPT-4-Vision script for classifying images for dark-mode inversion. **Now obsoleted by InvertOrNot.com.**

---

## Overview

`invertornot.py` was a prototype solution to one of the hardest problems in dark-mode implementations: deciding whether images should be inverted/negated or just dimmed. A bright white diagram may look fine inverted, but inverting a grayscale photograph of a human face looks terrible. No simple heuristic works—it's truly an "I know it when I see it" visual classification task.

The script used GPT-4-Vision's multi-image capability: it takes the original image, generates an inverted version using PIL's `ImageOps.invert()`, sends both to GPT-4o-mini, and asks "Does the inverted image look OK for dark-mode? YES/NO." This zero-shot comparison approach worked reasonably well, as the model could directly see both versions.

**Why it's obsolete:** The cost was prohibitive (~2¢ per image, or $440 to classify all Wikipedia thumbnails on gwern.net). Mattis Megevand created InvertOrNot.com, a free API using a small finetuned EfficientNet running on CPU, with higher accuracy, user feedback for continuous improvement, and zero per-query cost. The script remains as a reference implementation for similar vision tasks.

## Key Functions

- **`encode_image(image_path)`**: Converts image to base64 for API upload
- **`invert_image(image_path)`**: Creates negated version using PIL ImageOps
- **API request**: Sends both images to GPT-4o-mini with comparison prompt

## Command Line Usage

```bash
# Classify a single image
OPENAI_API_KEY="sk-XXX" python invertornot.py path/to/image.jpg
# Output: JSON response with YES/NO judgment

# Example output structure:
# {
#   "choices": [{
#     "message": {
#       "content": "NO"  # or "YES"
#     }
#   }]
# }
```

**Note:** The script has a hardcoded test image path on line 59. In practice, you'd replace this with `sys.argv[1]` or similar to accept arbitrary inputs.

**Prompt strategy:**
- Sends original image first, inverted second
- Asks: "This is the inverted image... Does it look OK? Respond YES/NO."
- max_tokens: 300 (minimal, as response is just YES/NO)

## See Also

- [Image.hs](/backend/image-hs) - Server-side image processing including inversion detection
- [image-focus.js](/frontend/image-focus-js) - Client-side image lightbox
- [dark-mode.js](/frontend/dark-mode-js) - Frontend dark-mode system that uses inversion classifications
- [should_image_have_outline.php](/php/should-image-have-outline) - Related image analysis tool
- [image-margin-checker.py](/python/image-margin-checker) - Another GPT-4-Vision image analysis tool
- [rewrite.js](/frontend/rewrite-js) - DOM transformation handlers that apply inversion CSS
- **InvertOrNot.com**: The production solution replacing this script
