---
sidebar_position: 5
---

# image-margin-checker.py [DEPRECATED]

**Path:** `build/image-margin-checker.py` | **Language:** Python | **Lines:** ~103

CLI tool to check whether an image is too tightly cropped and needs greater padding/margin using GPT-4 Vision.

---

## Overview

**WARNING:** This script is deprecated and not recommended for use as of early 2024. Testing at scale revealed that the results were poor, producing vastly oversized margin recommendations. The root cause—whether GPT-4-Vision's visual understanding or the prompt design—remains unclear, but the tool should not be used as-is.

The original intent was to automatically detect research figures, graphs, and diagrams that lacked sufficient whitespace around their edges. Tight cropping can make images feel cramped and harder to read. The script used GPT-4-Vision's image understanding to make binary YES/NO recommendations about whether additional margin was needed.

Padding could then be added using Bash functions `pad` and `pad-black` (not part of this script). The vision model was called with `detail: "low"` to minimize cost, as the gestalt of margins shouldn't require fine detail.

## Key Functions

- **`is_valid_api_key(key)`**: Validates OpenAI API key format (starts with `sk-` followed by 40 alphanumeric characters)
- **`encode_image(image_path)`**: Base64-encodes image for API transmission
- **`check_image_margin(api_key, image_path)`**: Sends image to GPT-4o-mini with prompt asking for YES/NO margin assessment
- **Error handling**: Detects HTTP errors, OpenAI safety system blocks, and unexpected responses

## Command Line Usage

```bash
# Check single image
OPENAI_API_KEY="sk-XXX" python image-margin-checker.py image.jpg
# Output: image.jpg : NO  (or YES)

# Check multiple images
OPENAI_API_KEY="sk-XXX" python image-margin-checker.py img1.png img2.jpg img3.png

# Example outputs
./doc/fiction/humor/2020-01-24-gwern-meme-mickeymouse.jpg : NO
./doc/fiction/science-fiction/time-travel/1998-chiang-figure-1.png : YES

# Safety system error example
WARNING: Received an error for image ./path/to/image.png: 'Your input image may contain content that is not allowed by our safety system.'
```

**Requirements:**
- Python 3
- `openai` Python package
- Valid OpenAI API key in `$OPENAI_API_KEY` environment variable

**API Details:**
- Model: `gpt-4o-mini` (current version; was originally `gpt-4-turbo-preview`)
- Detail level: `low` (85 tokens per image)
- Prompt: ~1,150 tokens
- Response: 1 token (YES or NO)
- Cost: ~¢1.2 per image (as of 2024 pricing)

**Known issues:**
- Produced vastly excessive margin recommendations in production
- Unclear whether vision model or prompt is the primary failure mode
- May be worth revisiting with improved vision models (GPT-4.5-Vision, etc.)

## See Also

- [Image.hs](/backend/image-hs) - Server-side image processing pipeline
- [invertornot.py](/python/invertornot) - Related GPT-4-Vision image classification tool
- [image-focus.js](/frontend/image-focus-js) - Client-side image lightbox
- [should_image_have_outline.php](/php/should-image-have-outline) - PHP-based image analysis
- [png.sh](/shell/png) - PNG optimization script
- [collapse-checker.py](/python/collapse-checker) - Validates collapsed HTML blocks
