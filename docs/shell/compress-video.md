# compressVideo

**Path:** `build/compressVideo` | **Language:** Bash | **Lines:** 245

> Recompresses poorly-compressed MP4 videos for web hosting.

---

## Overview

`compressVideo` transcodes MP4 videos in place to browser-oriented H.265/HEVC plus AAC in an MP4 container. It is aimed at oversized supplementary videos and phone footage that are much larger than needed for web streaming.

The script probes the first video stream with `ffprobe`, detects codec/tag/HDR state, and either skips, stream-copies, retags, tone-maps, or re-encodes depending on the input.

## Behavior

- Already-compatible HEVC/hvc1 SDR files are skipped unless `FORCE=1`.
- HEVC SDR files tagged `hev1` are remuxed to `hvc1` with video streamcopy to avoid generation loss.
- HDR sources using HLG or PQ transfer characteristics are tone-mapped to SDR Rec. 709.
- Other inputs are encoded with `libx265`, default `CRF=26`, `-preset slow`, AAC audio, `hvc1` tagging, and `+faststart`.
- The original is normally replaced only when the output beats the default size-reduction threshold of 20 percent, except for compatibility fixes such as HDR-to-SDR.

## Configuration

Environment variables include `CRF`, `AUDIO_BITRATE`, `SIZE_REDUCTION_THRESHOLD`, and `FORCE`.

Dependencies listed in the source are `ffmpeg` with `libx265` and `libzimg`, `ffprobe`, `awk`, and GNU coreutils.

