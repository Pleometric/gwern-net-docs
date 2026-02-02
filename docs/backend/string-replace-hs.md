
# stringReplace.hs

**Path:** `build/stringReplace.hs` | **Language:** Haskell | **Lines:** ~47

> Parallel exact string search-and-replace utility for batch file processing

---

## Overview

`stringReplace.hs` is a command-line utility for performing exact string literal replacements across multiple files in parallel. Unlike `sed`, `perl`, or other common text processing tools, it handles all special characters (`#`, `&`, etc.) correctly without escaping issues, making it reliable for replacing URLs, HTML fragments, or other content containing shell-problematic characters.

The tool is designed for the gwern.net build system where mass string replacements are common—updating URLs after link archival, changing domain names, or correcting repeated typos across thousands of files. By using Haskell's `Data.Text` for efficient Unicode text handling and `Control.Monad.Parallel` for concurrent file processing, it achieves good performance on large filesets.

Key design decisions include: exact (non-regex) matching for predictability, silent directory skipping for shell glob convenience, only writing files that actually changed (avoiding unnecessary disk I/O and timestamp churn), and automatic parallelization based on available CPU cores.

---

## Public API

This is a standalone executable, not a library module.

### Command-Line Interface

```bash
# Single file mode
./stringReplace.hs 'original' 'replacement' filename.txt

# Stdin mode (filenames from pipe)
find . -name "*.txt" | ./stringReplace.hs 'original' 'replacement'
```

**Arguments:**
1. `original` — The exact string to search for (required, non-empty)
2. `replacement` — The replacement string (required, can be empty for deletion)
3. `filename` — Optional; if omitted, filenames are read from stdin (newline-delimited)

**Called by:** [sync-sh](sync-sh), various build scripts
**Calls:** Standard Haskell libraries only

---

## Internal Architecture

### Data Flow

```
Arguments → Parse → Get file list → Deduplicate → Parallel map → Per-file replace
                         ↓
              stdin (pipe) OR single file argument
```

### Core Function

```haskell
replace :: T.Text -> T.Text -> FilePath -> IO ()
```

The `replace` function handles a single file:
1. Validates inputs (non-empty original, original ≠ replacement)
2. Checks if path is a file (skips directories silently)
3. Reads entire file into memory as `Text`
4. Applies `T.replace` (single-pass replacement)
5. Writes back only if content changed

### Parallelization

```haskell
cores <- getNumCapabilities
setNumCapabilities ((cores - 1) `max` 1)
Par.mapM_ (replace ...) files
```

Uses `Control.Monad.Parallel.mapM_` to process files concurrently. Reserves one core (the formula `(cores - 1) max 1` ensures at least one worker) presumably to keep the system responsive during batch operations.

### Deduplication

```haskell
files <- if null file then fmap (nubOrd . lines) getContents else return file
```

When reading from stdin, filenames are deduplicated using `nubOrd` (O(n log n) via ordered containers) to avoid processing the same file multiple times if piped input contains duplicates.

---

## Key Patterns

### Exact Matching by Design

The tool deliberately uses `Data.Text.replace` which does literal string matching. This avoids the entire class of regex escaping bugs that plague `sed`/`perl` one-liners when replacing URLs or code containing `.*?[](){}^$\|` characters.

### Write-If-Changed

```haskell
when (old /= new) $ TIO.writeFile f new
```

Only writes when content actually changed. This is important for build systems that use file modification times—unnecessary writes would trigger downstream rebuilds.

### Fail-Fast Validation

The function validates inputs before doing any I/O work:
- Empty original string → error (meaningless operation)
- Empty filename → error
- Original equals replacement → error (no-op)
- Non-existent non-directory path → error

This catches user errors immediately rather than silently doing nothing.

### Silent Directory Handling

```haskell
if isDirectory then return ()
```

Directories are silently skipped rather than erroring. This enables convenient shell usage like `stringReplace foo bar *` where globs may include directories.

---

## Configuration

No configuration files. Behavior is controlled entirely by command-line arguments.

**Compile-time optimization:** The shebang `#!/usr/bin/env runghc` runs interpreted. For production use, compile with optimizations:

```bash
ghc -O2 -threaded stringReplace.hs -o stringReplace
./stringReplace +RTS -N -RTS 'old' 'new' file.txt
```

---

## Integration Points

### Build System Integration

Typically invoked from shell scripts via `find` pipelines:

```bash
find ./docs -name "*.html" -type f | ./stringReplace 'http://old-url' 'https://new-url'
```

### Dependencies

- `containers` — for `Data.Containers.ListUtils.nubOrd`
- `text` — for efficient `Data.Text` operations
- `monad-parallel` — for `Control.Monad.Parallel.mapM_`
- `directory` — for `doesFileExist`, `doesDirectoryExist`

---

## See Also

- [Utils.hs](/backend/utils-hs) - Core utility module with `replace` and `sed` functions for in-memory operations
- [Typography.hs](/backend/typography-hs) - Text transformation module with similar string processing
- [sync.sh](/backend/sync-sh) - Build orchestrator that calls this utility for batch file processing
- [Hakyll.hs](/backend/hakyll-hs) - Site generator that may use string replacement for post-processing
- [rename.hs](/backend/rename-hs) - Page renaming utility that generates similar replacement commands
