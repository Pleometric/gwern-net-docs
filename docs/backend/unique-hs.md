
# Unique.hs

**Path:** `build/Unique.hs` | **Language:** Haskell | **Lines:** ~70

> Runtime uniqueness checking for configuration lists to catch duplicates early

---

## Overview

Unique.hs provides a set of validation functions that check configuration data structures for duplicate entries at runtime. The module exists because gwern.net has many large, hand-maintained configuration lists (URL rewrites, metadata mappings, CSS class definitions) where accidental duplicates can cause subtle bugs.

The design philosophy is "fail fast": rather than silently accepting duplicates that might cause one entry to shadow another, these functions throw descriptive errors at program startup. This catches configuration mistakes immediately rather than during production use.

The module header notes that compile-time checking via Template Haskell was considered but abandoned due to complexity. Runtime checking is simpler and provides equally good protection for static configuration data.

---

## Public API

### `isUniqueList :: (Eq a, Ord a, Show a) => [a] -> [a]`

Validates that a simple list contains no duplicate elements.

**Called by:** Config modules with flat lists
**Calls:** `checkUniqueOrThrow`, `getDuplicates`

---

### `isUnique :: (Eq a, Show a, Eq b, Ord a, Ord b, Show b) => [(a,b)] -> [(a,b)]`

Validates that an association list contains no duplicate key-value pairs (identical tuples).

```haskell
isUnique [("a", 1), ("b", 2), ("a", 1)]  -- throws error
isUnique [("a", 1), ("a", 2)]            -- passes (same key, different value)
```

**Called by:** Config modules with pair-based mappings
**Calls:** `checkUniqueOrThrow`

---

### `isUniqueKeys :: (Eq a, Ord a, Show a, Show b) => [(a,b)] -> [(a,b)]`

Validates that all keys in an association list are unique (no duplicate first elements).

```haskell
isUniqueKeys [("a", 1), ("a", 2)]  -- throws error (duplicate key "a")
isUniqueKeys [("a", 1), ("b", 1)]  -- passes (same value, different keys)
```

**Called by:** Lookup tables where keys must be unique
**Calls:** `getDuplicates`, `throwError`

---

### `isUniqueKeys3 :: (Eq a, Ord a, Show a) => [(a,b,c)] -> [(a,b,c)]`

Validates uniqueness of the first element in 3-tuples.

**Called by:** Config tables using `(key, value1, value2)` format
**Calls:** `getDuplicates`, `throwError`

---

### `isUniqueKeys4 :: (Eq a, Ord a, Show a) => [(a,b,c,d)] -> [(a,b,c,d)]`

Validates uniqueness of the first element in 4-tuples.

**Called by:** Config tables using `(key, value1, value2, value3)` format
**Calls:** `getDuplicates`, `throwError`

---

### `isUniqueMiddle3 :: (Eq a, Ord a, Ord b, Show b, Show a) => [(a,b,c)] -> [(a,b,c)]`

Validates uniqueness of the *second* element in 3-tuples.

```haskell
isUniqueMiddle3 [(1, "a", True), (2, "a", False)]  -- throws error
```

**Called by:** Config where middle element is a secondary key
**Calls:** `getDuplicates`, `throwError`

---

### `isUniqueValues :: (Show a, Ord a, Eq b, Ord b, Show b) => [(a,b)] -> [(a,b)]`

Validates that all values in an association list are unique (no duplicate second elements).

**Called by:** Bijective mappings where values must also be unique
**Calls:** `getDuplicates`, `throwError`

---

### `isUniqueAll :: (Eq a, Ord a, Show a, Eq b, Ord b, Show b) => [(a,b)] -> [(a,b)]`

Validates that keys, values, *and* complete pairs are all unique. Composes all three checks.

```haskell
isUniqueAll = isUniqueValues . isUniqueKeys . isUnique
```

**Called by:** Strict bijective mappings
**Calls:** `isUniqueValues`, `isUniqueKeys`, `isUnique`

---

## Internal Architecture

### Core Algorithm

The duplicate detection uses a single-pass `foldl'` with a `Set` accumulator:

```haskell
getDuplicates :: Ord a => [a] -> [a]
getDuplicates = snd . foldl' go (Set.empty, [])
  where
    go (seen, duplicates) x
      | x `Set.member` seen = (seen, x : duplicates)
      | otherwise = (Set.insert x seen, duplicates)
```

This is O(n log n) in time and O(n) in space. Elements are added to `seen` as they're encountered; if an element is already in `seen`, it's a duplicate.

### Error Reporting

```haskell
throwError :: Show a => String -> [a] -> b
throwError msg xs = error $ "Error: " ++ msg ++ " " ++ show xs
```

Errors include the message context and the actual duplicate values, making debugging straightforward.

### Validation Pattern

All public functions follow the same pattern:
1. Return the input unchanged if valid (enables chaining)
2. Throw with `error` if duplicates found
3. Include module/function name in error message for easy grep

---

## Key Patterns

### Identity-on-Success

All validators return their input unchanged on success:

```haskell
checkUniqueOrThrow msg xs
  | null duplicates = xs  -- return original list
  | otherwise = throwError msg duplicates
```

This allows validators to be inserted inline in config definitions:

```haskell
urlRewrites :: [(String, String)]
urlRewrites = isUniqueKeys
  [ ("/old", "/new")
  , ("/foo", "/bar")
  ]
```

### Tuple Projection Variants

The `isUniqueKeys3`, `isUniqueKeys4`, and `isUniqueMiddle3` functions exist because Haskell tuples aren't iterable and there's no generic "get nth element" operation. Each tuple arity needs its own function.

---

## Configuration

This module has no configuration. It's a pure validation library.

---

## Integration Points

### Usage in Config Modules

Validators are typically applied at module initialization:

```haskell
-- In some config module
myConfig :: [(URL, Handler)]
myConfig = isUniqueKeys
  [ ("https://example.com", handleExample)
  , ...
  ]
```

### Error Behavior

Throws via `error`, which:
- Terminates the program immediately in pure code
- Can be caught in IO with `catch` if needed
- Prints to stderr before exit

### Constraints

All validators require `Ord` on the elements being checked (for Set operations) and `Show` for error messages.

---

## See Also

- [Utils.hs](/backend/utils-hs) - Core utilities that Unique complements for validation
- [LinkMetadata.hs](/backend/link-metadata-hs) - Uses uniqueness checks on annotation databases
- [Annotation.hs](/backend/annotation-hs) - Config tables validated at startup
- [Config.Misc](/backend/config-misc-hs) - Configuration constants using uniqueness validation
- [Config.Tags](/backend/config-tags-hs) - Tag configuration with unique key constraints
- [sync.sh](/backend/sync-sh) - Build orchestrator relying on validated configurations
