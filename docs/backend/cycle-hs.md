
# Cycle.hs

**Path:** `build/Cycle.hs` | **Language:** Haskell | **Lines:** ~45

> Detects and prevents infinite loops in rewrite rule graphs

---

## Overview

Cycle.hs provides cycle detection for gwern.net's extensive rewrite rule systems. The site uses many transformation databases—URL redirects, author name canonicalization, tag normalization, interwiki link expansion—all expressed as `(before, after)` pairs. If these rules form cycles (A→B→A or A→B→C→A), applying them iteratively would loop forever.

This module uses graph theory (strongly connected components) to statically validate rule databases at build time. When cycles are detected, it identifies the specific problematic rules so they can be fixed. It also provides runtime loop testing via `fixedPoint` iteration with cycle detection.

The module is purely a safety mechanism—it either passes silently or halts the build with actionable diagnostics. This is critical because rewrite rules are scattered across dozens of config files and are easy to accidentally create cycles in.

---

## Public API

### `isCycleLess :: (Eq a, Ord a, Show a) => [(a,a)] -> [(a,a)]`

Validates that a list of rewrite rules contains no cycles. Returns the input unchanged if valid; throws an error with `findCycles` results if cycles exist.

**Called by:** Test.hs (validates Config.Tags.tagsShort2LongRewrites, Config.Metadata.Author.canonicals, Config.Metadata.Author.authorLinkDB), Tags.hs (testTags), Interwiki.hs (interwikiCycleTestSuite)

**Calls:** `cycleExists`, `findCycles`

---

### `cycleExists :: Ord a => [(a, a)] -> Bool`

Core detection function. Returns `True` if the rule graph contains any cycles.

Uses two checks:
1. Self-loops: any rule where `a == b`
2. Multi-node cycles: any strongly connected component with >1 node

**Called by:** `isCycleLess`, `findCycles`

**Calls:** `Data.Graph.stronglyConnComp`, `Data.Graph.flattenSCC`

---

### `findCycles :: Ord a => [(a, a)] -> [(a, a)]`

When cycles exist, identifies the specific rules causing them. Uses incremental validation: builds up a "good" list by adding rules one at a time, rejecting any that would create a cycle.

**Called by:** `isCycleLess`, Interwiki.hs (interwikiCycleTestSuite for diagnostics)

**Calls:** `cycleExists`

---

### `testInfixRewriteLoops :: (Show a, Eq a, Ord a) => [(a,a)] -> (a -> a) -> [(a,a,a)]`

Runtime loop tester. Takes a rewrite database and a transformation function, applies the function to each "before" value until fixedpoint, returns triples of `(before, expectedAfter, actualResult)`.

If the transformation loops, `fixedPoint` (from Utils) will error after 5000 iterations or upon detecting a value cycle.

**Called by:** Test suites (runtime validation)

**Calls:** `Utils.fixedPoint`

---

### `testCycleDetection :: [(Int,Int)]((Int,Int).md)`

Self-test for `cycleExists`. Returns empty list on success, otherwise the failing test cases. Uses test data from `Config.Misc.cycleTestCases`.

**Called by:** Test suite

**Calls:** `testCycleExists`, `Config.Misc.cycleTestCases`

---

## Internal Architecture

### Graph Representation

Rules `[(a, b)]` are converted to graph edges for SCC analysis:

```haskell
map (\(a, b) -> (a, a, [b])) tuples
-- Tuple format: (node, key, [neighbors])
```

### Cycle Detection Algorithm

```
1. Check for trivial self-loops (a → a)
2. Build directed graph from rules
3. Compute strongly connected components (Tarjan's algorithm via Data.Graph)
4. Any SCC with >1 node indicates a cycle
```

### Cycle Identification (findCycles)

```
1. Start with empty "good" and "bad" lists
2. For each rule:
   - Try adding to "good" list
   - If creates cycle, add to "bad" instead
3. Return "bad" list (minimal cycle-causing rules)
```

This is O(n²) in the number of rules but rule lists are small (\<1000) and this only runs at build time.

---

## Key Patterns

### Strongly Connected Components for Cycle Detection

Rather than implementing cycle detection from scratch, the module leverages `Data.Graph.stronglyConnComp`. An SCC with multiple nodes means those nodes are all mutually reachable—a cycle. This is a classic graph theory approach:

```haskell
any ((> 1) . length . flattenSCC)
    (stronglyConnComp $ map (\(a, b) -> (a, a, [b])) tuples)
```

### Incremental Fault Isolation

`findCycles` doesn't just report "cycles exist"—it identifies which rules are problematic. The algorithm adds rules one-by-one, keeping those that don't create cycles. This gives actionable output: "remove these specific rules to fix."

### Dual Static/Runtime Checking

The module supports two complementary approaches:
- **Static**: `cycleExists`/`isCycleLess` check rule databases at compile/build time
- **Runtime**: `testInfixRewriteLoops` + `fixedPoint` detect actual infinite loops during transformation

---

## Configuration

Test cases live in `Config/Misc.hs`:

```haskell
cycleTestCases :: [([(Int, Int)], Bool)]
-- [(rules, expectedHasCycle)]
```

Covers:
- Empty rules
- Self-loops
- Two-node cycles (A↔B)
- Multi-node cycles (A→B→C→A)
- Disjoint cycle sets
- Acyclic graphs

---

## Integration Points

### Build-Time Validation

`Test.hs` runs `isCycleLess` on multiple config databases:

```haskell
isCycleLess Config.Tags.tagsShort2LongRewrites
isCycleLess (M.toList Config.Metadata.Author.canonicals)
isCycleLess (M.toList Config.Metadata.Author.authorLinkDB)
```

Build fails immediately if any contain cycles.

### Runtime Protection via Utils.fixedPoint

The related `fixedPoint` function in Utils.hs provides runtime protection:

```haskell
fixedPoint' 0 _ _ i = error $ "Hit recursion limit: still changing after 5,000 iterations!"
fixedPoint' n seen f i
  | i `S.member` seen = error $ "Cycle detected!"
  | i' == i = i  -- converged
  | otherwise = fixedPoint' (n-1) (S.insert i seen) f i'
```

This catches cycles that static analysis might miss (e.g., cycles that only manifest with certain input values).

### Protected Databases

Databases validated with this module include:
- `Config.Tags.tagsShort2LongRewrites` - Tag abbreviation expansion
- `Config.Metadata.Author.canonicals` - Author name normalization
- `Config.Metadata.Author.authorLinkDB` - Author→URL mapping
- `Config.Interwiki.redirectDB` - URL redirect rules

---

## See Also

- [rewrite.js](/frontend/rewrite-js) - Frontend URL rewriting that benefits from cycle-free configs
- [hakyll.hs](/backend/hakyll-hs) - Build system that uses validated rewrite rules
- [Utils.hs](/backend/utils-hs) - `fixedPoint` function for runtime cycle detection
- [Test.hs](/backend/test-hs) - Build-time validation runner
- [Tags.hs](/backend/tags-hs) - Uses cycle checking for tag rewrites
- [Interwiki.hs](/backend/interwiki-hs) - Uses cycle checking for redirect rules
