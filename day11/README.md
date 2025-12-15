# Day 11: DAG Path Counting

## Problem Summary
Count paths in a directed acyclic graph from sources to sinks.
- **Part 1:** Count paths from "YOU" to "OUT"
- **Part 2:** Count paths from "SVR" to output via intermediate nodes "DAC" and "FFT"

## Algorithm

### Topological DP
1. Topological sort of the DAG
2. Dynamic programming: `paths[v] = sum(paths[u] for u in predecessors(v))`
3. Memoization to avoid recomputation
4. Sum paths ending at sink nodes

### Level-Synchronous Alternative (ASM-friendly)
Instead of recursive memoized DFS:
1. Assign topological level to each node (distance from sources)
2. Process nodes level-by-level in forward order
3. For each node, sum path counts from all predecessors

Benefits: sequential memory access, naturally parallelizable, no recursion overhead.

## Performance Results (Internal Timing)

| Language | Time (ms) | vs C | Startup (ms) |
|----------|-----------|------|--------------|
| **ASM** | 0.086 | 0.12x (fastest) | 1.5 |
| **Go** | 0.462 | 0.64x | 2.1 |
| **Rust** | 0.487 | 0.68x | 1.5 |
| **C** | 0.717 | 1.0x (baseline) | 1.5 |
| **TypeScript** | 0.993 | 1.4x | 465.5 |
| **Python** | 1.246 | 1.7x | 15.7 |
| **Ruby** | 2.350 | 3.3x | 30.9 |
| **Lisp** | 3.600 | 5.0x | 38.8 |
| **Haskell** | 3.441 | 4.3x | 2.4 |
| **Julia** | 29.678 | 41.4x | 240.9 |

*Internal timing measures algorithm execution only; Startup measures process/runtime initialization.*

## Resource Metrics

| Language | Memory (KiB) | Lines | Complexity |
|----------|-------------|-------|------------|
| **C** | 1,728 | 101 | 18 |
| **Python** | 11,328 | 38 | 6 |
| **Go** | 19,584 | 51 | 6 |
| **Rust** | 2,112 | 56 | 7 |
| **TypeScript** | 207,372 | 42 | 6 |
| **Ruby** | 12,672 | 25 | 4 |
| **ASM** | 1,344 | 499 | 26 |
| **Lisp** | 31,488 | 56 | 8 |
| **Julia** | 288,192 | 41 | 5 |
| **Haskell** | 8,448 | 55 | 5 |

### Anomalies & Analysis

- **ASM is 8x faster than C internally:** 0.086ms vs 0.717ms. The level-synchronous approach with tight loops provides dramatic speedup.
- **Go/Rust beat C internally:** 0.462ms and 0.487ms vs 0.717ms. All three are faster than C for this DAG traversal.
- **TypeScript internal (1.4x):** Only 0.993ms internally, not 507ms externally. The 466ms startup created the illusion of 229x slowness.
- **Julia internal (41x):** Still slow even internally - JIT doesn't help graph traversal with many map lookups.
- **Ruby line count (25):** Shortest implementation - `Hash.new { |h,k| ... }` auto-memoization handles DP in minimal code.
- **Complexity uniformly low (4-8):** The topological DP algorithm has few branches. This is one of the simplest algorithms.

## Language Notes

| Language | Memoization | Notes |
|----------|-------------|-------|
| **Python** | `functools.lru_cache` | `networkx.topological_sort` |
| **Go** | `map` | Recursive with memo |
| **Rust** | `HashMap` | or `petgraph::algo::toposort` |
| **Ruby** | `Hash.new` with default block | Auto-memoization |
| **Haskell** | **Ideal:** Lazy evaluation = natural memoization | `fix` combinator |

## Assembly Optimizations
- **Linearize DAG:** Sequential memory access for predecessor lists
- **Gather instructions:** `vpgatherdd` for predecessor lookup (potential)
- **Unroll DP loop:** Better pipelining
- **Level-synchronous:** Process all nodes at same level with SIMD
- **Prefetch:** Next level's predecessor lists while processing current

## Interesting Points
- **ASM is 8x faster than C internally:** 0.086ms vs 0.717ms. Level-synchronous approach with tight loops is dramatically faster.
- **Go/Rust beat C:** 0.462ms and 0.487ms vs 0.717ms. Graph traversal favors modern language implementations.
- **TypeScript internal (1.4x) is competitive:** V8 handles DAG traversal well. The 466ms startup distorted perception.
- **Julia internal (41x) is genuinely slow:** JIT doesn't help graph traversal with many map lookups. Not a startup issue.
- **Haskell internal (4.3x):** Reasonable - lazy evaluation provides efficient memoization for DAG traversal.
- Ruby's `Hash.new` with block provides elegant auto-memoization despite 3.3x slower performance.
