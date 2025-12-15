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

## Performance Results

| Language | Time (ms) | vs C |
|----------|-----------|------|
| **ASM** | 1.566 | 0.71x (fastest) |
| **Rust** | 1.904 | 0.86x |
| **C** | 2.216 | 1.0x (baseline) |
| **Haskell** | 5.830 | 2.6x |
| **Lisp** | 14.310 | 6.5x |
| **Python** | 17.896 | 8.1x |
| **Ruby** | 34.748 | 15.7x |
| **Go** | 46.932 | 21.2x |
| **Julia** | 259.183 | 117.0x |
| **TypeScript** | 507.153 | 228.9x |

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

- **Ruby line count (25):** Shortest implementation in the entire suite - Ruby's `Hash.new { |h,k| ... }` auto-memoization pattern handles the entire DP in minimal code. Yet 15.7x slower than C.
- **Complexity uniformly low (4-8):** All high-level languages have similar low complexity - the topological DP algorithm has few branches. This is one of the simplest algorithms in the suite.
- **ASM complexity (26):** Highest - the level-synchronous approach requires explicit queue management and predecessor iteration that high-level languages abstract away.
- **C memory (1,728 KiB):** Very low - the small graph fits in stack-allocated arrays. No dynamic allocation needed.
- **TypeScript timing (229x):** Worst ratio in Day 11 - Node.js startup completely dominates this ~2ms problem. The algorithm itself is fast.
- **Julia timing (117x):** Poor despite simple algorithm - graph building and map lookups incur JIT overhead. Julia excels at numeric computation, not graph traversal.

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
- ASM and Rust are ~15-30% faster than C - tight loops benefit from optimization
- Julia is 117x slower - JIT overhead on graph algorithms hurts
- Haskell performs well (2.6x) - lazy evaluation provides efficient memoization for DAG traversal
- Ruby's `Hash.new` with block provides elegant auto-memoization
- The level-synchronous approach enables SIMD parallelism not possible with recursive DFS
- TypeScript shows extreme startup overhead (229x) on this fast-running problem
