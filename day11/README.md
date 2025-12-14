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
| **ASM** | 0.026 | 0.81x (fastest) |
| **C** | 0.032 | 1.0x (baseline) |
| **Rust** | 0.208 | 6.5x |
| **Go** | 0.274 | 8.6x |
| **Lisp** | 0.777 | 24.3x |
| **TypeScript** | 0.856 | 26.8x |
| **Ruby** | 1.010 | 31.6x |
| **Python** | 1.058 | 33.1x |
| **Haskell** | 3.741 | 116.9x |
| **Julia** | 33.130 | 1035x |

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
- ASM is only 20% faster than C - the problem is too small for low-level optimization
- **Julia is 1035x slower** - the worst relative performance in the suite! JIT overhead on graph algorithms
- Haskell's lazy evaluation should theoretically provide free memoization, but it's still 117x slower
- Ruby's `Hash.new` with block provides elegant auto-memoization
- The level-synchronous approach enables SIMD parallelism not possible with recursive DFS
- This is one of the fastest problems overall (<1ms for most languages)
