# Day 8: 3D Circuits (Union-Find)

## Problem Summary
3D points with connections.
- **Part 1:** Count connected components
- **Part 2:** Find minimum spanning tree edge weight sum

## Algorithm

### Union-Find + Kruskal's MST
1. **Union-Find (Disjoint Set Union)** with path compression for component tracking
2. Sort edges by weight (Manhattan distance)
3. Apply **Kruskal's algorithm:** add edges in weight order if they connect different components
4. Part 1: Count distinct roots after processing
5. Part 2: Sum of MST edge weights

## Performance Results (Internal Timing)

| Language | Time (ms) | vs C | Startup (ms) |
|----------|-----------|------|--------------|
| **ASM** | 25.661 | 0.79x (fastest) | 2.1 |
| **Rust** | 30.879 | 0.94x | 2.9 |
| **C** | 32.674 | 1.0x (baseline) | 2.2 |
| **Go** | 91.806 | 2.8x | 4.5 |
| **Ruby** | 146.075 | 4.5x | 36.7 |
| **TypeScript** | 208.474 | 6.4x | 484.9 |
| **Lisp** | 721.355 | 22.1x | 42.7 |
| **Julia** | 915.638 | 28.0x | 1013.3 |
| **Haskell** | 1123.198 | 35.8x | 11.2 |
| **Python** | 1732.907 | 53.0x | 174.3 |

*Internal timing measures algorithm execution only; Startup measures process/runtime initialization.*

## Resource Metrics

| Language | Memory (KiB) | Lines | Complexity |
|----------|-------------|-------|------------|
| **C** | 9,216 | 153 | 28 |
| **Python** | 256,776 | 50 | 11 |
| **Go** | 46,572 | 109 | 16 |
| **Rust** | 31,104 | 66 | 9 |
| **TypeScript** | 257,988 | 82 | 14 |
| **Ruby** | 47,232 | 66 | 7 |
| **ASM** | 9,216 | 392 | 14 |
| **Lisp** | 68,544 | 104 | 19 |
| **Julia** | 436,724 | 68 | 13 |
| **Haskell** | 181,440 | 84 | 7 |

### Anomalies & Analysis

- **Julia startup (1013ms):** Largest in suite - JIT compilation for graph algorithms is expensive. Internal timing (915.6ms, 28x C) is better than external suggested.
- **Haskell internal (35.8x):** Union-Find with immutable data structures requires copying on updates, creating significant overhead.
- **Python internal (53x):** Truly slow, not just startup. `networkx` library has significant overhead for these operations.
- **TypeScript internal (6.4x):** Much better than external (19.6x). The 485ms startup dominated external measurement.
- **Python memory (257 MB):** `networkx` library loads extensive graph infrastructure. Convenience at significant memory cost.
- **Haskell memory (181 MB):** Immutable Union-Find requires copying on each update. Pathological case for functional programming.

## Language Notes

| Language | Union-Find | Graph Library |
|----------|------------|---------------|
| **Python** | Custom | `networkx`: `connected_components()`, `minimum_spanning_tree()` |
| **Go** | Custom struct | Manual implementation |
| **Rust** | Custom | `petgraph` crate or `union-find` crate |
| **TypeScript** | Class-based | Manual with path compression |
| **Ruby** | Hash for parent | Manual implementation |
| **Haskell** | `Data.IntMap` | `Data.Graph` or `fgl` package |

## Assembly Optimizations
- Flatten 3D coordinates to single index for cache locality
- Use `rep stosd` for fast array initialization
- Cache-oblivious Union-Find memory layout

## Interesting Points
- **ASM, Rust, C within 21%:** 25.7ms, 30.9ms, 32.7ms. Sorting dominates, so low-level optimization has limited impact.
- **Julia's 1013ms startup is largest in suite:** Internal timing (28x C) is reasonable for graph algorithms.
- **Haskell is slow (35.8x):** Union-Find with immutable data structures requires copying on updates. Pathological case for functional programming.
- **TypeScript is 6.4x C internally:** V8 handles graph algorithms reasonably. The 485ms startup inflated external timing.
- **Python is genuinely slow (53x):** `networkx` library overhead is real, not startup-related.
- Union-Find path compression makes component queries effectively O(1) amortized.
