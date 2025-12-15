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

## Performance Results

| Language | Time (ms) | vs C |
|----------|-----------|------|
| **ASM** | 29.608 | 0.83x (fastest) |
| **Rust** | 32.147 | 0.91x |
| **C** | 35.470 | 1.0x (baseline) |
| **Go** | 139.210 | 3.9x |
| **Ruby** | 189.184 | 5.3x |
| **TypeScript** | 695.467 | 19.6x |
| **Lisp** | 743.877 | 21.0x |
| **Haskell** | 1207.803 | 34.1x |
| **Julia** | 1919.109 | 54.1x |
| **Python** | 1944.205 | 54.8x |

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

- **Python memory (257 MB):** Highest for Python across all days - `networkx` library loads extensive graph infrastructure. The library provides convenience at significant memory cost.
- **Haskell memory (181 MB):** Unusually high - immutable Union-Find requires copying on each update, and the MST algorithm generates many intermediate structures. This is a pathological case for functional programming.
- **Rust memory (31 MB):** Higher than usual for Rust - `petgraph` crate or custom Union-Find with all edges stored adds overhead. Still 8x less than Python.
- **Go memory (46 MB):** Higher than other Go days - storing all edges for Kruskal's and the Union-Find structure requires heap allocation.
- **Python lines (50):** Very concise - `networkx` handles graph construction, MST, and component counting. The brevity hides 55x slower performance.
- **Ruby/Haskell complexity (7):** Lowest - both rely on library abstractions (`Set`, `IntMap`) that internalize branching logic.

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
- ASM, Rust, and C are within 20% of each other - the algorithm is dominated by sorting, not low-level optimization
- Python with `networkx` is 55x slower - library overhead for graph operations
- The Union-Find path compression makes component queries effectively O(1) amortized
- Haskell's immutable data structures are a poor fit for Union-Find's inherently mutable operations
- Julia and Python are both ~55x slower - JIT compilation overhead and interpreted loops hurt on graph algorithms
