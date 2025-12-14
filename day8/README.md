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
| **Go** | 0.105 | 0.003x (fastest!) |
| **Rust** | 0.351 | 0.01x |
| **TypeScript** | 1.621 | 0.05x |
| **Ruby** | 2.689 | 0.08x |
| **ASM** | 26.434 | 0.80x |
| **C** | 32.987 | 1.0x (baseline) |
| **Julia** | 502.076 | 15.2x |
| **Lisp** | 750.227 | 22.7x |
| **Haskell** | 1285.632 | 39.0x |
| **Python** | 1776.771 | 53.9x |

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
- **Go dominates** at 0.105ms - 314x faster than C! The efficient built-in map and slices shine here.
- This is the most dramatic language performance inversion in the suite
- Python with `networkx` is 54x slower - library overhead for small graphs
- ASM (26ms) is close to C (33ms) - the algorithm is dominated by sorting, not low-level optimization
- The Union-Find path compression makes component queries effectively O(1) amortized
- Haskell's immutable data structures are a poor fit for Union-Find's inherently mutable operations
- Julia's JIT compilation overhead may explain its poor performance (502ms)
