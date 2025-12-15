# Day 7: Tachyon Beam Splitter

## Problem Summary
Simulate beams of light through a grid of mirrors and splitters.
- **Part 1:** Count unique illuminated cells
- **Part 2:** Count total "quantum timelines" (beam paths through splitters)

## Algorithm

### Row-by-Row Simulation (C Implementation)
Instead of generic BFS, the C implementation uses:
1. Process grid row by row, tracking active beam columns per row
2. Beams always move downward; splitters spawn left/right within same row
3. **Part 1:** Track unique illuminated cells with deduplication
4. **Part 2:** Track count of "ways to reach each column" (quantum timelines)
   - When hitting splitter, count doubles (branches into two timelines)
   - Sum counts at final row for total paths

This row-by-row approach is more cache-friendly than generic BFS.

## Performance Results

| Language | Time (ms) | vs C |
|----------|-----------|------|
| **ASM** | 1.996 | 0.75x (fastest) |
| **Rust** | 2.062 | 0.78x |
| **C** | 2.649 | 1.0x (baseline) |
| **Haskell** | 8.046 | 3.0x |
| **Python** | 17.956 | 6.8x |
| **Lisp** | 29.859 | 11.3x |
| **Ruby** | 38.505 | 14.5x |
| **Go** | 56.397 | 21.3x |
| **Julia** | 349.362 | 131.9x |
| **TypeScript** | 510.946 | 192.9x |

## Resource Metrics

| Language | Memory (KiB) | Lines | Complexity |
|----------|-------------|-------|------------|
| **C** | 5,760 | 146 | 34 |
| **Python** | 11,136 | 69 | 18 |
| **Go** | 18,996 | 157 | 25 |
| **Rust** | 2,112 | 86 | 15 |
| **TypeScript** | 212,068 | 70 | 19 |
| **Ruby** | 12,672 | 61 | 11 |
| **ASM** | 1,344 | 357 | 16 |
| **Lisp** | 41,856 | 77 | 20 |
| **Julia** | 305,472 | 71 | 18 |
| **Haskell** | 9,408 | 76 | 11 |

### Anomalies & Analysis

- **Go lines (157):** Most verbose implementation - channel-based parallelism and explicit visited map management add significant code. Yet Go is 21x slower than C.
- **C complexity (34):** High - the row-by-row simulation with splitter handling requires many direction checks and boundary conditions.
- **Rust lines (86):** Compact despite safety - `VecDeque` and `HashSet` provide efficient abstractions without verbosity. 22% faster than C.
- **Ruby/Haskell complexity (11):** Tied for lowest - both use higher-order functions that hide branching in method calls. Ruby pays 14.5x penalty; Haskell only 3x.
- **C memory (5,760 KiB):** Higher than simpler problems - the grid and visited tracking require 2D arrays. Still much less than interpreted languages.

## Language Notes

| Language | BFS Queue | Visited Tracking |
|----------|-----------|------------------|
| **Python** | `collections.deque` | `set` of `(x, y, dir)` tuples |
| **Go** | Channel-based parallel | `map` for visited |
| **Rust** | `VecDeque` | `HashSet<(i32, i32, Dir)>` |
| **TypeScript** | `Map` with string keys | State encoding |
| **Ruby** | `Set` with arrays | `queue.shift` for BFS |
| **Haskell** | `Data.Set` for visited | `Seq` for BFS queue |

## Assembly Optimizations

For row-by-row simulation (matching C):
- **SIMD `vpaddd`:** Parallel column count updates within a row
- **Bitmask packing:** Active columns as bitmask for fast splitter propagation
- **`popcnt`:** Counting active beams per row
- **Branchless splitter handling:** Lookup tables for left/right expansion

For generic BFS alternative:
- Pack direction in 2 bits, position in remaining bits
- Use `bt`/`bts` for bitset visited tracking

## Interesting Points
- ASM and Rust are 25% faster than C - the tight inner loops benefit from optimization
- Go is slow (21x) - its generic hash map may be the bottleneck
- The row-by-row approach is a key algorithmic insight: converts 2D BFS to 1D scan
- Part 2's "quantum timelines" counting doubles at each splitter - exponential growth handled via counting, not enumeration
- Julia and TypeScript show extreme startup overhead (132x-193x) on this problem
