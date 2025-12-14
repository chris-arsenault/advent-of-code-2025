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
| **ASM** | 0.065 | 0.65x (fastest) |
| **C** | 0.100 | 1.0x (baseline) |
| **Rust** | 0.553 | 5.5x |
| **Python** | 1.314 | 13.1x |
| **Julia** | 1.654 | 16.5x |
| **TypeScript** | 2.084 | 20.8x |
| **Lisp** | 2.529 | 25.3x |
| **Ruby** | 5.980 | 59.8x |
| **Haskell** | 6.101 | 61.0x |
| **Go** | 8.334 | 83.3x |

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
- Go is unusually slow (83x) - its generic hash map may be the bottleneck
- The row-by-row approach is a key algorithmic insight: converts 2D BFS to 1D scan
- Part 2's "quantum timelines" counting doubles at each splitter - exponential growth handled via counting, not enumeration
- ASM's 35% speedup over C comes from tight SIMD inner loops
- Rust at 5.5x is respectable for a safe language with bounds checking
