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

## Performance Results (Internal Timing)

| Language | Time (ms) | vs C | Startup (ms) |
|----------|-----------|------|--------------|
| **ASM** | 0.098 | 0.69x (fastest) | 1.3 |
| **C** | 0.142 | 1.0x (baseline) | 2.8 |
| **Rust** | 0.528 | 3.7x | 1.7 |
| **Python** | 1.522 | 10.7x | 17.3 |
| **Julia** | 1.680 | 11.8x | 362.2 |
| **TypeScript** | 2.130 | 15.0x | 500.6 |
| **Lisp** | 3.718 | 26.2x | 41.9 |
| **Ruby** | 4.981 | 35.1x | 33.1 |
| **Haskell** | 6.396 | 45.0x | 1.7 |
| **Go** | 9.117 | 64.2x | 3.0 |

*Internal timing measures algorithm execution only; Startup measures process/runtime initialization.*

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

- **Go is surprisingly slow internally (64x):** 9.117ms vs C's 0.142ms. Go's hash map overhead is visible on this grid simulation problem.
- **Julia is competitive internally:** 1.680ms (11.8x) is reasonable. The 362ms startup made it appear 132x slower in external timing.
- **Haskell internal (45x):** Slower than Python (10.7x) on this problem. Lazy evaluation doesn't help for strict grid simulation.
- **TypeScript internal (15x):** Better than external timing suggested. V8 handles this beam simulation reasonably well.
- **Rust is 3.7x C internally:** Less impressive than expected - the row-by-row simulation may have overhead from `VecDeque`/`HashSet`.
- **C memory (5,760 KiB):** Higher than simpler problems - the grid and visited tracking require 2D arrays.

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
- **ASM is only 31% faster than C internally:** 0.098ms vs 0.142ms. The row-by-row algorithm is simple enough that C optimizes well.
- **Go internal (64x) is genuinely slow:** Not just startup - hash map overhead on grid operations is significant.
- **Julia startup distorts perception:** 11.8x C internally vs 132x externally. The 362ms startup dominates.
- **Python beats Go internally:** 1.522ms (10.7x) vs Go's 9.117ms (64x). Go's overhead is problem-specific, not general.
- The row-by-row approach is a key algorithmic insight: converts 2D BFS to 1D scan.
- Part 2's "quantum timelines" counting doubles at each splitter - exponential growth handled via counting, not enumeration.
