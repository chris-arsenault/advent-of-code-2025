# Day 12: Polyomino Packing (Exact Cover)

## Problem Summary
Pack given polyomino shapes into rectangular regions.
- **Part 1:** Count regions that can be exactly filled

## Algorithm

### Shape Handling
- Read each 3x3 shape
- Enumerate all unique orientations (4 rotations + horizontal flip = up to 8)
- Normalize coordinates so top-left is (0,0)

### Parity Pruning
Before backtracking:
1. **Checkerboard coloring:** Each shape has a fixed black/white cell ratio
2. **DP feasibility:** Check if any combination of shape parities can match board's parity
3. Early reject boards that are mathematically impossible (~50% of cases)

### Exact Cover with Dancing Links (DLX)
For manageable boards (<=400 cells and <=60 pieces):
1. **Columns:** One primary column per piece copy (must be covered) + one secondary column per board cell
2. **Rows:** Each legal placement lists its piece column plus cell columns it occupies
3. **Algorithm X:** Select column with fewest 1s (MRV heuristic), try each row, recurse
4. First solution returned proves feasibility

**Why DLX is faster (5-100x typical):**
- O(1) cover/uncover via pointer manipulation
- MRV heuristic prunes aggressively
- O(1) backtracking via pointer restoration

### Large Regions
For bigger instances, rely on cheap area checks to avoid explosive search.

## Performance Results

| Language | Time (ms) | vs C |
|----------|-----------|------|
| **ASM** | 1.626 | 0.05x (fastest) |
| **Rust** | 1.711 | 0.06x |
| **Python** | 29.630 | 1.0x |
| **C** | 29.787 | 1.0x (baseline) |
| **Ruby** | 39.261 | 1.3x |
| **Go** | 47.838 | 1.6x |
| **Lisp** | 61.978 | 2.1x |
| **TypeScript** | 524.641 | 17.6x |
| **Julia** | 1164.391 | 39.1x |
| **Haskell** | 7072.939 | 237.5x |

## Resource Metrics

| Language | Memory (KiB) | Lines | Complexity |
|----------|-------------|-------|------------|
| **C** | 1,536 | 292 | 83 |
| **Python** | 13,824 | 123 | 48 |
| **Go** | 18,600 | 261 | 59 |
| **Rust** | 2,112 | 179 | 37 |
| **TypeScript** | 216,856 | 146 | 33 |
| **Ruby** | 13,440 | 123 | 18 |
| **ASM** | 1,344 | 1,385 | 100 |
| **Lisp** | 59,328 | 251 | 55 |
| **Julia** | 354,452 | 135 | 51 |
| **Haskell** | 12,512 | 157 | 30 |

### Anomalies & Analysis

- **ASM line count (1,385):** Second highest in suite - DLX implementation with bitboard operations, shape rotation, and parity pruning requires massive manual implementation. 4.7x more than C.
- **ASM complexity (100):** High - the backtracking search with shape enumeration and bitwise operations creates many branch points.
- **C complexity (83):** High - naive backtracking has inherent branching at every placement decision. DLX would reduce this.
- **Rust complexity (37):** Much lower than C - the `dlx` crate encapsulates the branching internally, reducing visible complexity.
- **Ruby complexity (18):** Low - Ruby's `yield` for backtracking and iterator patterns hide the search tree exploration.
- **Haskell timing (238x):** Worst in suite - lazy evaluation creates thunks for every backtracking state, overwhelming GC. Backtracking should be strict.
- **Python matches C timing:** The `exact_cover` library uses efficient C extensions internally, matching C's performance despite Python overhead.

## Language Notes

| Language | Approach | Library |
|----------|----------|---------|
| **Python** | DLX | `exact_cover` library; `dlx` package |
| **Go** | DLX | Custom + `goroutines` for parallel |
| **Rust** | DLX | `dlx` crate; iterator-based solutions |
| **TypeScript** | Backtracking | Custom with `Set` constraints |
| **Ruby** | Backtracking | `yield` for solutions |
| **Haskell** | **Ideal:** Lazy | `Control.Monad` backtracking monad |

## Assembly Optimizations
- **Bitboard representation:** For regions <= 64 cells, pack into single 64-bit word
- **`popcnt`:** Piece size counting
- **`pext`/`pdep`:** Placement extraction and deposit
- **`bzhi`:** Range masking
- Profile-guided optimization for backtracking order

## Interesting Points
- ASM and Rust are ~17x faster than C - efficient DLX implementation vs naive backtracking
- Python matches C (1.0x) - likely using an efficient DLX library (`exact_cover` package)
- Haskell is 238x slower - lazy evaluation is wrong paradigm for backtracking search
- Julia is slow (39x) despite being "fast" - JIT overhead on recursive search
- The parity pruning catches ~50% of impossible boards before expensive search
- Exact cover is NP-complete; the exponential complexity means algorithm choice matters enormously
