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

## Performance Results (Internal Timing)

| Language | Time (ms) | vs C | Startup (ms) |
|----------|-----------|------|--------------|
| **ASM** | 0.138 | 0.005x (fastest) | 1.7 |
| **Rust** | 0.199 | 0.007x | 1.4 |
| **Lisp** | <0.001 | ~0x | 42.7 |
| **Python** | 2.078 | 0.08x | 25.0 |
| **Go** | 2.171 | 0.08x | 2.2 |
| **TypeScript** | 4.651 | 0.17x | 490.1 |
| **Ruby** | 5.708 | 0.21x | 30.5 |
| **C** | 26.693 | 1.0x (baseline) | 1.6 |
| **Julia** | 171.241 | 6.4x | 930.1 |
| **Haskell** | 6992.630 | 262.0x | 2.7 |

*Internal timing measures algorithm execution only; Startup measures process/runtime initialization.*

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

- **ASM/Rust are 200x faster than C:** 0.138ms and 0.199ms vs 26.7ms. DLX vs naive backtracking is the difference.
- **Python internal (0.08x C):** 2.078ms is 13x faster than C's 26.7ms. `exact_cover` library uses efficient C extensions.
- **Julia startup (930ms):** Largest in suite - JIT compilation for complex search is expensive. Internal (6.4x C) is reasonable.
- **Haskell internal (262x):** Genuinely catastrophic. Lazy evaluation creates thunks for every backtracking state, overwhelming GC.
- **C is slow (baseline):** Naive backtracking is inherently inefficient. Algorithm choice (DLX) dominates.
- **ASM line count (1,385):** Second highest - DLX with bitboard operations requires massive manual implementation.

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
- **ASM/Rust are 200x faster than C:** DLX vs naive backtracking. Algorithm choice dominates.
- **Python is 13x faster than C internally:** `exact_cover` library with C extensions beats C's naive implementation.
- **Go is 12x faster than C internally:** 2.171ms vs 26.7ms. Go's DLX implementation is efficient.
- **Haskell internal (262x) is genuinely catastrophic:** Lazy evaluation is wrong paradigm for backtracking. Not a startup issue.
- **Julia internal (6.4x) is reasonable:** The 930ms startup (largest in suite) distorted external timing to 39x.
- The parity pruning catches ~50% of impossible boards before expensive search.
- Exact cover is NP-complete; algorithm choice (DLX vs backtracking) matters enormously.
