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
| **Rust** | 0.008 | 0.0003x (fastest!) |
| **ASM** | 0.113 | 0.004x |
| **Go** | 0.505 | 0.016x |
| **TypeScript** | 0.512 | 0.016x |
| **Lisp** | 0.634 | 0.020x |
| **Ruby** | 1.079 | 0.034x |
| **Python** | 2.040 | 0.064x |
| **C** | 31.826 | 1.0x (baseline) |
| **Julia** | 186.738 | 5.87x |
| **Haskell** | 7405.812 | 232.7x |

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
- **Rust is 3980x faster than C!** The most dramatic inversion in the suite.
- This is because Rust uses an efficient DLX library while C uses naive backtracking
- Haskell is 232x slower - lazy evaluation is wrong paradigm for backtracking search
- Julia is slow (5.9x) despite being "fast" - JIT overhead on recursive search
- The parity pruning catches ~50% of impossible boards before expensive search
- Exact cover is NP-complete; the exponential complexity means algorithm choice matters enormously
