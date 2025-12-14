# Day 10: Light Toggle Puzzle

## Problem Summary
Grid of lights with toggle rules.
- **Part 1:** Find minimum button presses to turn all lights on (GF(2) linear algebra)
- **Part 2:** Counter puzzle with target values (Integer Linear Programming)

## Algorithm

### Part 1: GF(2) Gaussian Elimination
1. Build matrix where each button toggles certain lights (XOR relationship)
2. Solve system over GF(2) (XOR operations only)
3. Count button presses in solution

GF(2) = Galois Field with 2 elements {0, 1}, where addition = XOR, multiplication = AND.

### Part 2: Integer Linear Programming with DFS
1. Build constraint matrix for counter targets
2. Use rational arithmetic RREF (Reduced Row Echelon Form)
3. DFS/backtracking to find integer solution
4. Minimize total button presses

### Key Implementation Details
- `MAX_LIGHTS` 1024, `MAX_BUTTONS` 256, `ENUM_LIMIT` 20
- Bitsets store button columns in chunks of 64 for GF(2) matrix
- Part 2 uses doubles for a tiny matrix with free variable count <= 3

## Performance Results

| Language | Time (ms) | vs C |
|----------|-----------|------|
| **ASM** | 182.798 | 0.92x (fastest) |
| **C** | 198.527 | 1.0x (baseline) |
| **Go** | 286.949 | 1.44x |
| **Rust** | 305.243 | 1.54x |
| **Python** | 483.436 | 2.44x |
| **TypeScript** | 544.904 | 2.74x |
| **Lisp** | 2218.197 | 11.2x |
| **Haskell** | 4481.797 | 22.6x |
| **Julia** | 7203.888 | 36.3x |
| **Ruby** | 29670.660 | 149x |

## Language Notes

| Language | Matrix Library | ILP Approach |
|----------|----------------|--------------|
| **Python** | `numpy` with `%2` for GF(2) | `scipy.optimize.milp`; `sympy` for rational |
| **Go** | `gonum` | Manual GF(2) implementation |
| **Rust** | `nalgebra` | `good_lp` for ILP |
| **Julia** | **Ideal:** `JuMP.jl` + `HiGHS` | `GaloisFields.jl` for GF(2) |
| **Haskell** | `hmatrix` | `glpk-hs` for ILP |

## Assembly Optimizations

### Part 1 (GF(2) Gaussian Elimination)
- **`pxor` (SSE2):** 128-bit XOR row operations
- **`vpxor` (AVX2):** 256-bit row operations
- **`vpxorq` (AVX-512):** 512-bit operations - process 512 lights per instruction
- **`tzcnt`/`lzcnt`:** Pivot finding in bitpacked rows
- Process 64 columns at once with GPR bitwise operations

### Part 2 (ILP with DFS)
The major ASM optimization was **DFS state collapse**:
- Moved `free_vals[0..3]` from far scratch memory to stack frame `[rsp+32..63]`
- Unrolled sum loop, set_free loop, sub_free loop, increment cascade
- **Result:** 318ms -> 172ms (46% speedup), now faster than C

Additional optimizations:
- **Pivot bitmask:** Track pivot columns for O(1) lookup
- **XOR-swap:** Swap rows without temporary using `xor` chain
- **Row base pointer hoisting:** Avoid recomputing base pointers in inner loops
- AVX2 for row operations (swap, normalize, eliminate) - negligible improvement due to small matrices

## Interesting Points
- This is the most algorithmically complex problem in the suite
- ASM beats C by 8% through careful register allocation and loop unrolling
- Ruby is 149x slower - the DFS backtracking is devastating for interpreted languages
- Julia is surprisingly slow (36x) despite being "fast" - JIT overhead on complex control flow
- The GF(2) and rational RREF are conceptually similar but require different number representations
- AVX2 provided negligible improvement because matrices are small (MAX_COLS=17)
- The DFS state collapse optimization was the key insight: cache locality matters more than SIMD for small data
