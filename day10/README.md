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
| **ASM** | 160.740 | 0.80x (fastest) |
| **C** | 201.466 | 1.0x (baseline) |
| **Rust** | 311.147 | 1.5x |
| **Go** | 345.560 | 1.7x |
| **Python** | 580.836 | 2.9x |
| **TypeScript** | 1067.511 | 5.3x |
| **Lisp** | 2339.611 | 11.6x |
| **Haskell** | 4109.113 | 20.4x |
| **Julia** | 7912.589 | 39.3x |
| **Ruby** | 29046.014 | 144.2x |

## Resource Metrics

| Language | Memory (KiB) | Lines | Complexity |
|----------|-------------|-------|------------|
| **C** | 4,032 | 408 | 139 |
| **Python** | 31,680 | 114 | 46 |
| **Go** | 19,944 | 375 | 101 |
| **Rust** | 4,032 | 338 | 58 |
| **TypeScript** | 219,556 | 186 | 58 |
| **Ruby** | 12,864 | 156 | 35 |
| **ASM** | 1,344 | 1,085 | 56 |
| **Lisp** | 88,280 | 260 | 101 |
| **Julia** | 374,748 | 269 | 88 |
| **Haskell** | 10,176 | 312 | 45 |

### Anomalies & Analysis

- **C complexity (139):** Highest in the entire suite - GF(2) Gaussian elimination combined with ILP DFS backtracking creates extreme branching. The algorithm is inherently complex.
- **C line count (408):** Highest for C - manual matrix operations, pivot selection, and backtracking enumeration require extensive code. This is the most complex C implementation.
- **ASM line count (1,085):** Largest in suite - SIMD row operations, DFS state management, and rational arithmetic all require manual implementation. 2.7x more than C.
- **Lisp complexity (101):** Matches Go - the macro-heavy implementation with rational arithmetic doesn't reduce branching like other Lisp strengths (recursion, list processing).
- **Ruby timing (144x):** Second worst performance ratio - the DFS backtracking with interpreted loops is catastrophic. Each backtrack requires thousands of operations.
- **Lisp memory (88 MB):** High - rational arithmetic creates many intermediate bignum objects. SBCL's GC pressure is significant during the ILP solve.

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
- ASM beats C by 20% through careful register allocation and loop unrolling
- Ruby is 144x slower - the DFS backtracking is devastating for interpreted languages
- Julia is surprisingly slow (39x) despite being "fast" - JIT overhead on complex control flow
- The GF(2) and rational RREF are conceptually similar but require different number representations
- AVX2 provided negligible improvement because matrices are small (MAX_COLS=17)
- The DFS state collapse optimization was the key insight: cache locality matters more than SIMD for small data
