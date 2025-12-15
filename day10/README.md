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

## Performance Results (Internal Timing)

| Language | Time (ms) | vs C | Startup (ms) |
|----------|-----------|------|--------------|
| **ASM** | 167.412 | 0.90x (fastest) | 1.9 |
| **C** | 186.462 | 1.0x (baseline) | 2.4 |
| **Go** | 264.359 | 1.4x | 2.7 |
| **Rust** | 302.398 | 1.6x | 2.1 |
| **Python** | 465.135 | 2.5x | 125.8 |
| **TypeScript** | 512.687 | 2.7x | 523.3 |
| **Lisp** | 2153.409 | 11.5x | 8.2 |
| **Haskell** | 3926.412 | 21.7x | 2.3 |
| **Julia** | 7723.529 | 41.4x | 156.7 |
| **Ruby** | 28120.626 | 150.8x | 36.4 |

*Internal timing measures algorithm execution only; Startup measures process/runtime initialization.*

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

- **TypeScript internal (2.7x):** Only 512.7ms internally, not 1067ms externally. The 523ms startup was half the external time.
- **Python internal (2.5x):** 465ms internally is competitive. The 126ms startup is higher than usual due to numpy/scipy imports.
- **Julia internal (41.4x):** 7.7 seconds is genuinely slow, not startup-related. JIT doesn't help complex DFS backtracking.
- **Ruby internal (151x):** 28 seconds is truly catastrophic. The DFS backtracking with interpreted loops is devastating.
- **C complexity (139):** Highest in the entire suite - GF(2) elimination + ILP DFS creates extreme branching.
- **ASM line count (1,085):** Largest in suite - SIMD row operations, DFS state management require manual implementation.

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
- **ASM is only 10% faster than C internally:** 167ms vs 186ms. The DFS state collapse optimization helped, but diminishing returns.
- **TypeScript internal (2.7x) is competitive:** V8 handles the DFS reasonably. The 523ms startup was misleading.
- **Python internal (2.5x) is surprisingly good:** numpy vectorization helps with the GF(2) operations.
- **Ruby is genuinely 151x slower internally:** The DFS backtracking is catastrophic for interpreted loops. Not a startup issue.
- **Julia is genuinely slow (41x) internally:** JIT doesn't help complex branching control flow. Not a startup issue.
- The GF(2) and rational RREF are conceptually similar but require different number representations.
- The DFS state collapse optimization was key: cache locality matters more than SIMD for small data.
