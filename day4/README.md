# Day 4: Paper Rolls Grid

## Problem Summary
Grid of paper rolls (`@`).
- **Part 1:** Count rolls accessible by forklift (fewer than 4 of 8 adjacent neighbors)
- **Part 2:** Iteratively remove accessible rolls until none remain; count total removed

## Algorithm

### Part 1
- Parse grid into 2D array
- Precompute neighbor count for each roll (8-directional)
- Count cells with neighbor count < 4

### Part 2 (BFS Wavefront Removal)
1. Enqueue all initially accessible rolls
2. Dequeue a roll, remove it, increment count
3. Decrement neighbor counts of adjacent rolls
4. If any neighbor's count drops below 4, enqueue it
5. Repeat until queue empty

**Complexity:** O(rows * cols) - each cell processed at most once

### Key Insight
The naive approach (rescan entire grid each iteration) is O(iterations * n). BFS ensures O(n) by only processing cells when they become newly accessible.

## Performance Results (Internal Timing)

| Language | Time (ms) | vs C | Startup (ms) |
|----------|-----------|------|--------------|
| **ASM** | 0.753 | 0.68x (fastest) | 1.4 |
| **C** | 1.111 | 1.0x (baseline) | 1.3 |
| **Rust** | 3.448 | 3.1x | 1.6 |
| **Julia** | 5.994 | 5.4x | 382.5 |
| **Go** | 9.357 | 8.4x | 2.5 |
| **Lisp** | 18.646 | 16.8x | 41.6 |
| **Python** | 25.966 | 23.4x | 17.7 |
| **Haskell** | 28.661 | 25.8x | 0.6 |
| **TypeScript** | 32.707 | 29.4x | 499.9 |
| **Ruby** | 72.318 | 65.1x | 32.3 |

*Internal timing measures algorithm execution only; Startup measures process/runtime initialization.*

## Resource Metrics

| Language | Memory (KiB) | Lines | Complexity |
|----------|-------------|-------|------------|
| **C** | 3,264 | 90 | 31 |
| **Python** | 14,292 | 58 | 19 |
| **Go** | 19,372 | 92 | 18 |
| **Rust** | 2,880 | 103 | 16 |
| **TypeScript** | 215,528 | 83 | 14 |
| **Ruby** | 16,716 | 60 | 5 |
| **ASM** | 1,536 | 913 | 74 |
| **Lisp** | 52,224 | 70 | 13 |
| **Julia** | 305,604 | 59 | 14 |
| **Haskell** | 13,056 | 62 | 8 |

### Anomalies & Analysis

- **Julia is competitive internally:** 5.994ms (5.4x C) is reasonable. The 383ms startup made it appear 140x slower in external timing.
- **TypeScript internal (32.7ms):** Actually slower than most languages internally. V8 doesn't optimize BFS/grid operations as well as numeric workloads.
- **Haskell has lowest startup (0.6ms):** The GHC-compiled binary has minimal runtime initialization, but the algorithm runs 26x slower than C.
- **ASM complexity (74):** Highest in Day 4 - the SIMD bit manipulation (`pshufb`, `pand`, `pcmpeqb`), BFS queue management, and 8-directional neighbor checking each contribute many branch points.
- **ASM line count (913):** 10x more than C (90) - bitfield packing, SIMD operations, and manual queue management require extensive code. Only 32% faster than C internally.
- **Ruby complexity (5):** Extremely low despite implementing BFS - Ruby's iterator-based approach hides branching. The 65x slower runtime reveals the abstraction cost.

## Language Notes

| Language | BFS Queue | Grid Representation |
|----------|-----------|---------------------|
| **Python** | `collections.deque` | NumPy + `scipy.ndimage.convolve` |
| **Go** | Slice-based | 2D slice |
| **Rust** | `VecDeque` | `ndarray` or flat `Vec` |
| **TypeScript** | Array (shift/push) | `Map` for neighbor counts |
| **Haskell** | `Data.Sequence` | `Data.Set`/`Data.Map.Strict` for sparse |

## Assembly Optimizations

**Data Structures:**
- **Bitfield-packed arrays:** `grid_bits` and `removed_bits` use 1 bit per cell (8KB instead of 64KB each)
- **Byte array for counts:** `counts[]` as bytes for efficient BFS update
- **Cache-line alignment:** All arrays aligned to 64-byte boundaries
- **Packed BFS queue:** Entries as `(row<<8 | col)` in 16-bit words, halving memory traffic

**Parsing Phase:**
- **`bts` instruction:** Atomic-style bit-set replaces `shl`/`or` pattern

**Neighbor Counting (Inlined):**
- **`setc` pattern:** Branchless bitmask building
- Build 8-bit bitmask of present neighbors, then single `popcnt` counts all
- **Critical:** Inlined into hot loop (function call overhead was the biggest bottleneck)

**Part 1 (Full SIMD):**
- **Branchless bit expansion:** `pshufb` + `pand` + `pcmpeqb` converts 16-bit packed grid row to 16-byte vector
- **`pmovmskb` + `popcnt`:** After masking with neighbor counts < 4, extract and count in ~6-8 instructions per 16 cells

**BFS Removal:**
- **Hoisted `imul`:** Row byte offsets computed once per row via `add`, not per cell

## Interesting Points
- **ASM is only 32% faster than C internally:** 0.753ms vs 1.111ms. The BFS algorithm is well-suited to C's imperative style.
- **Julia startup misleads:** 5.4x C internally, but appeared 140x slower due to 383ms startup overhead.
- **TypeScript is genuinely slow here:** 32.7ms (29x C) internally - V8 doesn't optimize grid/BFS operations well.
- The bitfield representation reduces memory by 8x, improving cache utilization
- `popcnt` is key for both neighbor counting and Part 1 SIMD counting
- The BFS wavefront is algorithmically superior to iteration-based removal
