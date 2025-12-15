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

## Performance Results

| Language | Time (ms) | vs C |
|----------|-----------|------|
| **ASM** | 2.330 | 0.89x (fastest) |
| **C** | 2.631 | 1.0x (baseline) |
| **Rust** | 5.196 | 2.0x |
| **Haskell** | 29.573 | 11.2x |
| **Lisp** | 42.303 | 16.1x |
| **Python** | 44.449 | 16.9x |
| **Go** | 54.198 | 20.6x |
| **Ruby** | 107.137 | 40.7x |
| **Julia** | 369.073 | 140.3x |
| **TypeScript** | 552.919 | 210.2x |

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

- **ASM complexity (74):** Highest in Day 4 and second-highest overall - the SIMD bit manipulation (`pshufb`, `pand`, `pcmpeqb`), BFS queue management, and 8-directional neighbor checking each contribute many branch points. This is the cost of hand-optimized performance.
- **ASM line count (913):** 10x more than C (90) - bitfield packing, SIMD operations, and manual queue management require extensive code. Yet it's only 11% faster than C, showing diminishing returns.
- **C complexity (31):** High for C - the 8-directional neighbor counting and BFS wavefront propagation require many conditionals. Grid problems inherently have high branching.
- **Ruby complexity (5):** Extremely low despite implementing BFS - Ruby's iterator-based approach and implicit conditionals in methods hide the true branching. The 40.7x slower runtime reveals the cost.
- **Rust lines (103):** More than C (90) - Rust's explicit error handling, iterator chains, and type annotations add verbosity. The safety guarantees come at a readability cost here.

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
- ASM and C are very close (~11% difference) - the BFS algorithm is well-suited to C's imperative style
- The bitfield representation reduces memory by 8x, improving cache utilization
- `popcnt` is key for both neighbor counting and Part 1 SIMD counting
- The BFS wavefront is algorithmically superior to iteration-based removal
- Julia and TypeScript show extreme overhead (140x-210x slower) due to startup costs on this fast problem
