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
| **ASM** | 0.680 | 0.61x (fastest) |
| **C** | 1.109 | 1.0x (baseline) |
| **Rust** | 1.772 | 1.60x |
| **Go** | 4.857 | 4.38x |
| **Julia** | 5.725 | 5.16x |
| **TypeScript** | 16.188 | 14.6x |
| **Lisp** | 19.332 | 17.4x |
| **Python** | 24.447 | 22.0x |
| **Haskell** | 26.846 | 24.2x |
| **Ruby** | 30.515 | 27.5x |

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
- ASM achieves 35% improvement over naive optimizations through inlining + full SIMD
- The bitfield representation reduces memory by 8x, improving cache utilization
- `popcnt` is key for both neighbor counting and Part 1 SIMD counting
- The BFS wavefront is algorithmically superior to iteration-based removal
