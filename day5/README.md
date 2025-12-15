# Day 5: Fresh Paint (Interval Merging)

## Problem Summary
Given painted intervals with IDs, find "fresh" IDs (those whose paint is still visible after all painting).
- **Part 1:** Return smallest fresh ID
- **Part 2:** Count all fresh IDs

## Algorithm
1. Parse all intervals as (start, end, id) tuples
2. Sort by start position
3. Merge overlapping intervals, tracking which IDs get covered
4. Part 1: Binary search to find smallest uncovered ID
5. Part 2: Count all IDs that remain visible after merging

### Two-Pointer Alternative
For Part 1, instead of binary search per ID:
1. Sort ID list once: O(ids * log ids)
2. Merge-walk sorted IDs with sorted intervals using two pointers
3. Complexity: O(ids * log ids + ids + intervals) vs O(ids * log intervals)

For 698 IDs and ~100 merged intervals: ~800 comparisons vs ~4600

## Performance Results (Internal Timing)

| Language | Time (ms) | vs C | Startup (ms) |
|----------|-----------|------|--------------|
| **Haskell** | 3.197 | 20.8x | 2.3 |
| **ASM** | 0.103 | 0.62x | 1.6 |
| **Rust** | 0.109 | 0.66x | 1.3 |
| **C** | 0.165 | 1.0x (baseline) | 1.4 |
| **Go** | 0.191 | 1.16x | 2.1 |
| **Julia** | 0.327 | 1.98x | 440.6 |
| **TypeScript** | 0.875 | 5.30x | 496.9 |
| **Ruby** | 1.518 | 9.20x | 32.1 |
| **Lisp** | <0.001 | ~0x | 44.9 |
| **Python** | 3.223 | 19.5x | 21.9 |

*Internal timing measures algorithm execution only; Startup measures process/runtime initialization.*

## Resource Metrics

| Language | Memory (KiB) | Lines | Complexity |
|----------|-------------|-------|------------|
| **C** | 1,536 | 137 | 27 |
| **Python** | 12,480 | 30 | 9 |
| **Go** | 18,616 | 85 | 17 |
| **Rust** | 2,112 | 81 | 12 |
| **TypeScript** | 209,824 | 65 | 8 |
| **Ruby** | 12,672 | 35 | 10 |
| **ASM** | 1,344 | 565 | 21 |
| **Lisp** | 29,952 | 75 | 12 |
| **Julia** | 320,640 | 63 | 17 |
| **Haskell** | 8,256 | 59 | 6 |

### Anomalies & Analysis

- **ASM/Rust are fastest:** 0.103ms and 0.109ms (0.62-0.66x C) - tight interval merging loops benefit from low-level optimization.
- **Most compiled languages are sub-millisecond:** C (0.165ms), Rust (0.109ms), Go (0.191ms), ASM (0.103ms) all finish in under 200μs internally.
- **Julia is competitive:** 0.327ms (1.98x C) - JIT-compiled interval operations work well. The 441ms startup was hiding this efficiency.
- **TypeScript internal (0.875ms):** Only 5.3x C internally, not 305x. The 497ms startup created the illusion of massive slowness.
- **Python line count (30):** Shortest implementation - `sortedcontainers` library handles interval merging. Internal timing of 3.2ms is 19.5x C.
- **C complexity (27):** Highest - the two-pointer merge algorithm requires many boundary checks. Interval problems have inherent branching.

## Language Notes

| Language | Data Structure | Interval Operations |
|----------|----------------|---------------------|
| **Python** | `sortedcontainers.SortedList` | or `intervaltree` library |
| **Go** | Sorted slice | Two-pointer technique |
| **Rust** | `BTreeMap` | Range queries |
| **Ruby** | `sort_by` | `each_cons(2)` for pairwise comparison |
| **Haskell** | `Data.IntervalMap` | or `Data.Set` operations |

## Assembly Optimizations
- **`cmov` for branchless min/max** in merge operations
- Cache-aligned interval storage
- Potential for AVX-512 masked comparisons for batch interval operations

## Interesting Points
- **ASM/Rust are fastest:** 0.103ms and 0.109ms (0.62-0.66x C) - tight loops benefit from low-level optimization.
- **Most compiled languages complete in <200μs:** This sub-millisecond problem amplifies startup differences.
- **Julia/TypeScript startup dominates:** Both complete in <1ms internally but have 440-497ms startup overhead.
- **Go is fast internally:** 0.191ms (1.16x C), not the 25.6x external timing suggested.
- **Python is slowest:** 3.2ms (19.5x C) - interpreted loop overhead shows on this simple algorithm.
- The two-pointer merge technique enables O(n) complexity for interval queries.
