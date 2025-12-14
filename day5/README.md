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

## Performance Results

| Language | Time (ms) | vs C |
|----------|-----------|------|
| **Haskell** | 0.003 | 0.09x (fastest!) |
| **C** | 0.035 | 1.0x (baseline) |
| **ASM** | 0.082 | 2.3x |
| **Rust** | 0.118 | 3.4x |
| **Go** | 0.204 | 5.8x |
| **Ruby** | 0.244 | 7.0x |
| **Julia** | 0.252 | 7.2x |
| **Lisp** | 0.618 | 17.7x |
| **TypeScript** | 0.850 | 24.3x |
| **Python** | 3.363 | 96.1x |

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
- **Haskell wins dramatically** at 0.003ms - 10x faster than C!
- This is due to lazy evaluation + efficient `Data.Set` operations
- Haskell's purely functional approach excels at set-based interval problems
- ASM is actually slower than C here - the problem is memory-bound and high-level optimizations matter more
- Ruby performs surprisingly well (0.244ms) due to efficient `bsearch_index`
