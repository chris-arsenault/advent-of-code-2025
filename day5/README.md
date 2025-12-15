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
| **ASM** | 1.593 | 0.95x (fastest) |
| **C** | 1.682 | 1.0x (baseline) |
| **Rust** | 1.721 | 1.02x |
| **Haskell** | 5.431 | 3.2x |
| **Lisp** | 12.565 | 7.5x |
| **Python** | 24.504 | 14.6x |
| **Ruby** | 34.876 | 20.7x |
| **Go** | 43.109 | 25.6x |
| **Julia** | 420.637 | 250.1x |
| **TypeScript** | 513.188 | 305.1x |

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

- **Python line count (30):** Shortest implementation - `sortedcontainers` library handles interval merging, and Python's concise syntax eliminates boilerplate. Yet 14.6x slower than C.
- **C line count (137):** Longest among high-level languages - manual interval merging with explicit memory management. The verbose code produces fast execution (1.68ms).
- **C complexity (27):** Highest - the two-pointer merge algorithm requires many boundary checks. Interval problems have inherent branching complexity.
- **Haskell complexity (6):** Low despite efficient runtime (3.2x C) - functional composition with `Data.Set` reduces explicit branches while maintaining good performance.
- **TypeScript timing (305x):** Worst in suite - Node.js startup dominates this 1.6ms problem. The actual algorithm is fast once running.

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
- ASM, C, and Rust are nearly identical in performance (within 7%) - the algorithm is simple and well-optimized by compilers
- Haskell performs well (3.2x) with efficient `Data.Set` operations for interval handling
- Go is surprisingly slow (25.6x) on this interval problem - hash map overhead may be a factor
- Julia and TypeScript show extreme startup overhead (250x-305x) on this fast-running problem
