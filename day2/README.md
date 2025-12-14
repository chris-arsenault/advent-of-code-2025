# Day 2: Gift Shop (Invalid Product IDs)

## Problem Summary
Find IDs within given ranges that consist of a digit sequence repeated k times.
- **Part 1:** k=2 (e.g., "1212", "123123")
- **Part 2:** k >= 2 (any repetition count)

Sum all invalid IDs in each case.

## Algorithm

### C Reference (Per-Number Checking)
- For each number in range, convert to string
- Try each divisor of length, check if first `len/k` digits repeat exactly k times
- O(range_size * digit_checks) - slow for large ranges

### Optimized Approach (Precomputation)
Most implementations use precomputation:
1. Generate all k-repeated patterns up to max range value
2. Store in sorted array (~100k-1M numbers)
3. For each range, binary search to count matches
4. O(patterns) + O(queries * log(patterns)) - much faster

## Performance Results

| Language | Time (ms) | vs C | Notes |
|----------|-----------|------|-------|
| **ASM** | 1.428 | 0.01x | 74x faster - precomputation |
| **Rust** | 6.719 | 0.06x | precomputation |
| **Julia** | 14.027 | 0.13x | precomputation |
| **Go** | 19.385 | 0.18x | precomputation |
| **Lisp** | 30.931 | 0.29x | precomputation |
| **Haskell** | 32.932 | 0.31x | precomputation |
| **Ruby** | 36.169 | 0.34x | precomputation |
| **TypeScript** | 42.467 | 0.40x | precomputation |
| **Python** | 55.059 | 0.52x | precomputation |
| **C** | 105.904 | 1.0x | per-number checking |

## Language Notes

| Language | Pattern Generation | Range Query |
|----------|-------------------|-------------|
| **Python** | `base_str * k` | `bisect` binary search |
| **Go** | `strings.Repeat` | `sort.Search` |
| **Rust** | `str.repeat(k)` | `partition_point` |
| **Ruby** | `str * k` | `bsearch_index` |
| **Julia** | `repeat(s, k)` | `searchsortedfirst` |
| **Haskell** | List comprehension | `Data.Vector` + binary search |

### Alternative: Regex
For checking individual numbers: `re.fullmatch(r'(.+)\1+', str(n))` in Python, `/^(.+)\1+$/` in JS/Ruby

## Assembly Optimizations

The ASM uses the precomputation strategy with:
- **Pattern generation:** Generate all even-half and periodic patterns up to max range value
- **Hybrid quicksort:** Median-of-three pivot + insertion sort for small subarrays
- **Binary search:** `lower_bound_u64`, `upper_bound_u64` from shared utils
- **Prefix sums:** O(1) range sum queries after O(n) preprocessing

**Shared utilities used:** `pow10`, `lower_bound_u64`, `upper_bound_u64`, `sort_u64`

## Interesting Points
- This is the largest algorithmic improvement in the suite: 74x speedup from precomputation
- The C reference deliberately uses the naive approach to demonstrate the contrast
- ASM benefits from tight control over sorting and binary search hot paths
- Ruby's `bsearch_index` provides efficient built-in binary search
