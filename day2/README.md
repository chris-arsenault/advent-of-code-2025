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
| **ASM** | 3.239 | 0.03x | 33x faster - precomputation |
| **Rust** | 8.364 | 0.08x | precomputation |
| **Haskell** | 35.155 | 0.33x | precomputation |
| **Lisp** | 60.064 | 0.57x | precomputation |
| **Ruby** | 70.564 | 0.67x | precomputation |
| **Go** | 71.724 | 0.68x | precomputation |
| **Python** | 74.159 | 0.70x | precomputation |
| **C** | 105.829 | 1.0x | per-number checking |
| **Julia** | 351.906 | 3.32x | precomputation |
| **TypeScript** | 548.647 | 5.18x | precomputation |

## Resource Metrics

| Language | Memory (KiB) | Lines | Complexity |
|----------|-------------|-------|------------|
| **C** | 1,920 | 83 | 16 |
| **Python** | 24,740 | 69 | 16 |
| **Go** | 19,380 | 102 | 16 |
| **Rust** | 5,136 | 95 | 12 |
| **TypeScript** | 222,128 | 78 | 14 |
| **Ruby** | 18,464 | 69 | 7 |
| **ASM** | 3,456 | 537 | 12 |
| **Lisp** | 59,136 | 119 | 19 |
| **Julia** | 317,568 | 94 | 17 |
| **Haskell** | 19,164 | 109 | 5 |

### Anomalies & Analysis

- **ASM line count (537):** 6.5x more than C (83) - precomputation with sorting, binary search, and prefix sums requires extensive manual implementation. The shared utility functions help but still require setup code.
- **Haskell complexity (5):** Lowest despite 109 lines - Haskell's declarative style with list comprehensions and pure functions produces code with few branch points. The complexity metric undercounts functional composition.
- **Ruby complexity (7):** Second lowest - Ruby's iterator-based approach (`each`, `map`, `select`) hides complexity in method calls rather than explicit branches.
- **Lisp memory (59 MB):** SBCL's runtime and standard library are substantial. The image-based model loads more than strictly necessary for this problem.
- **Julia memory (318 MB):** Highest overall - the full JIT compiler and numeric libraries remain resident even for simple string/integer work.

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
- Significant algorithmic improvement: ~33x speedup from precomputation (ASM vs C)
- The C reference deliberately uses the naive approach to demonstrate the contrast
- ASM benefits from tight control over sorting and binary search hot paths
- Ruby's `bsearch_index` provides efficient built-in binary search
- Julia and TypeScript show high overhead despite using precomputation - startup costs dominate
