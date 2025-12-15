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

## Performance Results (Internal Timing)

| Language | Time (ms) | vs C | Startup (ms) | Notes |
|----------|-----------|------|--------------|-------|
| **ASM** | 1.343 | 0.01x | 1.7 | 76x faster - precomputation |
| **Rust** | 6.496 | 0.06x | 1.7 | precomputation |
| **Julia** | 11.393 | 0.11x | 344.1 | precomputation |
| **Go** | 20.088 | 0.20x | 3.0 | precomputation |
| **Haskell** | 33.547 | 0.33x | 1.2 | precomputation |
| **Lisp** | 33.563 | 0.33x | 42.5 | precomputation |
| **Ruby** | 35.882 | 0.35x | 32.4 | precomputation |
| **TypeScript** | 37.891 | 0.37x | 505.5 | precomputation |
| **Python** | 55.773 | 0.55x | 16.9 | precomputation |
| **C** | 101.924 | 1.0x | 1.7 | per-number checking |

*Internal timing measures algorithm execution only; Startup measures process/runtime initialization.*

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

- **TypeScript startup (506ms):** Massive JIT overhead, but internal timing of 37.9ms (0.37x C) is competitive. TypeScript with precomputation beats C's per-number approach once running.
- **Julia internal (11.4ms):** With precomputation, Julia is 9x faster than C internally (0.11x). The 344ms startup makes it appear slow, but the JIT-compiled algorithm is efficient.
- **ASM speedup (76x):** Precomputation + tight binary search gives ASM a massive advantage over C's naive per-number checking. Algorithm choice dominates.
- **ASM line count (537):** 6.5x more than C (83) - precomputation with sorting, binary search, and prefix sums requires extensive manual implementation. The shared utility functions help but still require setup code.
- **Haskell complexity (5):** Lowest despite 109 lines - Haskell's declarative style with list comprehensions and pure functions produces code with few branch points.
- **Lisp memory (59 MB):** SBCL's runtime and standard library are substantial. The image-based model loads more than strictly necessary for this problem.

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
- **Algorithm dominates:** 76x speedup from precomputation (ASM vs C's naive approach). The C reference deliberately uses per-number checking to demonstrate algorithm choice matters more than language.
- **All precomputation languages beat C internally:** Even Python (55.8ms) and Ruby (35.9ms) are faster than C (101.9ms) with the right algorithm.
- **TypeScript is actually fast:** 37.9ms internal time is competitive with Ruby/Haskell. The 506ms startup creates the illusion of slowness.
- **Julia shines with precomputation:** 11.4ms internal time makes it the 3rd fastest, demonstrating JIT's strength for repeated operations once warmed up.
- **Startup overhead comparison:** TypeScript (506ms) and Julia (344ms) have 150-300x more startup than compiled languages (1-3ms). On this 100ms+ problem, it's less impactful than Day 1.
