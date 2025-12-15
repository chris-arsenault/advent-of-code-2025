# Day 3: Lobby (Battery Joltage)

## Problem Summary
Given a sequence of digits, select exactly k digits (in order) to form the largest possible number.
- **Part 1:** k=2
- **Part 2:** k=12

## Algorithm
**Greedy monotonic stack:**
1. Maintain a stack of selected digits
2. For each digit: pop from stack while stack not empty, top < current, and enough digits remain
3. Push current digit if stack size < k
4. This ensures lexicographically largest result

### Correctness
The greedy approach works because:
- We want the largest digits as early as possible
- Popping smaller digits is safe only if enough remain to fill k positions
- The constraint "enough digits remain" prevents removing too many

## Performance Results

| Language | Time (ms) | vs C |
|----------|-----------|------|
| **ASM** | 1.624 | 0.26x (fastest) |
| **Rust** | 1.825 | 0.29x |
| **Haskell** | 4.882 | 0.77x |
| **C** | 6.336 | 1.0x (baseline) |
| **Lisp** | 19.135 | 3.0x |
| **Python** | 24.386 | 3.9x |
| **Go** | 41.758 | 6.6x |
| **Ruby** | 43.230 | 6.8x |
| **Julia** | 285.762 | 45.1x |
| **TypeScript** | 506.226 | 79.9x |

## Resource Metrics

| Language | Memory (KiB) | Lines | Complexity |
|----------|-------------|-------|------------|
| **C** | 1,536 | 71 | 11 |
| **Python** | 11,136 | 48 | 17 |
| **Go** | 18,804 | 69 | 14 |
| **Rust** | 1,920 | 53 | 11 |
| **TypeScript** | 205,700 | 42 | 10 |
| **Ruby** | 12,480 | 38 | 6 |
| **ASM** | 1,344 | 284 | 16 |
| **Lisp** | 33,216 | 51 | 13 |
| **Julia** | 288,192 | 48 | 14 |
| **Haskell** | 7,872 | 50 | 3 |

### Anomalies & Analysis

- **Haskell complexity (3):** Lowest in the entire suite - the fold-based stack implementation with pattern matching produces almost no explicit branches. Haskell's expressiveness shines for this kind of recursive data structure manipulation.
- **Python complexity (17):** Highest among high-level languages - likely due to explicit index management and multiple conditional checks in the greedy algorithm.
- **Rust memory (1,920 KiB):** Nearly matches C (1,536 KiB) - Rust's zero-cost abstractions and lack of runtime overhead shows here. The `Vec` operations compile to essentially the same code as C arrays.
- **Ruby line count (38):** Most concise implementation - Ruby's iterator methods and implicit returns eliminate boilerplate. Yet runtime is 6.8x slower than C, showing the cost of that abstraction.
- **ASM vs C memory:** Identical (1,344 vs 1,536 KiB) - both use minimal stack-based allocation with no heap. The tiny difference is likely padding/alignment.

## Language Notes

| Language | Stack Implementation |
|----------|---------------------|
| **Python** | List with `pop()` |
| **Go** | Slice with `append` and reslice |
| **Rust** | `Vec` with `push`/`pop` |
| **Ruby** | Array with `each_char.with_index` |
| **Lisp** | Recursive with accumulator or `loop` |
| **Julia** | Vector with `push!`/`pop!` |
| **Haskell** | Fold with explicit stack state; reversed list (head=top) for O(1) ops |

## Assembly Optimizations

- **Suffix max array:** Build suffix maximum for O(1) best-pair lookups in Part 1
- **`cmov` instructions:** Branchless max comparisons (suffix max, find best candidate)
- **Greedy monotonic stack:** Standard loop with conditional pop

## Interesting Points
- ASM and Rust are significantly faster than C (3-4x), showing compiler-generated code isn't optimal for this stack-based algorithm
- Haskell performs well here (0.77x C) - functional languages handle stack operations efficiently
- Go and Ruby are surprisingly slow compared to Python on this problem
- The suffix max optimization for Part 1 (k=2) allows O(n) solution vs O(n) stack anyway
- TypeScript and Julia suffer from startup overhead on this fast-running problem
