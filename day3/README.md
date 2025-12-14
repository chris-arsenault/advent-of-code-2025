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
| **ASM** | 0.151 | 0.03x (fastest) |
| **Rust** | 0.167 | 0.03x |
| **Go** | 0.211 | 0.04x |
| **Julia** | 0.542 | 0.11x |
| **Lisp** | 2.153 | 0.44x |
| **Haskell** | 3.192 | 0.65x |
| **TypeScript** | 3.548 | 0.72x |
| **C** | 4.945 | 1.0x (baseline) |
| **Python** | 7.194 | 1.45x |
| **Ruby** | 8.780 | 1.78x |

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
- C is unusually slow here (4.9ms) compared to other compiled languages
- ASM, Rust, and Go all achieve sub-0.25ms, suggesting the algorithm is memory-bound
- The suffix max optimization for Part 1 (k=2) allows O(n) solution vs O(n) stack anyway
- Further optimization has diminishing returns at 0.15ms
