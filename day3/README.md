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

## Performance Results (Internal Timing)

| Language | Time (ms) | vs C | Startup (ms) |
|----------|-----------|------|--------------|
| **ASM** | 0.186 | 0.04x (fastest) | 1.4 |
| **Rust** | 0.193 | 0.04x | 1.5 |
| **Go** | 0.217 | 0.05x | 2.2 |
| **Julia** | 0.618 | 0.13x | 304.6 |
| **Haskell** | 2.513 | 0.56x | 1.9 |
| **TypeScript** | 3.756 | 0.79x | 491.2 |
| **C** | 4.783 | 1.0x (baseline) | 1.7 |
| **Python** | 7.287 | 1.52x | 17.3 |
| **Lisp** | 7.458 | 1.56x | 39.2 |
| **Ruby** | 8.483 | 1.77x | 32.7 |

*Internal timing measures algorithm execution only; Startup measures process/runtime initialization.*

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

- **ASM/Rust/Go are 25x faster than C:** Internal timing reveals C's implementation is suboptimal for this stack algorithm. ASM (0.186ms), Rust (0.193ms), Go (0.217ms) all outperform C (4.783ms) dramatically.
- **Julia is 8x faster than C internally:** At 0.618ms, Julia's JIT-compiled code is highly efficient. The 305ms startup makes it appear 45x slower in total time.
- **TypeScript beats C internally:** 3.756ms vs 4.783ms - V8's JIT produces better code than C here, but 491ms startup hides this.
- **Haskell complexity (3):** Lowest in the entire suite - the fold-based stack implementation with pattern matching produces almost no explicit branches.
- **Rust memory (1,920 KiB):** Nearly matches C (1,536 KiB) - Rust's zero-cost abstractions and lack of runtime overhead shows here.
- **Ruby line count (38):** Most concise implementation - Ruby's iterator methods eliminate boilerplate. Internal timing (8.5ms) is only 1.77x slower than C.

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
- **ASM and Rust are 25x faster than C:** This stack-based algorithm reveals C's implementation is suboptimal. The tight pop/push loops benefit from modern compiler/hand optimization.
- **Go outperforms C by 22x:** Go's implementation is surprisingly efficient at 0.217ms, rivaling ASM and Rust.
- **Julia and TypeScript beat C internally:** Once JIT-compiled, both produce faster code (0.618ms and 3.756ms) than C (4.783ms). Startup overhead (305ms and 491ms) hides this.
- **Haskell is competitive:** 2.5ms (0.56x C) shows functional languages handle stack operations efficiently with proper strictness.
- **Scripting languages are reasonable:** Python (1.52x), Lisp (1.56x), Ruby (1.77x) are all within 2x of C. The algorithm is simple enough that interpreter overhead is manageable.
- **The suffix max optimization** for Part 1 (k=2) allows O(n) solution, matching the stack's O(n) complexity.
