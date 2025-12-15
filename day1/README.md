# Day 1: Secret Entrance (Dial Rotation)

## Problem Summary
A dial points at numbers 0-99 in a circle. Follow rotation instructions (L/R with distance) starting from 50.
- **Part 1:** Count how many times the dial lands on 0 after each rotation
- **Part 2:** Count zero crossings during rotations (including landings)

## Algorithm
- Parse each instruction to extract direction and distance
- Track position with modular arithmetic: `pos = (pos + delta) % 100`
- Part 2 crossing calculation requires tracking position BEFORE each move plus direction/magnitude

### Key Insight
The sequential dependency (each position depends on the previous) makes pure functional patterns (`accumulate`, `scan`, `reduce`) awkward. All implementations use explicit loops, which is appropriate.

## Performance Results

| Language | Time (ms) | vs C |
|----------|-----------|------|
| **ASM** | 1.481 | 0.87x (fastest) |
| **C** | 1.699 | 1.0x (baseline) |
| **Rust** | 1.772 | 1.04x |
| **Haskell** | 5.665 | 3.3x |
| **Lisp** | 10.341 | 6.1x |
| **Python** | 17.565 | 10.3x |
| **Ruby** | 35.999 | 21.2x |
| **Go** | 46.173 | 27.2x |
| **Julia** | 196.864 | 115.9x |
| **TypeScript** | 478.824 | 281.8x |

## Resource Metrics

| Language | Memory (KiB) | Lines | Complexity |
|----------|-------------|-------|------------|
| **C** | 1,344 | 42 | 8 |
| **Python** | 11,328 | 32 | 9 |
| **Go** | 19,168 | 57 | 11 |
| **Rust** | 2,304 | 35 | 8 |
| **TypeScript** | 207,032 | 25 | 6 |
| **Ruby** | 12,864 | 21 | 6 |
| **ASM** | 1,344 | 164 | 17 |
| **Lisp** | 28,032 | 28 | 9 |
| **Julia** | 278,208 | 26 | 6 |
| **Haskell** | 7,872 | 27 | 7 |

### Anomalies & Analysis

- **TypeScript memory (207 MB):** Node.js/V8 engine overhead - the runtime itself dwarfs the actual solution memory. This is ~154x more than C for a trivial problem.
- **Julia memory (278 MB):** JIT compiler remains resident in memory. Julia's "time-to-first-plot" problem manifests as both memory and startup overhead.
- **ASM line count (164):** 8x more lines than Ruby (21) for the same algorithm. Assembly requires explicit register management, stack handling, and syscalls that high-level languages abstract away.
- **ASM complexity (17):** Highest complexity despite simplest algorithm - the metric counts conditional jumps (`cmov`, `jmp`), which branchless techniques ironically increase.
- **Go timing anomaly (27x slower):** Go's runtime initialization and garbage collector setup dominate on this sub-2ms problem. On longer-running problems, Go performs much better relative to C.

## Language Notes

| Language | Modulo Handling |
|----------|-----------------|
| **Rust** | `rem_euclid()` for correct negative modulo |
| **TypeScript** | Double-modulo trick `((x % 100) + 100) % 100` |
| **Go** | Manual negative modulo fix required |
| **Ruby, Lisp, Julia** | Native `%`/`mod` handles negatives correctly |
| **Haskell** | Worker pattern with explicit tail recursion; bang patterns for strictness |

## Assembly Optimizations

SIMD parallelism is limited because each position depends on the previous. The implementation uses **branchless scalar techniques**:

- **Branchless sign extraction:** `cmovne` to select 1 or -1 without branching
- **Branchless first calculation:** `cmove` for conditional `100 - pos` vs `pos`
- **Branchless zero-at-boundary:** `cmovz` to replace 0 with 100
- **Branchless negative modulo:** `cmovns` to conditionally add 100
- **Branchless zero counting:** `setz` + `add` instead of conditional increment
- Division for crossing count kept as branch (division is expensive; skip when possible)

## Interesting Points
- ASM, C, and Rust are nearly identical in performance (within 20%) - the algorithm is simple enough that compiler optimization matches hand-tuned assembly
- The branchless ASM techniques reduce branch misprediction but can't overcome the sequential dependency
- Haskell's lazy evaluation requires careful strictness annotations to avoid space leaks in accumulating loops
- TypeScript and Julia show significant startup overhead on this short-running problem
