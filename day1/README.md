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

## Performance Results (Internal Timing)

| Language | Time (ms) | vs C | Startup (ms) |
|----------|-----------|------|--------------|
| **ASM** | 0.064 | 0.48x (fastest) | 1.6 |
| **C** | 0.132 | 1.0x (baseline) | 1.5 |
| **Rust** | 0.237 | 1.8x | 1.8 |
| **Julia** | 0.647 | 4.9x | 212.5 |
| **TypeScript** | 1.036 | 7.8x | 484.0 |
| **Go** | 1.105 | 8.4x | 2.9 |
| **Python** | 1.169 | 8.9x | 16.9 |
| **Ruby** | 2.685 | 20.3x | 32.6 |
| **Haskell** | 3.88 | 34.9x | 1.9 |
| **Lisp** | <0.001 | ~0x | 44.4 |

*Internal timing measures algorithm execution only; Startup measures process/runtime initialization.*

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

- **TypeScript startup (484ms):** Node.js/V8 engine initialization dominates. The algorithm itself runs in 1.0ms (7.8x C), but total process time is 485ms (3,670x C). This is the clearest example of JIT startup cost in the suite.
- **Julia startup (213ms):** JIT compilation overhead. Internal timing of 0.647ms (4.9x C) is competitive, but 213ms startup makes it appear 1,620x slower in total process time.
- **Lisp sub-millisecond:** The 0.000ms reading indicates timing precision limits. The algorithm runs faster than the timer resolution, but 44ms SBCL startup dominates.
- **TypeScript memory (207 MB):** Node.js/V8 engine overhead - the runtime itself dwarfs the actual solution memory. This is ~154x more than C for a trivial problem.
- **Julia memory (278 MB):** JIT compiler remains resident in memory. Julia's "time-to-first-plot" problem manifests as both memory and startup overhead.
- **ASM line count (164):** 8x more lines than Ruby (21) for the same algorithm. Assembly requires explicit register management, stack handling, and syscalls that high-level languages abstract away.
- **Go internal timing (8.4x):** Unlike the old external timing that showed 27x, internal timing reveals Go is reasonably competitive once its 2.9ms runtime initializes.

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
- **ASM is 2x faster than C internally** (0.064ms vs 0.132ms) - branchless techniques provide real speedup on this tight loop
- **Rust is competitive** at 1.8x C, showing zero-cost abstractions work well for simple algorithms
- **Haskell is surprisingly slow** (35x C) despite being compiled - lazy evaluation overhead on strict accumulating loops
- **Startup dominates for JIT languages:** TypeScript (484ms) and Julia (213ms) startup makes them appear 1000x+ slower, but internally they're only 5-8x slower than C
- **The internal/external timing split** reveals that TypeScript's V8 and Julia's JIT are actually efficient once running - their reputation for slowness on short problems is purely startup cost
- **Compiled languages cluster:** C, Rust, ASM, Go, Haskell all have <3ms startup; interpreted/JIT languages have 17-484ms startup
