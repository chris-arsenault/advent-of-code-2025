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
| **Rust** | 0.034 | 0.4x (fastest) |
| **ASM** | 0.042 | 0.5x |
| **C** | 0.086 | 1.0x (baseline) |
| **Lisp** | 0.217 | 2.5x |
| **Julia** | 0.315 | 3.7x |
| **Go** | 0.922 | 10.7x |
| **Python** | 1.040 | 12.1x |
| **Ruby** | 2.457 | 28.6x |
| **Haskell** | 3.231 | 37.6x |

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
- Rust outperforms hand-written ASM here due to LLVM's excellent optimization of simple arithmetic loops
- The branchless ASM techniques reduce branch misprediction but can't overcome the sequential dependency
- Haskell's lazy evaluation requires careful strictness annotations to avoid space leaks in accumulating loops
