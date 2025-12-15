# Day 6: Vertical Math

## Problem Summary
Parse vertical math problems from ASCII art.
- **Part 1:** Numbers in rows (read horizontally)
- **Part 2:** Numbers in columns (read vertically, right-to-left, most significant digit at top)

Apply operator (+/*) and sum all problem results.

## Algorithm
1. Read input as 2D character grid, preserving column positions
2. Identify problem boundaries (columns that are all spaces)
3. **Part 1:** Parse numbers horizontally from each row within a problem
4. **Part 2:** Parse numbers vertically from each column (right to left)
5. Apply operator and sum results

### Part 2 Column Parsing
Each column's digits (top-to-bottom, most significant at top) form one number. Process columns right-to-left within each problem.

## Performance Results (Internal Timing)

| Language | Time (ms) | vs C | Startup (ms) |
|----------|-----------|------|--------------|
| **C** | 0.121 | 1.0x (baseline) | 4.0 |
| **ASM** | 0.125 | 1.03x | 1.5 |
| **Rust** | 0.164 | 1.36x | 1.4 |
| **Julia** | 0.614 | 5.07x | 600.6 |
| **Go** | 1.376 | 11.4x | 2.2 |
| **TypeScript** | 2.605 | 21.5x | 492.9 |
| **Python** | 4.155 | 34.3x | 19.2 |
| **Haskell** | 5.849 | 48.3x | 1.9 |
| **Lisp** | 7.436 | 61.5x | 37.7 |
| **Ruby** | 11.163 | 92.3x | 32.4 |

*Internal timing measures algorithm execution only; Startup measures process/runtime initialization.*

## Resource Metrics

| Language | Memory (KiB) | Lines | Complexity |
|----------|-------------|-------|------------|
| **C** | 7,680 | 129 | 35 |
| **Python** | 11,904 | 76 | 28 |
| **Go** | 18,792 | 139 | 33 |
| **Rust** | 1,920 | 118 | 30 |
| **TypeScript** | 213,556 | 79 | 22 |
| **Ruby** | 13,056 | 76 | 12 |
| **ASM** | 1,344 | 379 | 12 |
| **Lisp** | 35,136 | 76 | 24 |
| **Julia** | 325,632 | 83 | 25 |
| **Haskell** | 9,024 | 72 | 4 |

### Anomalies & Analysis

- **C, ASM, Rust are nearly identical:** 0.121ms, 0.125ms, 0.164ms - all within 36% of each other. The big integer parsing is well-optimized across compiled languages.
- **Julia internal (0.614ms):** Only 5x C internally, not 145x. The 601ms startup (largest in suite for Day 6) dominated external timing.
- **Haskell is surprisingly slow internally:** 5.849ms (48x C) despite native `Integer` type. String parsing overhead hurts here.
- **C memory (7,680 KiB):** Higher than usual for C - big integer arithmetic requires additional buffer space for multi-precision operations.
- **Rust memory (1,920 KiB):** Lowest compiled language - `num-bigint` crate is efficient, and Rust's ownership model minimizes allocations.
- **Go lines (139):** Most verbose - `math/big` API is explicit. Internal timing of 1.4ms (11x C) is reasonable.

## Language Notes

| Language | Big Integer Support | Parsing |
|----------|---------------------|---------|
| **Python** | Native arbitrary precision | `int(''.join(digits))` |
| **Go** | `math/big` package | Manual parsing |
| **Rust** | `num-bigint` crate | Pattern matching for operators |
| **TypeScript** | Native `BigInt` | String operations |
| **Ruby** | Native arbitrary precision | `reduce(:+)` for summing |
| **Lisp** | Native bignums | `parse-integer` |
| **Julia** | Native `BigInt` | `eachcol` for column parsing |
| **Haskell** | Native `Integer` type | `read` for parsing |

## Assembly Optimizations
- **`mulx` (BMI2):** Widening multiply without flags clobbering
- **`adc` chain:** Multi-precision addition for large numbers
- **Unrolled multiplication:** Inner loop unrolling for better pipelining

## Interesting Points
- **C, ASM, Rust are within 36%:** 0.121ms, 0.125ms, 0.164ms - big integer parsing is uniformly fast across compiled languages.
- **Julia internal (0.614ms):** Only 5x C, not 145x. The 601ms startup dominated external measurements.
- **Haskell internal (5.8ms):** Surprisingly slow (48x C) despite native `Integer`. String parsing overhead is significant.
- **TypeScript internal (2.6ms):** 21x C is moderate. The 493ms startup inflated external timing to 127x.
- Languages with native big integers (Python, Ruby, Lisp) still pay performance penalties for parsing, not arithmetic.
- The Part 2 column parsing (right-to-left, top-to-bottom) is the tricky part of this problem.
