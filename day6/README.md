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

## Performance Results

| Language | Time (ms) | vs C |
|----------|-----------|------|
| **Rust** | 1.633 | 0.40x (fastest) |
| **ASM** | 1.803 | 0.45x |
| **C** | 4.052 | 1.0x (baseline) |
| **Haskell** | 7.984 | 2.0x |
| **Lisp** | 20.311 | 5.0x |
| **Python** | 23.769 | 5.9x |
| **Ruby** | 43.336 | 10.7x |
| **Go** | 47.200 | 11.6x |
| **TypeScript** | 516.457 | 127.5x |
| **Julia** | 586.091 | 144.6x |

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

- **C memory (7,680 KiB):** Higher than usual for C - big integer arithmetic requires additional buffer space for multi-precision operations.
- **Rust memory (1,920 KiB):** Lowest compiled language - `num-bigint` crate is efficient, and Rust's ownership model minimizes allocations.
- **Haskell complexity (4):** Second lowest overall - native `Integer` type and pattern matching on operator characters produce almost branchless code. Haskell excels at parsing problems.
- **ASM complexity (12):** Surprisingly low - the BMI2 `mulx` and `adc` chain for big integers require few conditional branches. Most work is sequential arithmetic.
- **C complexity (35):** Highest - manual big integer implementation requires many carry checks and boundary conditions.
- **Go lines (139):** Most verbose - `math/big` API is explicit, and Go's error handling adds lines. Yet performs well (11.6x C).

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
- Rust and ASM outperform C by 2.2-2.5x on this parsing-heavy problem - LLVM optimizations excel at string processing
- Haskell performs reasonably (2x C) despite string parsing not being its strength
- Languages with native big integers (Python, Ruby, Lisp) don't pay a performance penalty for arithmetic
- The Part 2 column parsing (right-to-left, top-to-bottom) is the tricky part of this problem
- Julia and TypeScript show extreme startup overhead (127x-145x) on this problem
