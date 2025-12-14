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
| **C** | 0.057 | 1.0x (baseline) |
| **ASM** | 0.062 | 1.1x |
| **Rust** | 0.114 | 2.0x |
| **Julia** | 0.470 | 8.2x |
| **Lisp** | 0.679 | 11.9x |
| **Go** | 1.138 | 20.0x |
| **TypeScript** | 2.201 | 38.6x |
| **Python** | 2.236 | 39.2x |
| **Haskell** | 5.595 | 98.2x |
| **Ruby** | 7.765 | 136.2x |

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
- C and ASM are nearly identical (0.057 vs 0.062ms) - parsing overhead dominates
- Rust is only 2x slower, showing excellent compiler optimization
- Languages with native big integers (Python, Ruby, Lisp) don't pay a performance penalty for arithmetic
- Haskell is unusually slow here (98x) - string parsing is not its strength
- The Part 2 column parsing (right-to-left, top-to-bottom) is the tricky part of this problem
