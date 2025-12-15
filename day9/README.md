# Day 9: Red Corner Rectangles

## Problem Summary
Find largest axis-aligned rectangle with red corners.
- **Part 1:** Maximum area rectangle with 4 red corner points
- **Part 2:** Rectangle must also be entirely inside a polygon (including on-edge)

## Algorithm

### Part 1: O(n^2) Point Pair Enumeration
- Enumerate all point pairs as potential rectangle corners
- Area formula: `(|x1 - x2| + 1) * (|y1 - y2| + 1)` (inclusive grid counting)
- Track maximum area found

### Part 2: Ray Casting Point-in-Polygon
1. Check all 4 corners are inside polygon (including on-edge)
2. Check no polygon edge crosses rectangle interior
3. **Ray casting:** Count intersections of horizontal ray from point with polygon edges

### Point-on-Edge Handling
Critical for correct results - points exactly on polygon edges must be included.

## Performance Results

| Language | Time (ms) | vs C |
|----------|-----------|------|
| **C** | 75.198 | 1.0x (baseline) |
| **ASM** | 110.609 | 1.5x |
| **Rust** | 154.582 | 2.1x |
| **Go** | 197.146 | 2.6x |
| **Julia** | 428.581 | 5.7x |
| **Haskell** | 725.463 | 9.7x |
| **TypeScript** | 751.864 | 10.0x |
| **Lisp** | 2780.020 | 37.0x |
| **Python** | 8674.707 | 115.4x |
| **Ruby** | 9871.194 | 131.3x |

## Resource Metrics

| Language | Memory (KiB) | Lines | Complexity |
|----------|-------------|-------|------------|
| **C** | 3,072 | 128 | 49 |
| **Python** | 11,328 | 102 | 41 |
| **Go** | 18,948 | 158 | 44 |
| **Rust** | 1,728 | 130 | 38 |
| **TypeScript** | 212,684 | 98 | 40 |
| **Ruby** | 12,864 | 92 | 28 |
| **ASM** | 1,344 | 661 | 40 |
| **Lisp** | 45,312 | 122 | 30 |
| **Julia** | 293,376 | 107 | 38 |
| **Haskell** | 8,064 | 110 | 31 |

### Anomalies & Analysis

- **C complexity (49):** Highest among high-level languages - the O(n^2) point enumeration with ray casting requires many boundary and intersection checks. Geometric algorithms have inherent branching.
- **ASM line count (661):** 5x more than C (128) - integer ray casting, point-on-edge handling, and coordinate sorting require explicit implementation of every geometric primitive.
- **Rust memory (1,728 KiB):** Lowest - no heap allocation needed for the fixed-size point arrays. Rust's zero-cost abstractions shine on geometric problems.
- **Ruby complexity (28):** Lowest high-level language - Ruby's implicit iterators and method chaining hide the branching. Yet 131x slower than C.
- **Python/Ruby timing (100x+):** The O(n^2) algorithm with interpreted loop overhead creates the worst performance gap in the suite. Each iteration is tiny but there are millions of them.

## Language Notes

| Language | Point Representation | Notes |
|----------|---------------------|-------|
| **Python** | Tuples | Integer arithmetic for exact comparisons |
| **Rust** | `(i64, i64)` tuples | Manual ray casting |
| **Lisp** | `complex` numbers | `realpart`/`imagpart` accessors |
| **Julia** | `Tuple{Int,Int}` | Integer division `div` |
| **Haskell** | Tuples | Bang patterns for strict evaluation |

### External Libraries Evaluated
- `rgeo`: Doesn't support Polygon-contains-Polygon
- `geo` crate (Rust): 100x slower than manual
- `PolygonOps`: Requires closed polygons

Manual ray casting provides consistent, fast implementations.

## Assembly Optimizations
- **Integer ray casting:** Signed division with `idiv`
- **Point-on-edge check:** Boundary handling
- **Early exit:** On first failing corner or edge crossing
- **SIMD potential:** `vfmadd` for cross products
- **Sort points by x-coordinate:** Cache-friendly access
- **`pdep`/`pext` (BMI2):** Coordinate packing

## Interesting Points
- C is fastest here - the straightforward O(n^2) algorithm benefits from simple compiled code
- ASM is 50% slower than C - hand optimization doesn't help when the algorithm is memory-bound
- Python and Ruby are 100x+ slower - interpreted loop overhead on O(n^2)
- The Part 2 polygon constraint adds significant complexity
- Integer arithmetic is essential for exact geometric comparisons (no floating-point errors)
- This is the most computation-heavy problem in the suite for scripting languages
