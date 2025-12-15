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

## Performance Results (Internal Timing)

| Language | Time (ms) | vs C | Startup (ms) |
|----------|-----------|------|--------------|
| **C** | 70.736 | 1.0x (baseline) | 2.1 |
| **ASM** | 103.609 | 1.46x | 1.9 |
| **Go** | 144.570 | 2.0x | 2.3 |
| **Rust** | 147.416 | 2.1x | 2.1 |
| **Julia** | 156.678 | 2.2x | 278.1 |
| **TypeScript** | 237.308 | 3.4x | 493.4 |
| **Haskell** | 817.828 | 11.6x | -55.9* |
| **Lisp** | 2676.302 | 37.8x | 47.1 |
| **Python** | 8160.864 | 115.4x | 17.3 |
| **Ruby** | 9570.992 | 135.3x | 35.2 |

*Internal timing measures algorithm execution only; Startup measures process/runtime initialization.*
*\*Haskell shows negative startup due to lazy evaluation deferring work past timer end.*

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

- **C is fastest (baseline):** The straightforward O(n^2) algorithm benefits from simple compiled code. ASM is 46% slower due to optimization complexity.
- **Julia internal (2.2x):** Only 156.7ms internally, not 428ms externally. The 278ms startup was hiding competitive performance.
- **Haskell lazy evaluation anomaly:** -56ms "startup" means external < internal. Deferred work past timer end distorts measurement.
- **TypeScript internal (3.4x):** 237ms internally vs 752ms externally. V8 handles geometric computations reasonably.
- **Python/Ruby 100x+ slower internally:** The O(n^2) algorithm with interpreted loop overhead is genuinely slow, not startup-related.
- **Rust memory (1,728 KiB):** Lowest - no heap allocation needed for fixed-size point arrays.

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
- **C is fastest:** Straightforward O(n^2) algorithm compiles efficiently. ASM is 46% slower due to complexity.
- **Julia internal (2.2x) is competitive:** The 278ms startup made it appear 5.7x slower externally.
- **Haskell lazy evaluation backfires again:** External time < internal due to deferred computation.
- **Python/Ruby are genuinely slow (115x-135x):** Interpreted loop overhead on O(n^2) is unavoidable.
- **TypeScript internal (3.4x):** V8 handles geometric computations reasonably well.
- Integer arithmetic is essential for exact geometric comparisons (no floating-point errors).
- This is the most computation-heavy problem in the suite for scripting languages.
