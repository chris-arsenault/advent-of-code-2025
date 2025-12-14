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
| **C** | 76.965 | 1.0x (baseline) |
| **ASM** | 111.325 | 1.45x |
| **Go** | 146.096 | 1.90x |
| **Rust** | 146.652 | 1.90x |
| **Julia** | 167.899 | 2.18x |
| **TypeScript** | 233.529 | 3.03x |
| **Haskell** | 815.172 | 10.6x |
| **Lisp** | 2865.214 | 37.2x |
| **Python** | 8697.440 | 113x |
| **Ruby** | 9676.941 | 126x |

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
- ASM is actually 45% slower than C here - the O(n^2) algorithm dominates
- Python and Ruby are 100x+ slower - interpreted loop overhead on O(n^2)
- Go and Rust nearly identical at 1.9x - both have efficient point handling
- The Part 2 polygon constraint adds significant complexity
- Integer arithmetic is essential for exact geometric comparisons (no floating-point errors)
- This is the most computation-heavy problem in the suite for scripting languages
