# Advent of Code 2025 - Algorithm Reference

This document describes the algorithms used in the C implementations and suggests idiomatic approaches for other languages.

---

## Day 1: Secret Entrance (Dial Rotation)

### Problem Summary
A dial points at numbers 0-99 in a circle. Follow rotation instructions (L/R with distance) starting from 50. Count how many times the dial lands on 0 (Part 1: after rotations, Part 2: during rotations too).

### C Algorithm
- Parse each instruction to extract direction and distance
- Track current position with modular arithmetic: `pos = (pos + delta) % 100`
- Part 1: Check if `pos == 0` after each rotation
- Part 2: Count how many times we cross or land on 0 during rotation:
  - For right rotation from `a` to `b`: count zeros in range `(a, b]` wrapping
  - For left rotation from `a` to `b`: count zeros in range `[b, a)` wrapping

### Idiomatic Suggestions

| Language | Approach |
|----------|----------|
| **Python** | Use `itertools.accumulate` with modular reduction; `sum(1 for p in positions if p == 0)` |
| **Go** | Straightforward loop; no special idioms needed |
| **Rust** | Use `Iterator::scan` to track state; `filter(|&p| p == 0).count()` |
| **TypeScript** | `reduce` with accumulator tracking position and count |
| **Ruby** | `inject` with running position; `count { |p| p.zero? }` |
| **Lisp** | `loop` with `count` clause; `mod` for wrapping |
| **Julia** | Broadcasting with `mod.(cumsum(deltas), 100)`; `count(==(0), positions)` |
| **Haskell** | `scanl` for positions; `length . filter (== 0)` |

### Assembly Optimization
- Use SIMD to process multiple rotations in parallel (AVX2/AVX-512)
- Branchless zero-counting with `vpcmpeqd` + `vpmovmskb` + `popcnt`
- Unroll the main loop 4-8x for better ILP

---

## Day 2: Gift Shop (Invalid Product IDs)

### Problem Summary
Find IDs within given ranges that consist of a digit sequence repeated k times (Part 1: k=2, Part 2: k≥2). Sum all invalid IDs.

### C Algorithm
- Parse ranges from comma-separated input
- For each range, iterate through all numbers
- Check if number is "repeated pattern":
  - Convert to string, try each divisor of length
  - Check if first `len/k` digits repeat exactly k times
- Part 2: Check for any k ≥ 2 that divides the digit count

### Algorithm Notes

**Performance tip:** Avoid `log10`/`pow` for digit counting. Use string length instead:
```c
char buf[20]; sprintf(buf, "%llu", n);
int len = strlen(buf);
// Compare halves with strncmp instead of math
```

**Faster alternative (precomputation):** For large ranges, precompute all valid repeated-pattern numbers:
1. Generate all k-repeated patterns (k=2,3,...) up to max range value
2. Store in sorted array (~100k-1M numbers depending on max)
3. For each range query, binary search to count matches
4. Converts O(range_size × digit_checks) → O(log(valid_patterns))

This is especially effective when ranges are large or queries are repeated.

### Idiomatic Suggestions

| Language | Approach |
|----------|----------|
| **Python** | Use regex: `re.fullmatch(r'(.+)\1+', str(n))` for Part 2 |
| **Go** | String slicing with `strings.Repeat` comparison |
| **Rust** | `str.repeat(k) == original` pattern matching |
| **TypeScript** | Regex `/^(.+)\1+$/` matches repeated patterns |
| **Ruby** | `n.to_s.match?(/^(.+)\1+$/)` - Ruby's regex is elegant here |
| **Lisp** | `(string= s (concatenate 'string prefix prefix))` |
| **Julia** | `repeat(s[1:k], length(s)÷k) == s` |
| **Haskell** | `concat (replicate k prefix) == s` with list comprehension for divisors |

### Assembly Optimization
- Use `rep cmpsb` for fast string comparison
- Precompute all repeated-pattern numbers up to max range value
- Use SIMD for parallel range checking with `vpcmpgtd`

---

## Day 3: Lobby (Battery Joltage)

### Problem Summary
Given a sequence of digits, select exactly k digits (in order) to form the largest possible number. Part 1: k=2, Part 2: k=12.

### C Algorithm
- Greedy stack-based approach:
  - Maintain a stack of selected digits
  - For each digit, pop from stack while: stack not empty, top < current, and enough digits remain
  - Push current digit if stack size < k
- This ensures lexicographically largest result

### Idiomatic Suggestions

| Language | Approach |
|----------|----------|
| **Python** | Use `heapq` or implement monotonic stack; list as stack with `pop()` |
| **Go** | Slice-based stack; `append` and reslice for pop |
| **Rust** | `Vec` as stack with `push`/`pop`; iterator chaining |
| **TypeScript** | Array with `push`/`pop`; `reduce` for final number |
| **Ruby** | Array stack; `each_char.with_index` for iteration |
| **Lisp** | Recursive solution with accumulator; or `loop` with stack list |
| **Julia** | Vector stack; `push!`/`pop!` for mutation |
| **Haskell** | Fold with explicit stack state; or use `Seq` for O(1) both ends |

### Assembly Optimization
- Unroll digit comparisons
- Use `cmov` for branchless stack operations
- Process multiple banks in parallel with SIMD gather/scatter

---

## Day 4: Paper Rolls Grid

### Problem Summary
Grid of paper rolls (`@`). Part 1: Count rolls accessible by forklift (fewer than 4 of 8 adjacent neighbors). Part 2: Iteratively remove accessible rolls until none remain; count total removed.

### C Algorithm
- Parse grid into 2D array
- Precompute neighbor count for each roll (8-directional)
- **Part 1:** Count cells with neighbor count < 4
- **Part 2:** BFS wavefront removal:
  1. Enqueue all initially accessible rolls
  2. Dequeue a roll, remove it, increment count
  3. Decrement neighbor counts of adjacent rolls
  4. If any neighbor's count drops below 4, enqueue it
  5. Repeat until queue empty
- Complexity: O(rows × cols) - each cell processed at most once

**Key insight:** The naive approach (rescan entire grid each iteration) is O(iterations × n). BFS ensures O(n) by only processing cells when they become newly accessible.

### Idiomatic Suggestions

| Language | Approach |
|----------|----------|
| **Python** | `collections.deque` for BFS; NumPy for neighbor counting with `scipy.ndimage.convolve` |
| **Go** | Slice-based queue; 2D slice for grid and neighbor counts |
| **Rust** | `VecDeque` for BFS; `ndarray` or flat `Vec` with index math |
| **TypeScript** | Array as queue (shift/push); `Map` for neighbor counts |
| **Ruby** | Array queue; `each` with 8-direction offsets |
| **Lisp** | List-based queue or `cl-containers`; 2D array with `aref` |
| **Julia** | `DataStructures.Queue`; native 2D array with CartesianIndex |
| **Haskell** | `Data.Sequence` for O(1) queue; `Data.Array` for grid |

### Assembly Optimization
- Pack grid into bitfield for cache efficiency
- Use `popcnt` for neighbor counting with bitmasks
- SIMD parallel processing of grid rows
- Consider spatial locality: process in cache-line-sized chunks

**Bitfield packing optimization:** Instead of `int neighbors[MAX][MAX]` (16MB for 4K×4K):
- Pack neighbor count as 3-bit field (values 0-8) → 1 byte per cell
- Or use separate "accessible" bitfield for O(1) threshold check
- Reduces memory footprint 4-16x, improves cache utilization

---

## Day 5: Fresh Paint (Interval Merging)

### Problem Summary
Given painted intervals with IDs, find "fresh" IDs (those whose paint is still visible). Part 1: Return smallest fresh ID. Part 2: Count fresh IDs.

### C Algorithm
- Parse all intervals as (start, end, id) tuples
- Sort by start position
- Merge overlapping intervals, tracking which IDs get covered
- Part 1: Binary search to find smallest uncovered ID
- Part 2: Count all IDs that remain visible after merging

### Algorithm Notes

**Faster Part 1 alternative (two-pointer merge):** Instead of binary search per ID:
1. Sort ID list once: O(ids × log ids)
2. Merge-walk sorted IDs with sorted intervals using two pointers
3. For each ID, advance interval pointer until ID is covered or past current interval
4. Complexity: O(ids × log ids + ids + intervals) vs O(ids × log intervals)

For the typical case (698 IDs, ~100 merged intervals):
- Current: 698 × log(100) ≈ 4600 comparisons
- Two-pointer: 698 + 100 ≈ 800 comparisons (after initial sort)

### Idiomatic Suggestions

| Language | Approach |
|----------|----------|
| **Python** | `sortedcontainers.SortedList` for interval tree; or `intervaltree` library |
| **Go** | Sort slice; merge with two-pointer technique |
| **Rust** | `BTreeMap` for ordered intervals; range queries |
| **TypeScript** | Sort + reduce for merging; `Set` for tracking IDs |
| **Ruby** | `sort_by` + `each_cons(2)` for pairwise comparison |
| **Lisp** | `sort` with custom comparator; `reduce` for merging |
| **Julia** | `IntervalSets.jl` or `DataStructures.jl` for sorted containers |
| **Haskell** | `Data.IntervalMap` from `IntervalMap` package; or `Data.Set` operations |

### Assembly Optimization
- Use `cmov` for branchless min/max in merge
- Vectorize interval comparisons with AVX-512 masked operations
- Cache-aligned interval storage

---

## Day 6: Vertical Math

### Problem Summary
Parse vertical math problems from ASCII art. Part 1: Numbers are in rows (read horizontally). Part 2: Numbers are in columns (read vertically, right-to-left, most significant digit at top).

### C Algorithm
- Read input as 2D character grid, preserving column positions
- Identify problem boundaries (columns that are all spaces)
- **Part 1:** Parse numbers horizontally from each row within a problem
- **Part 2:** Parse numbers vertically from each column (right to left), where each column's digits (top-to-bottom, most significant at top) form one number
- Apply operator (+/*) and sum all problem results

### Idiomatic Suggestions

| Language | Approach |
|----------|----------|
| **Python** | `int(''.join(digits))` for parsing; native big integers |
| **Go** | `math/big` for arbitrary precision |
| **Rust** | `num-bigint` crate; pattern matching for operators |
| **TypeScript** | `BigInt` native type for large numbers |
| **Ruby** | Native arbitrary precision integers; `reduce(:+)` for summing |
| **Lisp** | Native bignums; `parse-integer` for string conversion |
| **Julia** | Native `BigInt`; column parsing with `eachcol` |
| **Haskell** | Native `Integer` type; `read` for parsing |

### Assembly Optimization
- Use `mulx` (BMI2) for widening multiply without flags
- `adc` chain for multi-precision addition
- Unroll multiplication inner loop

---

## Day 7: Tachyon Beam Splitter

### Problem Summary
Simulate beams of light through a grid of mirrors and splitters. Count how many cells each configuration illuminates.

### C Algorithm
The C implementation uses **row-by-row simulation** (not generic BFS):
- Process grid row by row, tracking active beam columns per row
- Beams always move downward; splitters spawn left/right within same row
- Part 1: Track unique illuminated cells with deduplication
- Part 2: Track count of "ways to reach each column" (quantum timelines)
  - When hitting splitter, count doubles (branches into two timelines)
  - Sum counts at final row for total paths

This row-by-row approach is more cache-friendly than generic BFS.

### Idiomatic Suggestions

| Language | Approach |
|----------|----------|
| **Python** | `collections.deque` for BFS; `set` of `(x, y, dir)` tuples |
| **Go** | Channel-based parallel simulation; `map` for visited |
| **Rust** | `VecDeque` for BFS; `HashSet<(i32, i32, Dir)>` |
| **TypeScript** | `Map` with string keys for visited state |
| **Ruby** | `Set` with `[x, y, dir]` arrays; `queue.shift` for BFS |
| **Lisp** | Property list or hash-table for visited; `loop` with queue |
| **Julia** | `Set{Tuple{Int,Int,Int}}` for visited; `Queue` from DataStructures |
| **Haskell** | `Data.Set` for visited; `Seq` for BFS queue |

### Assembly Optimization
For row-by-row simulation (matching C implementation):
- Use SIMD (`vpaddd`) for parallel column count updates within a row
- Pack active columns as bitmask for fast splitter propagation
- `popcnt` for counting active beams per row
- Branchless splitter handling with lookup tables for left/right expansion

For generic BFS approach (alternative):
- Pack direction in 2 bits, position in remaining bits for single-word state
- Use `bt`/`bts` for bitset visited tracking

---

## Day 8: 3D Circuits (Union-Find)

### Problem Summary
3D points with connections. Group connected components, find minimum spanning connections.

### C Algorithm
- Union-Find (Disjoint Set Union) with path compression
- Sort edges by weight (Manhattan distance)
- Apply Kruskal's algorithm for MST
- Part 1: Count connected components
- Part 2: Sum of MST edge weights

### Idiomatic Suggestions

| Language | Approach |
|----------|----------|
| **Python** | `networkx` library: `connected_components()`, `minimum_spanning_tree()` |
| **Go** | Custom Union-Find struct with `Find`/`Union` methods |
| **Rust** | `petgraph` crate for graph algorithms; or `union-find` crate |
| **TypeScript** | Class-based Union-Find with path compression |
| **Ruby** | Union-Find with hash for parent tracking |
| **Lisp** | Vector-based Union-Find; `make-array` with fill pointer |
| **Julia** | `LightGraphs.jl` for graph algorithms; native MST functions |
| **Haskell** | `Data.Graph` or `fgl` package; `Data.IntMap` for Union-Find |

### Assembly Optimization
- Flatten 3D coordinates to single index
- Use `rep stosd` for fast array initialization
- Cache-oblivious Union-Find layout

---

## Day 9: Red Corner Rectangles

### Problem Summary
Find largest axis-aligned rectangle with red corners. Part 2: Rectangle must also be inside a polygon.

### C Algorithm
- O(n²) enumeration of point pairs as potential rectangle corners
- For each pair, check if complementary corners exist
- Part 2: Ray casting algorithm for point-in-polygon test
- Track maximum area found

### Idiomatic Suggestions

| Language | Approach |
|----------|----------|
| **Python** | `shapely` library for polygon operations; `scipy.spatial` for point queries |
| **Go** | `orb` library for geometry; or manual ray casting |
| **Rust** | `geo` crate for polygon containment; `HashSet` for point lookup |
| **TypeScript** | `turf.js` for geospatial operations |
| **Ruby** | `rgeo` gem for geometric predicates |
| **Lisp** | Manual implementation; `complex` numbers for 2D points |
| **Julia** | `Meshes.jl` or `PolygonOps.jl` for containment; native broadcasting |
| **Haskell** | `hgeometry` package; or manual with `Data.Set` for points |

### Assembly Optimization
- SIMD point-in-polygon with `vfmadd` for cross products
- Sort points by x-coordinate for cache-friendly access
- Use `pdep`/`pext` (BMI2) for coordinate packing

---

## Day 10: Light Toggle Puzzle

### Problem Summary
Grid of lights with toggle rules. Part 1: Find button presses to turn all lights on. Part 2: Counter puzzle with target values.

### C Algorithm
- **Part 1:** GF(2) Gaussian elimination
  - Build matrix where each button toggles certain lights
  - Solve system over GF(2) (XOR operations)
  - Count button presses in solution
- **Part 2:** Integer Linear Programming relaxation
  - Build constraint matrix
  - Use rational arithmetic RREF
  - DFS/backtracking to find integer solution

### Idiomatic Suggestions

| Language | Approach |
|----------|----------|
| **Python** | `numpy` with `%2` for GF(2); `scipy.optimize.milp` for ILP; `sympy` for exact rational |
| **Go** | `gonum` for matrix operations; manual GF(2) implementation |
| **Rust** | `nalgebra` for matrices; `good_lp` for ILP |
| **TypeScript** | `mathjs` for matrix operations; `javascript-lp-solver` |
| **Ruby** | `matrix` stdlib; `rglpk` gem for linear programming |
| **Lisp** | `gsll` for linear algebra; or manual Gaussian elimination |
| **Julia** | **Ideal:** `JuMP.jl` with `HiGHS` for ILP; `GaloisFields.jl` for GF(2) |
| **Haskell** | `hmatrix` for linear algebra; `glpk-hs` for ILP |

### Assembly Optimization
- Use `pxor` (SSE2) for fast XOR row operations (128-bit)
- `vpxor` (AVX2) for 256-bit row operations
- `vpxorq` (AVX-512) for 512-bit row operations - process 512 lights in single instruction
- `tzcnt`/`lzcnt` for pivot finding in bitpacked rows
- Process 64 columns at once with bitwise operations using GPRs
- For systems > 64 lights: use multi-word bitmasks with SIMD

**Part 2 ILP optimization:** Consider LP relaxation bounds to prune DFS branches earlier.

---

## Day 11: DAG Path Counting

### Problem Summary
Count paths in a directed acyclic graph from sources to sinks.

### C Algorithm
- Topological sort of the DAG
- Dynamic programming: `paths[v] = sum(paths[u] for u in predecessors(v))`
- Memoization to avoid recomputation
- Sum paths ending at sink nodes

### Idiomatic Suggestions

| Language | Approach |
|----------|----------|
| **Python** | `functools.lru_cache` for memoization; `networkx.topological_sort` |
| **Go** | `map` for memoization; recursive with memo |
| **Rust** | `HashMap` for memo; or `petgraph::algo::toposort` |
| **TypeScript** | `Map` for memoization; recursive arrow functions |
| **Ruby** | `Hash.new` with default block for auto-memoization |
| **Lisp** | `memoize` macro; or hash-table with `gethash` |
| **Julia** | `Memoize.jl` package; or `Dict` for manual memoization |
| **Haskell** | **Ideal:** Lazy evaluation provides natural memoization with `fix`; `Data.Map` for explicit |

### Algorithm Notes

**ASM-friendly alternative (level-synchronous DP):** Instead of recursive memoized DFS:
1. Assign topological level to each node (distance from sources)
2. Process nodes level-by-level in forward order
3. For each node, sum path counts from all predecessors
4. Benefits: sequential memory access, naturally parallelizable across nodes in same level, no recursion overhead

This converts O(V + E) recursive with stack overhead to O(V + E) iterative with better cache locality.

### Assembly Optimization
- Linearize DAG for sequential memory access
- Use gather instructions (`vpgatherdd`) for predecessor lookup
- Unroll DP loop for better pipelining
- **Level-synchronous:** Process all nodes at same level in parallel with SIMD
- Prefetch next level's predecessor lists while processing current level

---

## Day 12: Polyomino Packing (Exact Cover)

### Problem Summary
Pack given polyomino shapes into rectangular regions. Count regions that can be exactly filled.

### C Algorithm
- Parse shapes and their rotations/reflections (up to 8 orientations each)
- **Parity pruning:** Checkerboard coloring - shapes have fixed parity, board has fixed black/white ratio
  - Use DP to check if any combination of shape parities can match board
- **Exact Cover:** Backtracking with constraint propagation
  - Each cell must be covered exactly once
  - Each piece copy must be placed exactly once
- Early termination with reachability check

### Recommended Alternative: Dancing Links (DLX)

For exact cover problems like polyomino packing, **Dancing Links (DLX)** is the standard optimal algorithm:

1. **Doubly-linked matrix representation:** Each constraint (cell coverage, piece usage) is a column; each placement is a row
2. **Cover/uncover operations:** O(1) removal and restoration of matrix rows/columns
3. **Knuth's Algorithm X:** Select column with fewest 1s (MRV heuristic), try each row, recurse

**Why DLX is faster (5-100x typical):**
- Efficient constraint propagation: removing a row automatically updates all related columns
- MRV heuristic naturally prunes search tree aggressively
- O(1) backtracking via pointer restoration (no copying)

**Complexity:** Still exponential worst-case, but with much smaller constants and better pruning than naive backtracking.

**Implementation:** Most languages have DLX libraries (see Idiomatic Suggestions below).

### Idiomatic Suggestions

| Language | Approach |
|----------|----------|
| **Python** | `exact_cover` library; `dlx` package for Dancing Links |
| **Go** | Custom DLX implementation; `goroutines` for parallel region solving |
| **Rust** | `dlx` crate; iterator-based solution generation |
| **TypeScript** | Custom backtracking; `Set` for constraint tracking |
| **Ruby** | Recursive backtracking with `yield` for solutions |
| **Lisp** | Natural for recursive backtracking; `defstruct` for DLX nodes |
| **Julia** | `ExactCover.jl` if available; or constraint programming with `JuMP` |
| **Haskell** | **Ideal:** Lazy list of solutions; `Control.Monad` for backtracking monad |

### Assembly Optimization
- Bitboard representation for small regions (≤64 cells)
- `popcnt` for piece size, `pext`/`pdep` for placement
- Use `bzhi` for range masking
- Profile-guided optimization for backtracking order

---

## General Assembly Optimization Techniques

### x86 Extensions to Consider

| Extension | Use Case |
|-----------|----------|
| **SSE2** | Basic SIMD, 128-bit XOR for GF(2) |
| **AVX2** | 256-bit operations, `gather` for sparse data |
| **AVX-512** | 512-bit operations, masking, `vpcompressd` |
| **BMI1/BMI2** | `tzcnt`, `lzcnt`, `pext`, `pdep`, `bzhi` |
| **POPCNT** | Population count for bit counting |
| **ADX** | `adcx`/`adox` for parallel carry chains |

### General Techniques

1. **Branchless programming:** Use `cmov`, bit manipulation, arithmetic
2. **Cache optimization:** Align data, prefetch, minimize indirection
3. **Loop unrolling:** 4-8x unroll for ILP
4. **SIMD parallelism:** Process multiple elements per instruction
5. **Memory layout:** SOA vs AOS based on access patterns
6. **Branch prediction hints:** `likely`/`unlikely` macros
7. **Avoid division:** Use multiplication by reciprocal, bit shifts

---

## Known Issues in Current Implementations

All known issues have been resolved.
