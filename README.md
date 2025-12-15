# Advent of Code 2025 - The Polyglot Workshop

*A message from the North Pole Engineering Division*

---

Greetings, traveler!

The Elves have good news and bad news.

The good news is that you've discovered our secret workshop where we solve the daily puzzles that keep the North Pole running smoothly. As it turns out, different Elves prefer different tools for their work - some swear by the ancient scrolls of C, others prefer the elegant incantations of Haskell, and a few ambitious ones have even learned to speak directly to the machine spirits in Assembly.

The bad news? We need help documenting all of it before Christmas!

## What Is This Place?

This repository contains solutions to [Advent of Code 2025](https://adventofcode.com/2025) implemented in many programming languages. Each day's puzzle is solved multiple ways, showcasing how different languages approach the same problem.

**Languages in the Workshop:**
C | Python | Go | Rust | TypeScript | Ruby | Common Lisp | Julia | Haskell | x86-64 Assembly

## Daily Puzzle Archives

The Elves have prepared detailed documentation for each day's challenge. Click a day to learn about the algorithm, performance results, and language-specific insights:

| Day | Puzzle | Highlights |
|-----|--------|------------|
| [Day 1](day1/README.md) | Dial Rotation | Branchless ASM with `cmov` |
| [Day 2](day2/README.md) | Invalid Product IDs | ~33x speedup via precomputation |
| [Day 3](day3/README.md) | Battery Joltage | Monotonic stack algorithm |
| [Day 4](day4/README.md) | Paper Rolls Grid | BFS wavefront + SIMD `popcnt` |
| [Day 5](day5/README.md) | Interval Merging | Two-pointer optimization |
| [Day 6](day6/README.md) | Vertical Math | Big integer parsing |
| [Day 7](day7/README.md) | Beam Splitter | Row-by-row simulation |
| [Day 8](day8/README.md) | 3D Circuits | Union-Find + Kruskal MST |
| [Day 9](day9/README.md) | Red Rectangles | Ray casting point-in-polygon |
| [Day 10](day10/README.md) | Light Toggle | GF(2) + ILP with DFS |
| [Day 11](day11/README.md) | DAG Paths | Topological DP |
| [Day 12](day12/README.md) | Polyomino Packing | Dancing Links exact cover |

## Performance Insights

*The Elves have been racing their solutions against each other. Here's what they've learned:*

### Average Timing Across All Days (ms)

| Language | Avg Time | Notes |
|----------|----------|-------|
| **ASM** | 26.5 | Fastest overall - hand-tuned hot paths |
| **C** | 39.1 | Solid baseline |
| **Rust** | 43.7 | Close to C with safety guarantees |
| **Go** | 94.8 | Consistent but slower than expected |
| **Lisp** | 511.2 | Native bignums help on arithmetic days |
| **TypeScript** | 597.8 | V8 startup overhead dominates short problems |
| **Python** | 956.2 | Interpreted overhead, but readable |
| **Haskell** | 1101.5 | Lazy evaluation: sometimes magical, sometimes painful |
| **Julia** | 1187.0 | JIT overhead hurts on short-running puzzles |
| **Ruby** | 3296.2 | Expressiveness over speed |

### Notable Observations

The Elves discovered some interesting patterns:

**Algorithm beats optimization:** Day 2's precomputation approach gives ASM a ~33x speedup over C's naive per-number checking. The C reference deliberately uses the slow approach to demonstrate that algorithm choice matters more than language.

**Library quality varies:** Day 12 shows Python matching C at 30ms because the `exact_cover` library uses efficient C extensions. Meanwhile, Haskell's lazy backtracking is 238x slower - a poor fit for the search paradigm.

**Startup overhead matters:** TypeScript and Julia consistently show 100-300x slowdowns on fast problems (Days 1, 5, 11) due to runtime initialization. On longer problems (Days 9, 10), they perform much better relatively.

**Rust matches or beats C:** Across most problems, Rust is within 1.5x of C, and occasionally faster (Days 3, 6, 12). The zero-cost abstractions live up to their name.

### Memory Usage (KiB Average)

| Language | Avg Memory | Notes |
|----------|------------|-------|
| **ASM** | 2,192 | Minimal runtime footprint |
| **C** | 3,552 | Just the code and data |
| **Rust** | 4,956 | Small overhead for safety checks |
| **Ruby** | 16,499 | VM and object system |
| **Go** | 21,398 | Garbage collector overhead |
| **Haskell** | 24,607 | GHC runtime |
| **Python** | 35,162 | Interpreter + libraries |
| **Lisp** | 47,708 | SBCL with standard library |
| **TypeScript** | 216,691 | Node.js/V8 engine |
| **Julia** | 324,067 | JIT compiler in memory |

### Complexity vs Performance

The Elves noticed that cyclomatic complexity doesn't always predict performance:

- **Day 10** has the highest complexity (C: 139, avg: 72) and longest runtime (avg: 4607ms)
- **Day 11** has the lowest complexity (avg: 9) and fastest runtime (avg: 89ms)
- **Day 12** shows that algorithm choice (DLX vs backtracking) matters more than code complexity
- **Haskell** consistently has the lowest complexity (avg: 13) but middling performance - functional style hides branching in composition

## Quick Start

One of the younger Elves built a contraption that runs everything at once:

```bash
# Run all solutions (assumes dependencies installed)
./run_all.py

# Build everything first, then run
./run_all.py --install
```

The machine will compile what needs compiling, execute each solution, compare outputs to the C reference, and emit summary tables for timings, memory usage, line counts, and a simple cyclomatic complexity heuristic.

### Target a subset of days
```bash
# Comma-separated list
./run_all.py --day 1,3,5
```

### Control which stats print
```bash
# Disable memory and complexity tables
./run_all.py --no-memory --no-complexity
```

All tables include per-day averages and per-language averages at the bottom. Failed runs are excluded from averages.

### Safety note
When validating changes locally, prefer running a single early day (e.g., `--day 1`) instead of the full suite to avoid long or flaky runs.

## Docker Usage

A container recipe is provided for a consistent toolchain.

Build:
```bash
docker build -t aoc2025 .
```

Run (installs/builds, then executes the suite):
```bash
docker run --rm -it aoc2025
```

Run a specific subset (e.g., day 1 only) inside the container:
```bash
docker run --rm -it aoc2025 python3 run_all.py --day 1
```

**Important:** Docker runs are typically ~2–3× slower than running directly on your host due to container overhead and missing host-level CPU optimizations. Prefer local runs for performance comparisons; use the container for reproducibility.

## Workshop Documents

The senior Elves have prepared several reference documents:

| Document | Purpose |
|----------|---------|
| [CLAUDE.md](CLAUDE.md) | Operating procedures for the workshop |
| [ALGORITHMS.md](ALGORITHMS.md) | How each puzzle is solved, with language-specific tips |
| [SHARED_ASM.md](SHARED_ASM.md) | Reference for the shared Assembly utilities |

## Workshop Layout

```
day1/ through day12/    The puzzle solutions, one folder per day
shared/                 Shared utilities (currently Assembly helpers)
run_all.py              The Elf-built test contraption
```

Each day folder contains:
- `README.md` - Day-specific algorithm and performance documentation
- `problem.txt` - The puzzle description
- `input.txt` - The puzzle input
- Solutions in various languages (`main.py`, `main.c`, `Main.hs`, etc.)

## Lessons from the Workshop

*The Elves have distilled their wisdom into these observations:*

### On Language Choice
- **Speed matters:** For computation-heavy puzzles (days 9, 10), language choice can mean 100x+ difference
- **Algorithms matter more:** Day 2's ~33x speedup came from algorithm change, not language
- **Libraries matter:** Day 12's Python `exact_cover` library matches C despite interpreted overhead

### On Assembly Optimization
- **Branchless techniques** (`cmov`, `setcc`) reduce branch misprediction
- **SIMD** helps for batch operations (`popcnt`, `pxor`) but not small matrices
- **Cache locality** (Day 10's DFS state collapse) often beats vectorization
- **Shared utilities** (`sort_u64`, `lower_bound`) avoid reinventing wheels
- **Diminishing returns:** ASM is often only 10-20% faster than C; the effort rarely pays off

### On Surprises
- Haskell's lazy evaluation is efficient for DAG traversal (Day 11), terrible for backtracking (Day 12)
- Julia's JIT overhead makes it slow on short-running puzzles (100x+ on Days 1, 5, 11)
- Ruby's expressiveness comes at 100x performance cost on tight loops (Days 9, 10)
- Go is consistently slower than expected - its runtime overhead shows even on simple problems

## A Note from the Workshop Manager

These solutions prioritize clarity over defensive coding. The Elves assume the input is valid (it comes from a trusted source, after all) and focus on expressing the algorithm cleanly in each language's native style.

This is a teaching workshop, not a production facility. Please don't use these patterns for anything load-bearing - the reindeer have enough to worry about.

## License

MIT License - See [LICENSE](LICENSE)

The Elves believe in sharing knowledge freely. Use this code however you wish, though a mention of the North Pole Engineering Division is always appreciated.

---

*"May your algorithms be optimal and your caches warm!"*
*- The North Pole Engineering Division*
