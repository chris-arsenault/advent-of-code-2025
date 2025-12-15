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

## Workshop Methodology

*How the Elves actually built all of this:*

The solutions in this workshop didn't appear by magic (despite what some junior Elves claim). Here's how the polyglot collection came together:

### Phase 1: The C Foundation

The senior Elf began with C implementations as the **reference algorithm** for each puzzle. These prioritize clarity over cleverness—few micro-optimizations, occasional brute force where it kept the code readable. The C solutions establish correctness and provide baseline timings against which all other languages are measured.

### Phase 2: The Polyglot Expansion

With C solutions producing verified answers, the other languages were implemented using **C as the test suite**. Each new implementation had to match C's output exactly. This let the Elves focus on expressing each algorithm idiomatically without worrying about correctness—if it matched C, it was right.

### Phase 3: The Language Specialist Review

An independent reviewer (a particularly discerning Elf from the Quality Assurance division) examined each language's solutions and identified **algorithm-language pairings** that deserved more attention. The goal: ensure each language showcased its natural strengths rather than just translating C verbatim.

Examples of this guidance:
- *Haskell*: Emphasize recursion and lazy evaluation patterns
- *Julia*: Leverage linear algebra and mathematical expressiveness
- *Lisp*: Exploit native bignums and macro capabilities
- *Ruby*: Use metaprogramming and expressive iterators

### Phase 4: Assembly Refinement

The x86-64 Assembly implementations received special treatment. The Elves established **explicit style guidelines** for ASM code: consistent register usage, clear section organization, documented calling conventions. The shared utilities in `shared/utils.asm` emerged from this disciplined approach.

### Phase 5: The AI Council

Finally, the workshop convened a council of artificial advisors:

- **ChatGPT 5.2** performed multi-pass reviews of the Assembly implementations, providing feedback on idiomatic patterns, register allocation, and micro-optimizations
- **Claude Code** evaluated the suggestions and implemented changes that made sense within each problem's context

Most days received **2 review passes**. Days 8 (3D Circuits) and Day 10 (Light Toggle) received **4 passes** due to their algorithmic complexity—Union-Find/Kruskal MST and GF(2)/ILP respectively demanded extra scrutiny.

*The Elves found this human-AI collaboration quite productive, though they're still debating whether to give the AIs their own workbenches.*

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

### Average Timing Across All Days (Internal, ms)

| Language | Avg Time | Avg Startup | Notes |
|----------|----------|-------------|-------|
| **ASM** | 25.0 | 1.6 | Fastest - hand-tuned hot paths |
| **C** | 35.5 | 2.1 | Solid baseline |
| **Rust** | 41.0 | 1.8 | Close to C with safety guarantees |
| **Go** | 45.4 | 2.7 | Consistent, competitive runtime |
| **TypeScript** | 87.1 | 494 | V8 is fast once running |
| **Lisp** | 468.8 | 41 | Native bignums, SBCL startup overhead |
| **Julia** | 751.5 | 439 | JIT-compiled code is efficient |
| **Python** | 871.8 | 49 | Interpreted overhead, but readable |
| **Haskell** | 1053.6 | 2.5 | Lazy evaluation: sometimes magical, sometimes painful |
| **Ruby** | 3165.2 | 33 | Expressiveness over speed |

*Internal timing excludes process startup. Startup column shows average initialization overhead in ms.*

### Notable Observations

The Elves discovered some interesting patterns:

**Startup overhead is massive for JIT languages:** TypeScript averages 494ms startup, Julia 439ms. On sub-millisecond problems (Days 1, 5, 11), this made them appear 100-500x slower than they actually are. Internal timing reveals TypeScript is only 2.5x slower than C on average, not 100x.

**Algorithm beats optimization:** Day 2's precomputation approach gives ASM a ~76x speedup over C's naive per-number checking. Day 12 shows ASM/Rust (using DLX) are 200x faster than C's naive backtracking.

**Library quality varies:** Day 12 shows Python is 13x *faster* than C because `exact_cover` uses efficient C extensions. Day 8 shows Python with `networkx` is 53x slower - library overhead matters.

**Haskell lazy evaluation has edge cases:** Union-Find (Day 8, 36x slower) requires immutable copying. Backtracking (Day 12, 241x slower) is a pathological case for lazy evaluation.

**Rust matches or beats C:** Across most problems, Rust averages 1.16x of C internally. Days 3, 5, 11 show Rust outperforming C. Zero-cost abstractions live up to their name.

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

- **Day 10** has the highest complexity (C: 139, avg: 72) and longest internal runtime (avg: 4386ms)
- **Day 11** has the lowest complexity (avg: 9) and fastest internal runtime (avg: 4.3ms)
- **Day 12** shows that algorithm choice (DLX vs backtracking) matters more than code complexity - ASM/Rust are 200x faster than C
- **Haskell** consistently has the lowest complexity (avg: 14) but middling performance - functional style hides branching in composition

### Notes from the AI Council

*Observations from the human-AI collaboration described in the Methodology section:*

**Day 10 broke the AIs:** The Light Toggle puzzle (GF(2) linear algebra + ILP) was by far the hardest problem for LLMs to conceptualize. Multiple failed attempts across both ChatGPT and Claude, with significant human coaxing required to achieve a correct implementation in *any* language. The AIs could discuss the theory but struggled to translate it into working code.

**Lisp defeated the language models:** Common Lisp was consistently the hardest language for the AI advisors to get right. Most days required manual balancing of parentheses by a human Elf. The models would lose track of nesting depth, drop closing parens, or misplace them entirely. Perhaps the training data underrepresents properly-formatted Lisp?

**Day 12's suspicious shortcut:** For the workshop's puzzle input, Day 12 (Polyomino Packing) can be solved with just a size test—no Dancing Links or backtracking required at all. Only C and ASM implement the full exact-cover algorithm. It remains unclear whether this is a special case unique to this input or an intentional problem space reduction. Other inputs may require the full algorithm.

**First-pass ASM read like compiler output:** Initial Assembly implementations from the AI advisors almost universally resembled compiler-generated code rather than hand-rolled ASM—excessive stack spills, missed register allocation opportunities, no exploitation of x86 idioms. This suggests training data is dominated by disassembled binaries and compiler output rather than human-written Assembly. The style guidelines and multi-pass reviews were essential to achieve idiomatic results.

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
- **Speed matters internally:** For computation-heavy puzzles (Days 9, 10), language choice can mean 100x+ difference even with internal timing
- **Startup matters for short problems:** TypeScript/Julia appear 100-500x slower on sub-millisecond problems due to ~500ms startup
- **Algorithms matter most:** Day 2's ~76x speedup and Day 12's ~200x speedup came from algorithm choice (precomputation, DLX vs backtracking)
- **Libraries matter:** Day 12's Python `exact_cover` is 13x *faster* than C; Day 8's `networkx` is 53x slower

### On Assembly Optimization
- **Branchless techniques** (`cmov`, `setcc`) reduce branch misprediction
- **SIMD** helps for batch operations (`popcnt`, `pxor`) but not small matrices
- **Cache locality** (Day 10's DFS state collapse) often beats vectorization
- **Shared utilities** (`sort_u64`, `lower_bound`) avoid reinventing wheels
- **Diminishing returns:** ASM is often only 10-20% faster than C; the effort rarely pays off

### On Startup vs Algorithm Time
Internal timing (algorithm only) reveals surprising truths:
- **TypeScript is 2.5x C on average**, not 100x - the 494ms V8 startup dominated external measurements
- **Julia is 21x C on average internally**, not 100x - the 439ms JIT startup was misleading
- **Go is competitive (1.3x C)** when startup is excluded - the 2.7ms runtime init matters on short problems
- **Compiled languages (C, Rust, ASM, Go, Haskell)** have <3ms startup; interpreted/JIT languages have 30-500ms

### On Surprises
- Haskell's lazy evaluation is efficient for DAG traversal (Day 11, 5.2x C), terrible for backtracking (Day 12, 262x C)
- Julia's JIT helps numeric work (Day 3: 0.13x C) but hurts graph traversal (Day 11: 41x C)
- Ruby is genuinely slow on tight loops (Days 9, 10: 135-151x C) - not a startup issue
- Go has problem-specific overhead: 64x C on Day 7's hash map operations, but 0.64x C on Day 11's graph traversal

## A Note from the Workshop Manager

These solutions prioritize clarity over defensive coding. The Elves assume the input is valid (it comes from a trusted source, after all) and focus on expressing the algorithm cleanly in each language's native style.

This is a teaching workshop, not a production facility. Please don't use these patterns for anything load-bearing - the reindeer have enough to worry about.

## License

MIT License - See [LICENSE](LICENSE)

The Elves believe in sharing knowledge freely. Use this code however you wish, though a mention of the North Pole Engineering Division is always appreciated.

---

*"May your algorithms be optimal and your caches warm!"*
*- The North Pole Engineering Division*
