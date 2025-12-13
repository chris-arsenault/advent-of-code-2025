# Advent of Code 2025 - Multi-Language Solutions

## Purpose

This repository implements solutions to Advent of Code 2025 puzzles in multiple programming languages. The goal is an **educational comparison** of language idioms, expressiveness, and performance across a variety of algorithmic problems.

### Design Principles

1. **Idiomatic Code**: Each language implementation should leverage that language's natural features, patterns, and ecosystem (e.g., Python libraries, Julia's linear algebra, Haskell's laziness, Ruby's metaprogramming)

2. **No Cross-Language Calls**: Solutions must be self-contained within their language. The only exception is standard wrapper bindings for independent third-party libraries (e.g., Python calling into NumPy's C backend)

3. **Readability Over Safety**: Prioritize clear, understandable code. Avoid:
   - Overly complex safety checks
   - Excessive runtime boundary validation
   - Defensive programming patterns that obscure the algorithm

4. **Not Production Code**: These solutions are for learning and comparison. They assume valid input and do not need enterprise-grade error handling

---

## Repository Structure

```
advant-code-2025/
├── day1/ ... day12/     # Each day's solutions
│   ├── input.txt        # Puzzle input (or input.xt for day 12)
│   ├── problem.txt      # Problem statement
│   ├── main.c           # C implementation (reference)
│   ├── main.py          # Python
│   ├── main.go          # Go
│   ├── main.rs          # Rust
│   ├── main.ts          # TypeScript
│   ├── main.rb          # Ruby
│   ├── main.lisp        # Common Lisp
│   ├── main.jl          # Julia
│   ├── Main.hs          # Haskell
│   └── main.asm         # x86-64 Assembly (optional)
├── shared/
│   └── utils.asm        # Shared assembly utilities
├── run_all.py           # Test runner
├── ALGORITHMS.md        # Algorithm documentation
├── SHARED_ASM.md        # Assembly utilities documentation
└── CLAUDE.md            # This file
```

---

## Reference Documentation

### ALGORITHMS.md

Describes the algorithm used by the C implementation for each day, plus idiomatic suggestions for other languages. Consult this when:
- Implementing a new language version
- Understanding the expected approach
- Looking for language-specific optimizations (SIMD, libraries, etc.)

### SHARED_ASM.md

Documents the shared x86-64 assembly utilities in `shared/utils.asm`. This is a **living document** - update it when adding or modifying shared assembly code.

---

## Running Solutions

### run_all.py

The test runner executes all solutions across all days and languages.

**Basic usage:**
```bash
./run_all.py
```

**With build/install step:**
```bash
./run_all.py --install
```

**What it does:**

1. **Build Phase** (with `--install`):
   - Compiles C: `cc -O2 -std=c11 main.c -o day{N}`
   - Compiles Rust: `rustc -O main.rs` or `cargo build --release`
   - Compiles Haskell: `ghc -O2 Main.hs`
   - Compiles ASM: `nasm -felf64` + links with shared/utils.o
   - Installs Python deps from `requirements.txt`

2. **Execution Phase:**
   - Runs each language's solution for each day
   - Captures stdout, extracts `elapsed_ms=X.XXX` timing
   - Extracts answer key=value pairs for comparison

3. **Validation:**
   - Compares all outputs against C (the reference implementation)
   - Reports mismatches with expected vs actual values
   - Shows timing delta (language - C) for each solution

4. **Summary:**
   - Prints timing table across all days and languages
   - Marks failed/missing runs as ERR

**Output Format:**

All solutions must output in this format:
```
answer1=VALUE answer2=VALUE elapsed_ms=X.XXX
```

Example:
```
part1=1234 part2=5678 elapsed_ms=0.456
```

**Supported Languages:**

| Extension | Command | Build Required |
|-----------|---------|----------------|
| `.c` | `./day{N}` | Yes |
| `.py` | `python main.py` | No |
| `.go` | `go run main.go` | No |
| `.rs` | `./day{N}_rs` | Yes |
| `.ts` | `ts-node main.ts` | No |
| `.rb` | `ruby main.rb` | No |
| `.lisp` | `sbcl --script main.lisp` | No |
| `.jl` | `julia main.jl` | No |
| `.hs` | `./day{N}_hs` | Yes |
| `.asm` | `./day{N}_asm` | Yes |

---

## Maintaining shared/utils.asm

The shared assembly utilities provide common functions for ASM solutions. **Keep the code and documentation in sync.**

### Adding a New Function

1. **Implement** the function in `shared/utils.asm`:
   ```asm
   global new_function_name

   ;------------------------------------------------------------------------------
   ; Brief description
   ; Input:  rdi = ...
   ; Output: rax = ...
   ;------------------------------------------------------------------------------
   new_function_name:
       ; implementation
       ret
   ```

2. **Document** in `SHARED_ASM.md`:
   - Add function signature
   - Parameter/return table
   - Behavior description
   - Usage example

3. **Rebuild** shared object:
   ```bash
   nasm -felf64 shared/utils.asm -o shared/utils.o
   ```

### Extracting Code from Day Solutions

When you identify reusable code in a day's `main.asm`:

1. Move the function to `shared/utils.asm`
2. Add `global function_name` declaration
3. Add `extern function_name` to the day's main.asm
4. Update SHARED_ASM.md with documentation
5. Update the "Future Additions" checklist in SHARED_ASM.md

### Modifying Existing Functions

1. Update the implementation in `shared/utils.asm`
2. Update the corresponding section in `SHARED_ASM.md`
3. Verify all ASM solutions still work: `./run_all.py --install`

---

## Adding a New Language

1. Create `main.{ext}` in each day directory
2. Ensure output format matches: `key=value ... elapsed_ms=X.XXX`
3. Add language config to `run_all.py`:
   ```python
   LANGS = {
       ...
       "lang": {"exe": "command main.ext", "build": False},
   }
   ```
4. Add build logic to `ensure_built()` if compilation required
5. Consider adding idiomatic suggestions to ALGORITHMS.md

---

## Common Issues

### CRLF Line Endings
Windows-style line endings (`\r\n`) can break parsing. Solutions should handle or strip `\r` characters, especially when checking for blank lines.

### Lazy Evaluation (Haskell)
Force evaluation before timing ends using `seq`:
```haskell
p1 `seq` p2 `seq` return ()
```

### Input File Names
Most days use `input.txt`, but day 12 uses `input.xt`. Always verify the filename in the C reference.

### Timing Accuracy
Use CPU time, not wall time, for consistent measurements:
- C: `clock_gettime(CLOCK_MONOTONIC, ...)`
- Python: `time.perf_counter()`
- Lisp: `get-internal-run-time` (not `get-internal-real-time`)
