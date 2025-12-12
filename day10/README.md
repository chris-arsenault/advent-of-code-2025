# Day 10 Solution Overview

This program solves both parts of the “Factory” puzzle.

## Input parsing
- One machine per line.
- Indicators pattern is inside `[...]`.
- Buttons are listed in `(...)`; each number is a zero-based index of a light/counter the button affects.
- Joltage targets are inside `{...}`.
- Lines are read into `lines[MAX_LINES][LINE_BUF]`; trailing newlines are stripped.

## Part 1 (indicator lights)
- Model as a linear system over GF(2): each button toggles certain lights; pressing a button an odd number of times flips those lights.
- Build a binary matrix `lights x buttons` (`matrix[light][button]` bit-set if button toggles that light) and a target vector from the pattern (`#` => 1).
- Run Gaussian elimination to reduced row echelon form (`rref`) to find pivots, rank, and detect inconsistencies.
- Enumerate all assignments to free variables when there are ≤ `ENUM_LIMIT` (20) free columns; otherwise pick the all‑zeros free solution.
- Back-substitute to get a complete solution and track its Hamming weight (total button presses). Pick the minimum weight; sum across machines.

## Part 2 (joltage counters)
- Ignore the indicator pattern; use `{...}` as target counts. Counters start at 0; each button adds +1 to the counters it lists.
- Build an augmented matrix (counters x buttons) and run Gaussian elimination to RREF to expose free columns (non-pivot buttons). Pivot rows are stored as `rhs` and `coef` against the free variables.
- Free-variable search (free count is tiny here, ≤3):
  - A quick bounded search (cap 400 per free) seeds an initial best total.
  - A full DFS explores non-negative assignments to free variables, pruning when the running total reaches the current best. For each assignment, pivot values are computed as `rhs - sum(free * coef)`; they must be non-negative integers. Total presses = sum(free vars) + sum(pivot vars).
- Sum the minimal press counts over all machines.

## Key constants
- `MAX_LIGHTS` 1024, `MAX_BUTTONS` 256, `ENUM_LIMIT` 20.
- Bitsets store button columns in chunks of 64 for the GF(2) matrix (part 1); part 2 uses doubles for a tiny matrix.

## Files
- `day10/main.c` — full implementation for both parts.
