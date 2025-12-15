from __future__ import annotations

import re
import time
from pathlib import Path

import numpy as np
import pulp

_start_time = time.perf_counter()


def parse_machine(line: str) -> tuple[str, list[list[int]], list[int]]:
    pattern = re.search(r"\[(.*?)\]", line).group(1)
    button_parts = re.findall(r"\(([^)]*)\)", line)
    buttons: list[list[int]] = []
    for part in button_parts:
        part = part.strip()
        if not part:
            buttons.append([])
        else:
            buttons.append([int(x) for x in part.split(",") if x.strip()])
    target = [int(x) for x in re.search(r"\{([^}]*)\}", line).group(1).split(",")]
    return pattern, buttons, target


def load(lines: list[str]) -> list[tuple[str, list[list[int]], list[int]]]:
    machines = []
    for line in lines:
        line = line.strip()
        if not line:
            continue
        machines.append(parse_machine(line))
    return machines


def gf2_solve(buttons: list[list[int]], target: list[int], lights: int) -> int | None:
    """Solve XOR system over GF(2) using numpy and find minimum presses."""
    n_buttons = len(buttons)
    if n_buttons == 0:
        return 0 if all(t == 0 for t in target) else None

    # Build augmented matrix [A | b] over GF(2)
    # Rows = lights, Cols = buttons + 1 (for target)
    aug = np.zeros((lights, n_buttons + 1), dtype=np.uint8)
    for b_idx, btn in enumerate(buttons):
        for light in btn:
            if light < lights:
                aug[light, b_idx] = 1
    for i, t in enumerate(target):
        aug[i, n_buttons] = t

    # Gaussian elimination over GF(2)
    pivot_cols = []
    row = 0
    for col in range(n_buttons):
        # Find pivot
        pivot = None
        for r in range(row, lights):
            if aug[r, col] == 1:
                pivot = r
                break
        if pivot is None:
            continue
        # Swap rows
        aug[[row, pivot]] = aug[[pivot, row]]
        pivot_cols.append(col)
        # Eliminate
        for r in range(lights):
            if r != row and aug[r, col] == 1:
                aug[r] = (aug[r] + aug[row]) % 2
        row += 1

    rank = len(pivot_cols)

    # Check consistency
    for r in range(rank, lights):
        if aug[r, n_buttons] == 1:
            return None

    # Free variables
    free_cols = [c for c in range(n_buttons) if c not in pivot_cols]
    n_free = len(free_cols)

    # Enumerate all 2^n_free assignments to find minimum weight solution
    best = None
    for mask in range(1 << n_free):
        sol = np.zeros(n_buttons, dtype=np.uint8)
        for i, fc in enumerate(free_cols):
            sol[fc] = (mask >> i) & 1

        # Back-substitute to find pivot values
        valid = True
        for i in range(rank - 1, -1, -1):
            pc = pivot_cols[i]
            val = aug[i, n_buttons]
            for c in range(pc + 1, n_buttons):
                val = (val + aug[i, c] * sol[c]) % 2
            sol[pc] = val

        if valid:
            weight = int(sol.sum())
            if best is None or weight < best:
                best = weight

    return best


def part1(machines: list[tuple[str, list[list[int]], list[int]]]) -> int:
    total = 0
    for pattern, buttons, _ in machines:
        lights = len(pattern)
        target = [1 if ch == "#" else 0 for ch in pattern]
        result = gf2_solve(buttons, target, lights)
        total += result or 0
    return total


def part2(machines: list[tuple[str, list[list[int]], list[int]]]) -> int:
    total = 0
    for _, buttons, targets in machines:
        counters = len(targets)
        prob = pulp.LpProblem("presses", pulp.LpMinimize)
        x = [pulp.LpVariable(f"x_{i}", lowBound=0, cat="Integer") for i in range(len(buttons))]
        prob += pulp.lpSum(x)
        for c in range(counters):
            prob += pulp.lpSum(x[i] for i, b in enumerate(buttons) if c in b) == targets[c]
        prob.solve(pulp.PULP_CBC_CMD(msg=0))
        total += int(sum(v.value() for v in x))
    return total


def main() -> None:
    lines = Path(__file__).with_name("input.txt").read_text().splitlines()
    machines = load(lines)
    p1 = part1(machines)
    p2 = part2(machines)
    elapsed_ms = (time.perf_counter() - _start_time) * 1000
    print(f"min_lights_presses={p1} min_counter_presses={p2} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
