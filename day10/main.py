from __future__ import annotations

import re
from datetime import datetime
from pathlib import Path
import time
from typing import List, Tuple

import pulp


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


def part1(machines: list[tuple[str, list[list[int]], list[int]]]) -> int:
    total = 0
    for pattern, buttons, _ in machines:
        lights = len(pattern)
        target_mask = 0
        for i, ch in enumerate(pattern):
            if ch == "#":
                target_mask |= 1 << i
        button_masks = []
        for b in buttons:
            mask = 0
            for idx in b:
                if idx < lights:
                    mask |= 1 << idx
            button_masks.append(mask)

        best = None
        n = len(button_masks)
        for mask in range(1 << n):
            state = 0
            for i in range(n):
                if mask >> i & 1:
                    state ^= button_masks[i]
            if state == target_mask:
                presses = mask.bit_count()
                if best is None or presses < best:
                    best = presses
        total += best or 0
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
    t0 = time.perf_counter()
    p1 = part1(machines)
    p2 = part2(machines)
    elapsed_ms = (time.perf_counter() - t0) * 1000
    timestamp = datetime.now().isoformat(timespec="seconds")
    print(f"[{timestamp}] min_lights_presses={p1} min_counter_presses={p2} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
