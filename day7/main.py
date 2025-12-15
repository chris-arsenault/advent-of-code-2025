from __future__ import annotations

from collections import deque
from pathlib import Path
import time


def load_grid(lines: list[str]) -> tuple[list[str], int, int]:
    grid: list[str] = []
    start_r = start_c = -1
    for r, line in enumerate(lines):
        row = line.rstrip("\n")
        grid.append(row)
        if "S" in row:
            start_r = r
            start_c = row.index("S")
    if start_r < 0:
        raise ValueError("missing start S")
    return grid, start_r, start_c


def part1(grid: list[str], start_r: int, start_c: int) -> int:
    height = len(grid)
    width = len(grid[0])
    active = {start_c}
    splits = 0
    for r in range(start_r, height):
        next_active: set[int] = set()
        seen: set[int] = set()
        queue = deque(active)
        while queue:
            c = queue.popleft()
            if c in seen:
                continue
            seen.add(c)
            cell = grid[r][c]
            if cell == "^":
                splits += 1
                if c > 0:
                    queue.append(c - 1)
                if c + 1 < width:
                    queue.append(c + 1)
            else:
                next_active.add(c)
        active = next_active
        if not active:
            break
    return splits


def part2(grid: list[str], start_r: int, start_c: int) -> int:
    height = len(grid)
    width = len(grid[0])
    active = {start_c: 1}
    for r in range(start_r, height):
        next_active: dict[int, int] = {}
        for c, count in active.items():
            cell = grid[r][c]
            if cell == "^":
                if c > 0:
                    next_active[c - 1] = next_active.get(c - 1, 0) + count
                if c + 1 < width:
                    next_active[c + 1] = next_active.get(c + 1, 0) + count
            else:
                next_active[c] = next_active.get(c, 0) + count
        active = next_active
        if not active:
            break
    return sum(active.values())


def main() -> None:
    t0 = time.perf_counter()
    lines = Path(__file__).with_name("input.txt").read_text().splitlines()
    grid, sr, sc = load_grid(lines)
    p1 = part1(grid, sr, sc)
    p2 = part2(grid, sr, sc)
    elapsed_ms = (time.perf_counter() - t0) * 1000
    print(f"splits={p1} timelines={p2} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
