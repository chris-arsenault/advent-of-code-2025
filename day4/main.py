from __future__ import annotations

from collections import deque
from datetime import datetime
from pathlib import Path
import time

NEIGHBORS = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
]


def parse_grid(lines: list[str]) -> set[tuple[int, int]]:
    rolls: set[tuple[int, int]] = set()
    for r, line in enumerate(lines):
        for c, ch in enumerate(line.rstrip("\n")):
            if ch == "@":
                rolls.add((r, c))
    return rolls


def neighbor_counts(rolls: set[tuple[int, int]]) -> dict[tuple[int, int], int]:
    counts: dict[tuple[int, int], int] = {pos: 0 for pos in rolls}
    for r, c in rolls:
        cnt = 0
        for dr, dc in NEIGHBORS:
            if (r + dr, c + dc) in rolls:
                cnt += 1
        counts[(r, c)] = cnt
    return counts


def part1(rolls: set[tuple[int, int]]) -> int:
    counts = neighbor_counts(rolls)
    return sum(1 for v in counts.values() if v < 4)


def part2(rolls: set[tuple[int, int]]) -> int:
    counts = neighbor_counts(rolls)
    removed: set[tuple[int, int]] = set()
    q: deque[tuple[int, int]] = deque([pos for pos, cnt in counts.items() if cnt < 4])

    while q:
        pos = q.popleft()
        if pos in removed:
            continue
        removed.add(pos)
        r, c = pos
        for dr, dc in NEIGHBORS:
            nbr = (r + dr, c + dc)
            if nbr in rolls and nbr not in removed:
                counts[nbr] -= 1
                if counts[nbr] < 4:
                    q.append(nbr)
    return len(removed)


def main() -> None:
    input_path = Path(__file__).with_name("input.txt")
    lines = input_path.read_text().splitlines()
    rolls = parse_grid(lines)
    t0 = time.perf_counter()
    accessible = part1(rolls)
    removed = part2(rolls)
    elapsed_ms = (time.perf_counter() - t0) * 1000
    timestamp = datetime.now().isoformat(timespec="seconds")
    print(f"[{timestamp}] accessible={accessible} removable_total={removed} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
