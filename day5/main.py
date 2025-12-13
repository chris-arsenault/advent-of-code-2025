from __future__ import annotations

from pathlib import Path
import time
from intervaltree import IntervalTree


def parse(text: str) -> tuple[list[tuple[int, int]], list[int]]:
    top, bottom = text.split("\n\n", 1)
    ranges = []
    for line in top.strip().splitlines():
        a, b = map(int, line.strip().split("-"))
        ranges.append((a, b))
    ids = [int(line.strip()) for line in bottom.strip().splitlines() if line.strip()]
    return ranges, ids


def solve(text: str) -> tuple[int, int]:
    ranges, ids = parse(text)

    # Build interval tree with merged intervals
    tree = IntervalTree()
    for a, b in ranges:
        tree.addi(a, b + 1)  # intervaltree uses half-open intervals [a, b)
    tree.merge_overlaps()

    # Part 1: Count IDs that fall within any interval
    fresh_count = sum(1 for x in ids if tree.overlaps(x))

    # Part 2: Total span of merged intervals
    total_fresh = sum(iv.end - iv.begin for iv in tree)

    return fresh_count, total_fresh


def main() -> None:
    input_path = Path(__file__).with_name("input.txt")
    text = input_path.read_text()
    t0 = time.perf_counter()
    p1, p2 = solve(text)
    elapsed_ms = (time.perf_counter() - t0) * 1000
    print(f"available_fresh={p1} total_fresh_ids={p2} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
