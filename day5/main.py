from __future__ import annotations

from bisect import bisect_left
from datetime import datetime
from pathlib import Path
import time


def merge_ranges(ranges: list[tuple[int, int]]) -> list[tuple[int, int]]:
    ranges = sorted(ranges)
    merged: list[tuple[int, int]] = []
    for a, b in ranges:
        if not merged or a > merged[-1][1] + 1:
            merged.append((a, b))
        else:
            merged[-1] = (merged[-1][0], max(merged[-1][1], b))
    return merged


def in_any(merged: list[tuple[int, int]], x: int) -> bool:
    i = bisect_left(merged, (x, -10**18))
    if i < len(merged) and merged[i][0] <= x <= merged[i][1]:
        return True
    if i > 0 and merged[i - 1][0] <= x <= merged[i - 1][1]:
        return True
    return False


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
    merged = merge_ranges(ranges)

    fresh_count = sum(1 for x in ids if in_any(merged, x))
    total_fresh = sum(b - a + 1 for a, b in merged)
    return fresh_count, total_fresh


def main() -> None:
    input_path = Path(__file__).with_name("input.txt")
    text = input_path.read_text()
    t0 = time.perf_counter()
    p1, p2 = solve(text)
    elapsed_ms = (time.perf_counter() - t0) * 1000
    timestamp = datetime.now().isoformat(timespec="seconds")
    print(f"[{timestamp}] available_fresh={p1} total_fresh_ids={p2} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
