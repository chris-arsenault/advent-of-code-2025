from __future__ import annotations

from datetime import datetime
from pathlib import Path
from typing import Iterable
import time


def load_grid(text: str) -> list[str]:
    rows = [line.rstrip("\n") for line in text.splitlines() if line.strip("\n")]
    width = max(len(r) for r in rows)
    return [r.ljust(width) for r in rows]


def split_blocks(grid: list[str]) -> list[tuple[int, int]]:
    height = len(grid)
    width = len(grid[0])
    empty_col = [all(grid[r][c] == " " for r in range(height)) for c in range(width)]
    blocks: list[tuple[int, int]] = []
    c = 0
    while c < width:
        while c < width and empty_col[c]:
            c += 1
        if c >= width:
            break
        start = c
        while c < width and not empty_col[c]:
            c += 1
        blocks.append((start, c))
    return blocks


def problem_operator(op_row: str) -> str:
    for ch in op_row:
        if ch in "+*":
            return ch
    raise ValueError("no operator found")


def eval_numbers(nums: Iterable[int], op: str) -> int:
    if op == "+":
        total = 0
        for n in nums:
            total += n
        return total
    total = 1
    for n in nums:
        total *= n
    return total


def part1(grid: list[str], blocks: list[tuple[int, int]]) -> int:
    total = 0
    op_row = grid[-1]
    for start, end in blocks:
        op = problem_operator(op_row[start:end])
        nums: list[int] = []
        for row in grid[:-1]:
            token = row[start:end].strip()
            if token:
                nums.append(int(token))
        total += eval_numbers(nums, op)
    return total


def part2(grid: list[str], blocks: list[tuple[int, int]]) -> int:
    total = 0
    height = len(grid) - 1  # exclude operator row for digits
    op_row = grid[-1]
    for start, end in blocks:
        op = problem_operator(op_row[start:end])
        nums: list[int] = []
        for c in range(end - 1, start - 1, -1):
            digits = []
            for r in range(height):
                ch = grid[r][c]
                if ch.isdigit():
                    digits.append(ch)
            if digits:
                nums.append(int("".join(digits)))
        total += eval_numbers(nums, op)
    return total


def main() -> None:
    text = Path(__file__).with_name("input.txt").read_text()
    grid = load_grid(text)
    blocks = split_blocks(grid)
    t0 = time.perf_counter()
    p1 = part1(grid, blocks)
    p2 = part2(grid, blocks)
    elapsed_ms = (time.perf_counter() - t0) * 1000
    timestamp = datetime.now().isoformat(timespec="seconds")
    print(f"[{timestamp}] grand_total={p1} quantum_total={p2} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
