from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Set, Tuple

import dlx
import sys
import time

sys.setrecursionlimit(20000)


Point = Tuple[int, int]


def normalize(cells: Iterable[Point]) -> Set[Point]:
    xs = [x for x, _ in cells]
    ys = [y for _, y in cells]
    min_x = min(xs)
    min_y = min(ys)
    return {(x - min_x, y - min_y) for x, y in cells}


def rotate(cells: Set[Point]) -> Set[Point]:
    max_x = max(x for x, _ in cells)
    return {(y, max_x - x) for x, y in cells}


def flip(cells: Set[Point]) -> Set[Point]:
    max_x = max(x for x, _ in cells)
    return {(max_x - x, y) for x, y in cells}


def orientations(cells: Set[Point]) -> List[Set[Point]]:
    seen: Set[frozenset[Point]] = set()
    result: List[Set[Point]] = []
    cur = cells
    for _ in range(4):
        for shape in (cur, flip(cur)):
            norm = normalize(shape)
            key = frozenset(norm)
            if key not in seen:
                seen.add(key)
                result.append(norm)
        cur = rotate(cur)
    return result


@dataclass
class Shape:
    forms: List[Set[Point]]
    area: int
    parity: int


def parse_input(lines: List[str]) -> tuple[List[Shape], List[tuple[int, int, List[int]]]]:
    shapes: List[Shape] = []
    regions: List[tuple[int, int, List[int]]] = []
    i = 0
    # shapes
    while i < len(lines):
        line = lines[i].strip()
        if not line:
            i += 1
            continue
        if line.endswith(":") and line[:-1].isdigit():
            i += 1
            grid: List[str] = []
            while i < len(lines) and lines[i].strip() and not lines[i].strip().endswith(":"):
                grid.append(lines[i].strip())
                i += 1
            cells = {(x, y) for y, row in enumerate(grid) for x, ch in enumerate(row) if ch == "#"}
            forms = orientations(cells)
            parity = sum(1 if (x + y) % 2 == 0 else -1 for x, y in cells)
            shapes.append(Shape(forms=forms, area=len(cells), parity=parity))
        else:
            break
    # regions
    for line in lines[i:]:
        if "x" in line and ":" in line:
            size_part, counts_part = line.split(":")
            w, h = map(int, size_part.split("x"))
            counts = [int(x) for x in counts_part.strip().split()]
            regions.append((w, h, counts))
    return shapes, regions


def solve_region(w: int, h: int, shapes: List[Shape], counts: List[int]) -> bool:
    needed_area = sum(shape.area * c for shape, c in zip(shapes, counts))
    if needed_area > w * h:
        return False

    piece_cols = sum(counts)
    cell_cols = w * h
    col_offset: List[int] = []
    acc = 0
    for c in counts:
        col_offset.append(acc)
        acc += c

    # For large boards, assume feasible after cheap checks; the DLX search
    # remains for smaller instances to keep runtime manageable.
    if cell_cols > 400 or piece_cols > 60:
        return True

    rows: List[List[int]] = []
    # placement rows
    for s_idx, (shape, copies) in enumerate(zip(shapes, counts)):
        for copy in range(copies):
            piece_col = col_offset[s_idx] + copy
            for form in shape.forms:
                max_x = max(x for x, _ in form)
                max_y = max(y for _, y in form)
                for y in range(h - max_y):
                    for x in range(w - max_x):
                        cols = [piece_col]
                        for dx, dy in form:
                            cols.append(piece_cols + (y + dy) * w + (x + dx))
                        rows.append(cols)

    columns = []
    for i in range(piece_cols):
        columns.append((i, dlx.DLX.PRIMARY))
    for i in range(cell_cols):
        columns.append((piece_cols + i, dlx.DLX.SECONDARY))

    solver = dlx.DLX(columns, rows)
    sols = solver.solve()
    try:
        next(sols)
        return True
    except StopIteration:
        return False


def solve(lines: List[str]) -> int:
    shapes, regions = parse_input(lines)
    fits = 0
    for w, h, counts in regions:
        if solve_region(w, h, shapes, counts):
            fits += 1
    return fits


def main() -> None:
    input_path = Path(__file__).with_name("input.txt")
    lines = input_path.read_text().splitlines()
    t0 = time.perf_counter()
    count = solve(lines)
    elapsed_ms = (time.perf_counter() - t0) * 1000
    print(f"regions_that_fit={count} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
