from __future__ import annotations

from pathlib import Path
import time

from shapely.geometry import Polygon, box


def load_points(lines: list[str]) -> list[tuple[int, int]]:
    pts: list[tuple[int, int]] = []
    for line in lines:
        if not line.strip():
            continue
        x_str, y_str = line.strip().split(",")
        pts.append((int(x_str), int(y_str)))
    return pts


def max_rectangle_any(pts: list[tuple[int, int]]) -> int:
    best = 0
    n = len(pts)
    for i in range(n):
        x1, y1 = pts[i]
        for j in range(i + 1, n):
            x2, y2 = pts[j]
            if x1 == x2 or y1 == y2:
                continue
            area = abs(x1 - x2) * abs(y1 - y2)
            if area > best:
                best = area
    return best


def max_rectangle_inside(pts: list[tuple[int, int]], region: Polygon) -> int:
    best = 0
    n = len(pts)
    for i in range(n):
        x1, y1 = pts[i]
        for j in range(i + 1, n):
            x2, y2 = pts[j]
            if x1 == x2 or y1 == y2:
                continue
            rect = box(min(x1, x2), min(y1, y2), max(x1, x2), max(y1, y2))
            if region.covers(rect):
                area = rect.area
                if area > best:
                    best = int(area)
    return best


def build_polygon(pts: list[tuple[int, int]]) -> Polygon:
    return Polygon(pts)


def main() -> None:
    lines = Path(__file__).with_name("input.txt").read_text().splitlines()
    t0 = time.perf_counter()
    pts = load_points(lines)
    poly = build_polygon(pts)
    p1 = max_rectangle_any(pts)
    p2 = max_rectangle_inside(pts, poly)
    elapsed_ms = (time.perf_counter() - t0) * 1000
    print(f"max_rect_area={p1} max_green_rect_area={p2} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
