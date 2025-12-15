from __future__ import annotations

from pathlib import Path


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
            area = (abs(x1 - x2) + 1) * (abs(y1 - y2) + 1)
            if area > best:
                best = area
    return best


def point_on_edge(px: int, py: int, x1: int, y1: int, x2: int, y2: int) -> bool:
    if x1 == x2:
        return px == x1 and min(y1, y2) <= py <= max(y1, y2)
    elif y1 == y2:
        return py == y1 and min(x1, x2) <= px <= max(x1, x2)
    return False


def point_inside(px: int, py: int, poly: list[tuple[int, int]]) -> bool:
    n = len(poly)
    inside = False
    j = n - 1
    for i in range(n):
        x1, y1 = poly[j]
        x2, y2 = poly[i]
        if point_on_edge(px, py, x1, y1, x2, y2):
            return True
        if (y1 > py) != (y2 > py):
            x_intersect = (x2 - x1) * (py - y1) // (y2 - y1) + x1
            if px < x_intersect:
                inside = not inside
        j = i
    return inside


def edge_crosses_interior(xlo: int, xhi: int, ylo: int, yhi: int, x1: int, y1: int, x2: int, y2: int) -> bool:
    if x1 == x2:
        if x1 <= xlo or x1 >= xhi:
            return False
        ya, yb = min(y1, y2), max(y1, y2)
        if yb <= ylo or ya >= yhi:
            return False
        return ya < yhi and yb > ylo
    elif y1 == y2:
        if y1 <= ylo or y1 >= yhi:
            return False
        xa, xb = min(x1, x2), max(x1, x2)
        if xb <= xlo or xa >= xhi:
            return False
        return xa < xhi and xb > xlo
    return False


def rect_inside_polygon(xlo: int, xhi: int, ylo: int, yhi: int, poly: list[tuple[int, int]]) -> bool:
    if not point_inside(xlo, ylo, poly):
        return False
    if not point_inside(xlo, yhi, poly):
        return False
    if not point_inside(xhi, ylo, poly):
        return False
    if not point_inside(xhi, yhi, poly):
        return False

    n = len(poly)
    j = n - 1
    for i in range(n):
        x1, y1 = poly[j]
        x2, y2 = poly[i]
        if edge_crosses_interior(xlo, xhi, ylo, yhi, x1, y1, x2, y2):
            return False
        j = i
    return True


def max_rectangle_inside(pts: list[tuple[int, int]], poly: list[tuple[int, int]]) -> int:
    best = 0
    n = len(pts)
    for i in range(n):
        x1, y1 = pts[i]
        for j in range(i + 1, n):
            x2, y2 = pts[j]
            if x1 == x2 or y1 == y2:
                continue
            xlo, xhi = min(x1, x2), max(x1, x2)
            ylo, yhi = min(y1, y2), max(y1, y2)
            if rect_inside_polygon(xlo, xhi, ylo, yhi, poly):
                area = (xhi - xlo + 1) * (yhi - ylo + 1)
                if area > best:
                    best = area
    return best


def main() -> None:
    import time
    t0 = time.perf_counter()
    lines = Path(__file__).with_name("input.txt").read_text().splitlines()
    pts = load_points(lines)
    p1 = max_rectangle_any(pts)
    p2 = max_rectangle_inside(pts, pts)
    elapsed_ms = (time.perf_counter() - t0) * 1000
    print(f"max_rect_area={p1} max_green_rect_area={p2} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
