from __future__ import annotations

from pathlib import Path
import time


def load_points(lines: list[str]) -> list[tuple[int, int, int]]:
    pts: list[tuple[int, int, int]] = []
    for line in lines:
        if not line.strip():
            continue
        x, y, z = map(int, line.split(","))
        pts.append((x, y, z))
    return pts


class DSU:
    def __init__(self, n: int) -> None:
        self.parent = list(range(n))
        self.size = [1] * n
        self.components = n

    def find(self, x: int) -> int:
        while self.parent[x] != x:
            self.parent[x] = self.parent[self.parent[x]]
            x = self.parent[x]
        return x

    def union(self, a: int, b: int) -> bool:
        ra, rb = self.find(a), self.find(b)
        if ra == rb:
            return False
        if self.size[ra] < self.size[rb]:
            ra, rb = rb, ra
        self.parent[rb] = ra
        self.size[ra] += self.size[rb]
        self.components -= 1
        return True


def build_edges(points: list[tuple[int, int, int]]) -> list[tuple[int, int, int]]:
    edges: list[tuple[int, int, int]] = []
    n = len(points)
    for i in range(n):
        xi, yi, zi = points[i]
        for j in range(i + 1, n):
            xj, yj, zj = points[j]
            dx = xi - xj
            dy = yi - yj
            dz = zi - zj
            dist2 = dx * dx + dy * dy + dz * dz
            edges.append((dist2, i, j))
    edges.sort(key=lambda t: t[0])
    return edges


def part1(n: int, edges: list[tuple[int, int, int]], k: int = 1000) -> int:
    dsu = DSU(n)
    for _, a, b in edges[:k]:
        dsu.union(a, b)
    sizes = [dsu.size[i] for i in range(n) if dsu.find(i) == i]
    sizes.sort(reverse=True)
    while len(sizes) < 3:
        sizes.append(1)
    return sizes[0] * sizes[1] * sizes[2]


def part2(points: list[tuple[int, int, int]], edges: list[tuple[int, int, int]]) -> int:
    n = len(points)
    dsu = DSU(n)
    last_prod = 0
    for _, a, b in edges:
        if dsu.union(a, b):
            last_prod = points[a][0] * points[b][0]
            if dsu.components == 1:
                break
    return last_prod


def main() -> None:
    lines = Path(__file__).with_name("input.txt").read_text().splitlines()
    points = load_points(lines)
    t0 = time.perf_counter()
    edges = build_edges(points)
    p1 = part1(len(points), edges, k=1000)
    p2 = part2(points, edges)
    elapsed_ms = (time.perf_counter() - t0) * 1000
    print(f"top3_product={p1} final_join_x_product={p2} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
