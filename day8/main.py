from __future__ import annotations

from pathlib import Path
import time

import networkx as nx


def load_points(lines: list[str]) -> list[tuple[int, int, int]]:
    pts: list[tuple[int, int, int]] = []
    for line in lines:
        if not line.strip():
            continue
        x, y, z = map(int, line.split(","))
        pts.append((x, y, z))
    return pts


def build_graph(points: list[tuple[int, int, int]]) -> nx.Graph:
    G = nx.Graph()
    n = len(points)
    for i in range(n):
        G.add_node(i)
    for i in range(n):
        xi, yi, zi = points[i]
        for j in range(i + 1, n):
            xj, yj, zj = points[j]
            dx, dy, dz = xi - xj, yi - yj, zi - zj
            dist2 = dx * dx + dy * dy + dz * dz
            G.add_edge(i, j, weight=dist2)
    return G


def part1(G: nx.Graph, k: int = 1000) -> int:
    edges = sorted(G.edges(data=True), key=lambda e: e[2]["weight"])
    H = nx.Graph()
    H.add_nodes_from(G.nodes())
    for u, v, _ in edges[:k]:
        H.add_edge(u, v)
    sizes = sorted([len(c) for c in nx.connected_components(H)], reverse=True)
    while len(sizes) < 3:
        sizes.append(1)
    return sizes[0] * sizes[1] * sizes[2]


def part2(points: list[tuple[int, int, int]], G: nx.Graph) -> int:
    mst = nx.minimum_spanning_tree(G, weight="weight")
    mst_edges = sorted(mst.edges(data=True), key=lambda e: e[2]["weight"])
    if not mst_edges:
        return 0
    a, b, _ = mst_edges[-1]
    return points[a][0] * points[b][0]


def main() -> None:
    lines = Path(__file__).with_name("input.txt").read_text().splitlines()
    points = load_points(lines)
    t0 = time.perf_counter()
    G = build_graph(points)
    p1 = part1(G, k=1000)
    p2 = part2(points, G)
    elapsed_ms = (time.perf_counter() - t0) * 1000
    print(f"top3_product={p1} final_join_x_product={p2} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
