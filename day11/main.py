from __future__ import annotations

import time
from functools import lru_cache
from pathlib import Path


def load_graph(lines: list[str]) -> dict[str, list[str]]:
    graph: dict[str, list[str]] = {}
    for line in lines:
        line = line.strip()
        if not line:
            continue
        left, right = line.split(":", 1)
        src = left.strip()
        dests = right.strip().split()
        graph[src] = dests
    return graph


def count_paths(graph: dict[str, list[str]], start: str, target: str) -> int:
    @lru_cache(maxsize=None)
    def dfs(node: str) -> int:
        if node == target:
            return 1
        total = 0
        for nxt in graph.get(node, []):
            total += dfs(nxt)
        return total

    return dfs(start)


def main() -> None:
    t0 = time.perf_counter()

    lines = Path(__file__).with_name("input.txt").read_text().splitlines()
    graph = load_graph(lines)
    p1 = count_paths(graph, "you", "out")

    a1 = count_paths(graph, "svr", "dac")
    a2 = count_paths(graph, "dac", "fft")
    a3 = count_paths(graph, "fft", "out")
    b1 = count_paths(graph, "svr", "fft")
    b2 = count_paths(graph, "fft", "dac")
    b3 = count_paths(graph, "dac", "out")
    p2 = a1 * a2 * a3 + b1 * b2 * b3

    elapsed_ms = (time.perf_counter() - t0) * 1000
    print(f"paths_you_to_out={p1} paths_svr_via_dac_fft={p2} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
