from __future__ import annotations

import bisect
from pathlib import Path
import time


def generate_even_half(max_n: int) -> list[int]:
    vals: list[int] = []
    max_len = len(str(max_n))
    for half_len in range(1, max_len // 2 + 1):
        base = 10 ** (half_len - 1)
        limit = 10**half_len
        for t in range(base, limit):
            n = t * (10**half_len) + t
            if n > max_n:
                break
            vals.append(n)
    vals.sort()
    return vals


def generate_periodic(max_n: int) -> list[int]:
    vals: set[int] = set()
    max_len = len(str(max_n))
    for base_len in range(1, (max_len + 1) // 2 + 1):
        start = 10 ** (base_len - 1)
        end = 10**base_len
        for base in range(start, end):
            base_str = str(base)
            for repeats in range(2, max_len // base_len + 1):
                n_str = base_str * repeats
                if n_str[0] == "0":
                    continue
                n = int(n_str)
                if n > max_n:
                    break
                vals.add(n)
    vals_list = sorted(vals)
    return vals_list


def prefix_sums(nums: list[int]) -> list[int]:
    ps = [0]
    for n in nums:
        ps.append(ps[-1] + n)
    return ps


def range_sum(nums: list[int], pref: list[int], lo: int, hi: int) -> int:
    i = bisect.bisect_left(nums, lo)
    j = bisect.bisect_right(nums, hi)
    return pref[j] - pref[i]


def parse_ranges(text: str) -> list[tuple[int, int]]:
    ranges: list[tuple[int, int]] = []
    for part in text.replace("\n", ",").split(","):
        part = part.strip()
        if not part:
            continue
        a_str, b_str = part.split("-")
        ranges.append((int(a_str), int(b_str)))
    return ranges


def solve(text: str) -> tuple[int, int]:
    ranges = parse_ranges(text.strip())
    max_n = max(b for _, b in ranges)

    evens = generate_even_half(max_n)
    even_pref = prefix_sums(evens)

    periodic = generate_periodic(max_n)
    periodic_pref = prefix_sums(periodic)

    p1 = sum(range_sum(evens, even_pref, a, b) for a, b in ranges)
    p2 = sum(range_sum(periodic, periodic_pref, a, b) for a, b in ranges)
    return p1, p2


def main() -> None:
    input_path = Path(__file__).with_name("input.txt")
    text = input_path.read_text()
    t0 = time.perf_counter()
    p1, p2 = solve(text)
    elapsed_ms = (time.perf_counter() - t0) * 1000
    print(f"repeated-halves-sum={p1} repeated-pattern-sum={p2} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
