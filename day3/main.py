from __future__ import annotations

from pathlib import Path
import time


def best_two_digits(s: str) -> int:
    digits = [int(ch) for ch in s.strip()]
    n = len(digits)
    if n < 2:
        return 0
    suffix_max = [0] * (n + 1)
    for i in range(n - 1, -1, -1):
        suffix_max[i] = max(suffix_max[i + 1], digits[i])

    best = -1
    for i, d in enumerate(digits[:-1]):
        candidate = 10 * d + suffix_max[i + 1]
        if candidate > best:
            best = candidate
    return best


def best_k_digits(s: str, k: int) -> int:
    digits = [int(ch) for ch in s.strip()]
    drop = len(digits) - k
    stack: list[int] = []
    for d in digits:
        while drop and stack and stack[-1] < d:
            stack.pop()
            drop -= 1
        stack.append(d)
    # if we didn't drop enough due to monotonicity, trim tail
    stack = stack[:k]
    return int("".join(str(d) for d in stack))


def solve(lines: list[str], k: int = 12) -> tuple[int, int]:
    p1 = 0
    p2 = 0
    for line in lines:
        line = line.strip()
        if not line:
            continue
        p1 += best_two_digits(line)
        p2 += best_k_digits(line, k)
    return p1, p2


def main() -> None:
    base = Path(__file__).parent
    input_path = base / "input_eric.txt"
    if not input_path.exists():
        input_path = base / "input.txt"
    lines = input_path.read_text().splitlines()
    t0 = time.perf_counter()
    p1, p2 = solve(lines, k=12)
    elapsed_ms = (time.perf_counter() - t0) * 1000
    print(f"max-2-digit-sum={p1} max-12-digit-sum={p2} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
