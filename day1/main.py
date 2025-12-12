from __future__ import annotations

from pathlib import Path
import time


def simulate(lines: list[str]) -> tuple[int, int, int]:
    """
    Walk the circular 0-99 track starting at 50.
    Return (zero_landings, zero_crossings, final_position).
    """
    pos = 50
    zero_hits = 0
    crossings = 0

    for raw in lines:
        line = raw.strip()
        if not line:
            continue
        sign = 1 if line[0] == "R" else -1
        mag = int(line[1:])

        first = 100 - pos if sign == 1 else pos
        if first == 0:
            first = 100
        if mag >= first:
            crossings += 1 + (mag - first) // 100

        pos = (pos + sign * mag) % 100
        if pos == 0:
            zero_hits += 1

    return zero_hits, crossings, pos


def main() -> None:
    input_path = Path(__file__).with_name("input.txt")
    lines = input_path.read_text().splitlines()
    t0 = time.perf_counter()
    zero_hits, crossings, pos = simulate(lines)
    elapsed_ms = (time.perf_counter() - t0) * 1000
    print(f"zero_landings={zero_hits} crossings={crossings} final_pos={pos} elapsed_ms={elapsed_ms:.3f}")


if __name__ == "__main__":
    main()
