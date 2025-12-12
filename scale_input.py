#!/usr/bin/env python3
import argparse
from pathlib import Path


def scale_grid(lines, factor):
    if factor <= 0:
        raise ValueError("factor must be positive")

    # Ensure all lines are same width
    stripped = [line.rstrip("\n") for line in lines]
    widths = {len(line) for line in stripped}
    if len(widths) != 1:
        raise ValueError(f"inconsistent line widths: {sorted(widths)}")

    scaled_lines = []
    for line in stripped:
        expanded = "".join(ch * factor for ch in line)
        for _ in range(factor):
            scaled_lines.append(expanded)
    return scaled_lines


def main():
    parser = argparse.ArgumentParser(
        description="Scale an input grid by an integer factor in both dimensions."
    )
    parser.add_argument("factor", type=int, help="scale factor (e.g., 2 doubles width and height)")
    parser.add_argument(
        "--input",
        "-i",
        type=Path,
        default=Path("input.txt"),
        help="path to source grid file (default: input.txt)",
    )
    parser.add_argument(
        "--output",
        "-o",
        type=Path,
        default=Path("scaled_input.txt"),
        help="path to write scaled grid (default: scaled_input.txt)",
    )
    args = parser.parse_args()

    data = args.input.read_text().splitlines()
    scaled = scale_grid(data, args.factor)
    args.output.write_text("\n".join(scaled) + "\n")


if __name__ == "__main__":
    main()
