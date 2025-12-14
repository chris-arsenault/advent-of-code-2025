#!/usr/bin/env python3
from __future__ import annotations

import argparse
import statistics as st
import subprocess
import sys
import time
from typing import List


def run_once(cmd: List[str]) -> float:
    t0 = time.perf_counter_ns()
    subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    return time.perf_counter_ns() - t0


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Nanosecond-level microbenchmark wrapper. Runs command multiple times and reports stats."
    )
    parser.add_argument("cmd", nargs=argparse.REMAINDER, help="Command to run (prefix with -- to separate).")
    parser.add_argument("--runs", type=int, default=10, help="Number of measured runs (default: 10).")
    parser.add_argument("--warmup", type=int, default=2, help="Warmup runs (default: 2).")
    args = parser.parse_args()

    if not args.cmd:
        parser.error("Please provide a command to benchmark.")

    cmd = args.cmd
    if cmd and cmd[0] == "--":
        cmd = cmd[1:]

    for _ in range(max(args.warmup, 0)):
        subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    samples_ns = []
    for _ in range(max(args.runs, 1)):
        samples_ns.append(run_once(cmd))

    median_ns = st.median(samples_ns)
    min_ns = min(samples_ns)
    max_ns = max(samples_ns)
    mean_ns = st.mean(samples_ns)

    print(f"runs={len(samples_ns)} warmup={args.warmup}")
    print(f"median_ns={median_ns:.0f} mean_ns={mean_ns:.0f} min_ns={min_ns:.0f} max_ns={max_ns:.0f}")
    print("samples_ns=" + ",".join(f"{int(x)}" for x in samples_ns))


if __name__ == "__main__":
    main()
