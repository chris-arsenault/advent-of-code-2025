#!/usr/bin/env python3
from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

DAYS = [str(i) for i in range(1, 13)]
ROOT = Path(__file__).parent


def run_cmd(cmd, cwd):
    result = subprocess.run(
        cmd, shell=True, cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
    )
    return result.returncode, result.stdout.strip(), result.stderr.strip()


def extract_elapsed(text: str) -> float | None:
    for token in text.replace("\n", " ").split():
        if token.startswith("elapsed_ms="):
            try:
                return float(token.split("=", 1)[1])
            except ValueError:
                continue
    return None


def ensure_built(day_dir: Path, day: str) -> None:
    main_c = day_dir / "main.c"
    if not main_c.exists():
        return
    exe = day_dir / f"day{day}"
    cmd = f"cc -O2 -std=c11 -D_POSIX_C_SOURCE=200809L -o {exe} {main_c} -lm"
    subprocess.run(cmd, shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)


def install_requirements(day_dir: Path) -> None:
    req = day_dir / "requirements.txt"
    if req.exists() and req.read_text().strip():
        subprocess.run(f"pip install -q -r {req}", shell=True, cwd=day_dir, check=True)


def main() -> None:
    summary = []
    for day in DAYS:
        day_dir = ROOT / f"day{day}"
        print(f"== Day {day} ==")
        c_elapsed = None
        py_elapsed = None

        try:
            ensure_built(day_dir, day)
        except subprocess.CalledProcessError as e:
            print(f"[build error] {e.stderr.strip()}")

        exe = day_dir / f"day{day}"
        if exe.exists():
            code, out, err = run_cmd(f"./day{day}", day_dir)
            if out:
                print(out)
            if err:
                print(err, file=sys.stderr)
            c_elapsed = extract_elapsed(out)
        else:
            print("C binary not found for this day")

        try:
            install_requirements(day_dir)
        except subprocess.CalledProcessError as e:
            print(f"[pip error] {e.stderr.strip()}")

        code, out, err = run_cmd("python main.py", day_dir)
        if out:
            print(out)
        if err:
            print(err, file=sys.stderr)
        py_elapsed = extract_elapsed(out)

        delta = None
        if c_elapsed is not None and py_elapsed is not None:
            delta = py_elapsed - c_elapsed
            print(f"delta_ms (python - c): {delta}")
        else:
            print("delta_ms: N/A")

        summary.append((day, c_elapsed, py_elapsed, delta))
        print()

    print("== Summary (ms) ==")
    print(f"{'Day':>4}  {'C':>12}  {'Python':>12}  {'Delta':>12}")
    for day, c_e, p_e, d in summary:
        def fmt(x):
            return f"{x:.3f}" if x is not None else "N/A"
        print(f"{day:>4}  {fmt(c_e):>12}  {fmt(p_e):>12}  {fmt(d):>12}")


if __name__ == "__main__":
    main()
