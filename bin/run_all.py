#!/usr/bin/env python3
from __future__ import annotations

import argparse
import glob
import sys
from pathlib import Path
from typing import Dict, List, Tuple

if __package__ is None or __package__ == "":
    sys.path.append(str(Path(__file__).resolve().parent.parent))
    from bin.build import do_installs
    from bin.config import DAYS, LANGS, ROOT
    from bin.metrics import count_loc, estimate_cyclomatic, extract_answers, extract_elapsed, run_cmd
    from bin.reporting import coerce_float, coerce_int, format_complexity, format_loc, format_mem, format_ms, render_table
else:
    from .build import do_installs
    from .config import DAYS, LANGS, ROOT
    from .metrics import count_loc, estimate_cyclomatic, extract_answers, extract_elapsed, run_cmd
    from .reporting import coerce_float, coerce_int, format_complexity, format_loc, format_mem, format_ms, render_table


def parse_days(day_arg: str | None) -> List[str]:
    if not day_arg:
        return DAYS
    days: List[str] = []
    for part in day_arg.split(","):
        token = part.strip()
        if not token:
            continue
        if not token.isdigit():
            raise argparse.ArgumentTypeError(f"Invalid day value: {token}")
        days.append(str(int(token)))
    return days or DAYS


def resolve_command(day: str, lang: str, cfg: dict, day_dir: Path) -> str:
    cmd = cfg["exe"].format(day=day)
    if lang == "rs":
        cargo_exe = day_dir / "target" / "release" / f"day{day}rs"
        if cargo_exe.exists():
            cmd = str(cargo_exe)
    if lang == "hs":
        cabal_exe = day_dir / "dist-newstyle" / "build" / "x86_64-linux"
        if cabal_exe.exists():
            pattern = str(
                day_dir
                / "dist-newstyle"
                / "build"
                / "*"
                / "ghc-*"
                / f"day{day}-*"
                / "x"
                / f"day{day}hs"
                / "build"
                / f"day{day}hs"
                / f"day{day}hs"
            )
            matches = glob.glob(pattern)
            if matches:
                cmd = matches[0]
    return cmd


def locate_source(day_dir: Path, source_name: str | None) -> Path | None:
    if not source_name:
        return None
    path = day_dir / source_name
    return path if path.exists() else None


def run_all(
    days: List[str],
    measure_memory: bool,
    include_loc: bool,
    include_complexity: bool,
    do_install: bool,
) -> Tuple[Dict[str, List[float | None]], Dict[str, List[int | None]], Dict[str, List[bool]], Dict[str, List[int | None]], Dict[str, List[int | None]], bool]:
    if do_install:
        if not do_installs(days):
            print("Aborting runs due to build/install errors.")
            return {}, {}, {}, {}, {}, True

    timings: Dict[str, List[float | None]] = {lang: [] for lang in LANGS}
    memory: Dict[str, List[int | None]] = {lang: [] for lang in LANGS}
    ok_flags: Dict[str, List[bool]] = {lang: [] for lang in LANGS}
    locs: Dict[str, List[int | None]] = {lang: [] for lang in LANGS}
    complexities: Dict[str, List[int | None]] = {lang: [] for lang in LANGS}
    any_fail = False

    for day in days:
        day_dir = ROOT / f"day{day}"
        print(f"== Day {day} ==")
        day_results: Dict[str, dict] = {}

        for lang, cfg in LANGS.items():
            source_path = locate_source(day_dir, cfg.get("source"))
            loc_val = count_loc(source_path) if include_loc else None
            locs[lang].append(loc_val)
            complexity_val = estimate_cyclomatic(source_path) if include_complexity else None
            complexities[lang].append(complexity_val)

            cmd = resolve_command(day, lang, cfg, day_dir)
            code, out, err, mem_kb, elapsed_ms = run_cmd(cmd, day_dir, measure_memory)

            if out:
                print(f"[{lang}] {out}")
            if err:
                print(err, file=sys.stderr)

            elapsed = elapsed_ms if elapsed_ms is not None else (extract_elapsed(out) if code == 0 else None)
            ok_exec = code == 0
            day_results[lang] = {"elapsed": elapsed, "ok": ok_exec, "out": out, "mem_kb": mem_kb if ok_exec else None}
            timings[lang].append(elapsed if ok_exec else None)
            memory[lang].append(mem_kb if ok_exec else None)
            ok_flags[lang].append(ok_exec)

        base_out = day_results.get("c", {}).get("out")
        base_answers = extract_answers(base_out) if base_out else None
        for lang, res in day_results.items():
            if lang == "c":
                continue
            if base_answers is None or not res["ok"]:
                res["ok"] = False
                res["elapsed"] = None
                res["mem_kb"] = None
                timings[lang][-1] = None
                memory[lang][-1] = None
                ok_flags[lang][-1] = False
                continue
            lang_answers = extract_answers(res["out"])
            if lang_answers != base_answers:
                print(f"[mismatch] day {day} {lang} output differs from C")
                print(f"  C:    {base_answers}")
                print(f"  {lang}: {lang_answers}")
                res["ok"] = False
                res["elapsed"] = None
                res["mem_kb"] = None
                timings[lang][-1] = None
                memory[lang][-1] = None
                ok_flags[lang][-1] = False

        if any(not res.get("ok", False) for res in day_results.values()):
            any_fail = True
        print()

    return timings, memory, ok_flags, locs, complexities, any_fail


def cli_main() -> None:
    parser = argparse.ArgumentParser(description="Run all solutions with optional stats.")
    parser.add_argument("--install", action="store_true", help="Build/install dependencies before running.")
    parser.add_argument("--day", "--days", dest="days", help="Comma-separated list of days to run (e.g. 1,4,6). Default: all.")
    parser.add_argument("--no-time", action="store_true", help="Skip timing summary table.")
    parser.add_argument("--no-memory", action="store_true", help="Skip memory summary table.")
    parser.add_argument("--no-loc", action="store_true", help="Skip line count summary table.")
    parser.add_argument("--no-complexity", action="store_true", help="Skip cyclomatic complexity summary table.")
    args = parser.parse_args()

    try:
        days = parse_days(args.days)
    except argparse.ArgumentTypeError as exc:
        parser.error(str(exc))

    timings, memory, ok_flags, locs, complexities, any_fail = run_all(
        days=days,
        measure_memory=not args.no_memory,
        include_loc=not args.no_loc,
        include_complexity=not args.no_complexity,
        do_install=args.install,
    )

    langs = list(LANGS)
    if not args.no_time:
        render_table("Summary (ms)", days, langs, timings, ok_flags, format_ms, coerce_float)
    if not args.no_memory:
        render_table("Memory (KiB)", days, langs, memory, ok_flags, format_mem, coerce_int)
    if not args.no_loc:
        render_table("Line Count", days, langs, locs, None, format_loc, coerce_int)
    if not args.no_complexity:
        render_table("Cyclomatic Complexity", days, langs, complexities, None, format_complexity, coerce_int)

    if any_fail:
        print("Some runs failed or missing elapsed_ms; see logs above.")


if __name__ == "__main__":
    cli_main()
