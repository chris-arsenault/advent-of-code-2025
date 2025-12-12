#!/usr/bin/env python3
from __future__ import annotations

import subprocess
import sys
from pathlib import Path
import argparse

DAYS = [str(i) for i in range(1, 13)]
ROOT = Path(__file__).parent

LANGS = {
    "c": {"exe": "./day{day}", "build": True},
    "py": {"exe": "python main.py", "build": False},
    "go": {"exe": "go run main.go", "build": False},
    "rs": {"exe": "./day{day}_rs", "build": True},
    "ts": {"exe": "ts-node main.ts", "build": False},
    "rb": {"exe": "ruby main.rb", "build": False},
}


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
    if main_c.exists():
        exe = day_dir / f"day{day}"
        cmd = f"cc -O2 -std=c11 -D_POSIX_C_SOURCE=200809L -o {exe} {main_c} -lm"
        subprocess.run(cmd, shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if (day_dir / "Cargo.toml").exists():
        subprocess.run("cargo build --release", shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    else:
        main_rs = day_dir / "main.rs"
        if main_rs.exists():
            exe = day_dir / f"day{day}_rs"
            cmd = f"rustc -O -o {exe} main.rs"
            subprocess.run(cmd, shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    main_ts = day_dir / "main.ts"
    if main_ts.exists():
        subprocess.run("npm list ts-node >/dev/null 2>&1 || npm install -g ts-node typescript", shell=True)
    # go, rb: no build step


def install_requirements(day_dir: Path) -> None:
    req = day_dir / "requirements.txt"
    if req.exists() and req.read_text().strip():
        subprocess.run(f"pip install -q -r {req}", shell=True, cwd=day_dir, check=True)


def do_installs() -> bool:
    ok = True
    for day in DAYS:
        day_dir = ROOT / f"day{day}"
        try:
            ensure_built(day_dir, day)
        except subprocess.CalledProcessError as e:
            print(f"[build error day{day}] {e.stderr.strip()}")
            ok = False
        try:
            install_requirements(day_dir)
        except subprocess.CalledProcessError as e:
            print(f"[pip error day{day}] {e.stderr.strip()}")
            ok = False
    return ok


def main(do_install: bool) -> None:
    if do_install:
        if not do_installs():
            print("Aborting runs due to build/install errors.")
            return
    # Execute and collect stats
    summary = {lang: [] for lang in LANGS}
    any_fail = False
    for day in DAYS:
        day_dir = ROOT / f"day{day}"
        print(f"== Day {day} ==")
        day_results = {}
        for lang, cfg in LANGS.items():
            cmd = cfg["exe"].format(day=day)
            if lang == "rs":
                cargo_exe = day_dir / "target" / "release" / f"day{day}rs"
                if cargo_exe.exists():
                    cmd = str(cargo_exe)
            code, out, err = run_cmd(cmd, day_dir)
            if out:
                print(f"[{lang}] {out}")
            if err:
                print(err, file=sys.stderr)
            elapsed = extract_elapsed(out) if code == 0 else None
            ok = code == 0 and elapsed is not None
            if not ok:
                any_fail = True
            day_results[lang] = {"elapsed": elapsed, "ok": ok}
            summary[lang].append((day, elapsed, ok))

        base = day_results.get("c", {}).get("elapsed")
        if base is not None:
            for lang, res in day_results.items():
                if lang == "c":
                    continue
                if res["elapsed"] is not None:
                    print(f"delta_ms ({lang} - c): {res['elapsed'] - base:.3f}")
        print()

    print("== Summary (ms) ==")
    colw = 12
    header = ["Day"] + [lang.upper() for lang in LANGS]
    print("  ".join(f"{h:>{colw}}" for h in header))
    for idx, day in enumerate(DAYS):
        row = [f"{day:>{colw}}"]
        for lang in LANGS:
            val, ok = summary[lang][idx][1], summary[lang][idx][2]
            if ok and val is not None:
                row.append(f"{val:>{colw}.3f}")
            else:
                row.append(f"{'FAIL' if not ok else 'N/A':>{colw}}")
        print("  ".join(row))
    if any_fail:
        print("\nSome runs failed or missing elapsed_ms; see logs above.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run all solutions")
    parser.add_argument("--install", action="store_true", help="build/install dependencies before running")
    args = parser.parse_args()
    main(args.install)
