from __future__ import annotations

import glob
import subprocess
from pathlib import Path

from .config import ROOT


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

    main_asm = day_dir / "main.asm"
    if main_asm.exists():
        asm_exe = day_dir / f"day{day}_asm"
        shared_obj = ROOT / "shared" / "utils.o"
        if not shared_obj.exists():
            shared_src = ROOT / "shared" / "utils.asm"
            if shared_src.exists():
                subprocess.run(
                    f"nasm -felf64 {shared_src} -o {shared_obj}",
                    shell=True,
                    cwd=ROOT,
                    check=True,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                )
        cmd = f"nasm -felf64 main.asm -o main.o && gcc -no-pie main.o {shared_obj} -o {asm_exe}"
        subprocess.run(cmd, shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    main_hs = day_dir / "Main.hs"
    if main_hs.exists():
        cabal_file = day_dir / f"day{day}.cabal"
        if cabal_file.exists():
            subprocess.run("cabal build", shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        else:
            exe = day_dir / f"day{day}_hs"
            cmd = f"ghc -O2 -o {exe} Main.hs"
            subprocess.run(cmd, shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)


def install_requirements(day_dir: Path) -> None:
    req = day_dir / "requirements.txt"
    if req.exists() and req.read_text().strip():
        subprocess.run(
            f"PIP_BREAK_SYSTEM_PACKAGES=1 pip install -q --break-system-packages -r {req}",
            shell=True,
            cwd=day_dir,
            check=True,
        )
    # Node.js packages
    pkg_json = day_dir / "package.json"
    if pkg_json.exists():
        subprocess.run("npm install --silent", shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    # Ruby gems
    gemfile = day_dir / "Gemfile"
    if gemfile.exists():
        subprocess.run("bundle install --quiet", shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)


def do_installs(days: list[str]) -> bool:
    ok = True
    for day in days:
        day_dir = ROOT / f"day{day}"
        try:
            ensure_built(day_dir, day)
        except subprocess.CalledProcessError as e:
            stderr = e.stderr.strip() if e.stderr else str(e)
            print(f"[build error day{day}] {stderr}")
            ok = False
        try:
            install_requirements(day_dir)
        except subprocess.CalledProcessError as e:
            stderr = e.stderr.strip() if e.stderr else str(e)
            print(f"[pip error day{day}] {stderr}")
            ok = False
    return ok
