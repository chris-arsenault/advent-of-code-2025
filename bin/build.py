from __future__ import annotations

import glob
import subprocess
from pathlib import Path

from .config import ROOT


def ensure_built(day_dir: Path, day: str) -> None:
    # C compilation
    main_c = day_dir / "main.c"
    if main_c.exists():
        exe = day_dir / f"day{day}"
        cmd = f"cc -O2 -std=c11 -D_POSIX_C_SOURCE=200809L -o {exe} {main_c} -lm"
        subprocess.run(cmd, shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    # Rust compilation
    if (day_dir / "Cargo.toml").exists():
        subprocess.run("cargo build --release", shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    else:
        main_rs = day_dir / "main.rs"
        if main_rs.exists():
            exe = day_dir / f"day{day}_rs"
            cmd = f"rustc -O -o {exe} main.rs"
            subprocess.run(cmd, shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    # Go compilation (previously used "go run", now compile to binary)
    main_go = day_dir / "main.go"
    if main_go.exists():
        exe = day_dir / f"day{day}_go"
        cmd = f"go build -o {exe} main.go"
        subprocess.run(cmd, shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    # TypeScript setup
    main_ts = day_dir / "main.ts"
    if main_ts.exists():
        subprocess.run("npm list ts-node >/dev/null 2>&1 || npm install -g ts-node typescript", shell=True)

    # Assembly compilation
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

    # Haskell compilation
    main_hs = day_dir / "Main.hs"
    if main_hs.exists():
        cabal_file = day_dir / f"day{day}.cabal"
        if cabal_file.exists():
            subprocess.run("cabal build", shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        else:
            exe = day_dir / f"day{day}_hs"
            cmd = f"ghc -O2 -o {exe} Main.hs"
            subprocess.run(cmd, shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    # Common Lisp (SBCL) compilation to standalone executable
    main_lisp = day_dir / "main.lisp"
    if main_lisp.exists():
        exe = day_dir / f"day{day}_lisp"
        # Build approach: use --script to handle shebang, but wrap in a build script
        # that loads the source without running main, then saves executable
        build_script = f'''
;; Load the source file, skipping the shebang line
(with-open-file (in "main.lisp")
  (read-line in) ; skip shebang
  (loop for form = (read in nil :eof)
        until (eq form :eof)
        ;; Skip the (main) call at the end
        unless (and (consp form) (eq (car form) 'main))
        do (eval form)))
;; Save executable with main as toplevel function
(sb-ext:save-lisp-and-die "{exe}" :toplevel #'main :executable t :compression 9)
'''
        build_file = day_dir / "_build_lisp.lisp"
        build_file.write_text(build_script)
        try:
            subprocess.run(
                f'sbcl --noinform --non-interactive --load {build_file}',
                shell=True, cwd=day_dir, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE
            )
        finally:
            build_file.unlink(missing_ok=True)

    # Julia: create wrapper script with optimization flags
    # Note: Julia's JIT overhead cannot be fully eliminated without PackageCompiler,
    # which requires significant setup. This wrapper uses --compile=all for better
    # compilation but still has some startup overhead.
    main_jl = day_dir / "main.jl"
    if main_jl.exists():
        exe = day_dir / f"day{day}_jl"
        # Create wrapper script with optimized Julia flags:
        # --compile=all: compile all code to native, not just hot paths
        # -O3: maximum optimization level
        # --startup-file=no: skip loading ~/.julia/config/startup.jl
        wrapper_content = f'''#!/bin/bash
cd "$(dirname "$0")"
exec julia --compile=all -O3 --startup-file=no main.jl "$@"
'''
        exe.write_text(wrapper_content)
        exe.chmod(0o755)


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
