from __future__ import annotations

import re
import shutil
import subprocess
from pathlib import Path
from typing import Optional, Tuple


def extract_elapsed(text: str) -> Optional[float]:
    for token in text.replace("\n", " ").split():
        if token.startswith("elapsed_ms="):
            try:
                return float(token.split("=", 1)[1])
            except ValueError:
                continue
    return None


def extract_answers(text: str) -> str:
    """Extract answer key=value pairs, excluding elapsed_ms for comparison."""
    tokens = text.replace("\n", " ").split()
    answer_parts = [t for t in tokens if "=" in t and not t.startswith("elapsed_ms=")]
    return " ".join(sorted(answer_parts))


def extract_max_rss(stderr_text: str) -> Optional[int]:
    match = re.search(r"max_rss_kb=(\d+)", stderr_text)
    if match:
        try:
            return int(match.group(1))
        except ValueError:
            return None
    return None


def run_cmd(cmd: str, cwd: Path, measure_memory: bool) -> Tuple[int, str, str, Optional[int]]:
    use_time = measure_memory and shutil.which("/usr/bin/time") is not None
    timed_cmd = f"/usr/bin/time -f 'max_rss_kb=%M' {cmd}" if use_time else cmd
    result = subprocess.run(
        timed_cmd,
        shell=True,
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    stderr_text = result.stderr.strip()
    mem_kb = extract_max_rss(stderr_text) if use_time else None
    if use_time and stderr_text:
        stderr_lines = [line for line in stderr_text.splitlines() if not line.strip().startswith("max_rss_kb=")]
        stderr_text = "\n".join(stderr_lines).strip()
    return result.returncode, result.stdout.strip(), stderr_text, mem_kb


def count_loc(path: Path | None) -> Optional[int]:
    if not path or not path.exists():
        return None
    with path.open(encoding="utf-8", errors="ignore") as f:
        return sum(1 for line in f if line.strip())


def estimate_cyclomatic(path: Path | None) -> Optional[int]:
    if not path or not path.exists():
        return None
    text = path.read_text(encoding="utf-8", errors="ignore")
    keyword_hits = re.findall(r"\b(if|elif|else if|for|while|case|switch|when|match|except|catch|and|or)\b", text, flags=re.IGNORECASE)
    logic_ops = text.count("&&") + text.count("||")
    return 1 + len(keyword_hits) + logic_ops
