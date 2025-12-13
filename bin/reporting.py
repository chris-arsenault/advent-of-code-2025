from __future__ import annotations

from typing import Any, Callable, Dict, List, Optional


def render_table(
    title: str,
    days: List[str],
    langs: List[str],
    values: Dict[str, List[Any]],
    ok_flags: Optional[Dict[str, List[bool]]],
    formatter: Callable[[Any, bool], str],
    colw: int = 12,
) -> None:
    print(f"== {title} ==")
    header = ["Day"] + [lang.upper() for lang in langs]
    print("  ".join(f"{h:>{colw}}" for h in header))
    for idx, day in enumerate(days):
        row_cells = [str(day)]
        for lang in langs:
            val_list = values.get(lang, [])
            val = val_list[idx] if idx < len(val_list) else None
            ok_list = ok_flags.get(lang, []) if ok_flags else None
            ok = ok_list[idx] if ok_list and idx < len(ok_list) else True
            row_cells.append(formatter(val, ok))
        print("  ".join(f"{cell:>{colw}}" for cell in row_cells))
    print()


def format_ms(val: Any, ok: bool) -> str:
    if not ok:
        return "ERR"
    if val is None:
        return "N/A"
    try:
        return f"{float(val):.3f}"
    except (TypeError, ValueError):
        return "N/A"


def format_mem(val: Any, ok: bool) -> str:
    if not ok:
        return "ERR"
    if val is None:
        return "N/A"
    try:
        return str(int(val))
    except (TypeError, ValueError):
        return "N/A"


def format_loc(val: Any, ok: bool) -> str:
    if val is None:
        return "N/A"
    try:
        return str(int(val))
    except (TypeError, ValueError):
        return "N/A"


def format_complexity(val: Any, ok: bool) -> str:
    if val is None:
        return "N/A"
    try:
        return str(int(val))
    except (TypeError, ValueError):
        return "N/A"

