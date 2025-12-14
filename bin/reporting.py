from __future__ import annotations

from typing import Any, Callable, Dict, List, Optional


def render_table(
    title: str,
    days: List[str],
    langs: List[str],
    values: Dict[str, List[Any]],
    ok_flags: Optional[Dict[str, List[bool]]],
    formatter: Callable[[Any, bool], str],
    coerce_num: Callable[[Any], Optional[float]],
    colw: int = 12,
) -> None:
    print(f"== {title} ==")
    header = ["Day"] + [lang.upper() for lang in langs] + ["AVG"]
    print("  ".join(f"{h:>{colw}}" for h in header))

    for idx, day in enumerate(days):
        row_cells = [str(day)]
        day_numeric: List[float] = []
        for lang in langs:
            val_list = values.get(lang, [])
            val = val_list[idx] if idx < len(val_list) else None
            ok_list = ok_flags.get(lang, []) if ok_flags else None
            ok = ok_list[idx] if ok_list and idx < len(ok_list) else True
            if ok:
                num = coerce_num(val)
                if num is not None:
                    day_numeric.append(num)
            row_cells.append(formatter(val, ok))
        day_avg = sum(day_numeric) / len(day_numeric) if day_numeric else None
        row_cells.append(formatter(day_avg, True))
        print("  ".join(f"{cell:>{colw}}" for cell in row_cells))

    avg_cells = ["AVG"]
    overall_values: List[float] = []
    for lang in langs:
        lang_numeric: List[float] = []
        val_list = values.get(lang, [])
        ok_list = ok_flags.get(lang, []) if ok_flags else None
        for idx, _ in enumerate(days):
            val = val_list[idx] if idx < len(val_list) else None
            ok = ok_list[idx] if ok_list and idx < len(ok_list) else True
            if not ok:
                continue
            num = coerce_num(val)
            if num is not None:
                lang_numeric.append(num)
                overall_values.append(num)
        lang_avg = sum(lang_numeric) / len(lang_numeric) if lang_numeric else None
        avg_cells.append(formatter(lang_avg, True))
    overall_avg = sum(overall_values) / len(overall_values) if overall_values else None
    avg_cells.append(formatter(overall_avg, True))
    print("  ".join(f"{cell:>{colw}}" for cell in avg_cells))
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


def coerce_float(val: Any) -> Optional[float]:
    try:
        return float(val)
    except (TypeError, ValueError):
        return None


def coerce_int(val: Any) -> Optional[float]:
    try:
        return float(int(val))
    except (TypeError, ValueError):
        return None
