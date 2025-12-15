from __future__ import annotations

from pathlib import Path

DAYS = [str(i) for i in range(1, 13)]
ROOT = Path(__file__).resolve().parent.parent

LANGS = {
    "c": {"exe": "./day{day}", "build": True, "source": "main.c"},
    "py": {"exe": "python main.py", "build": False, "source": "main.py"},
    "go": {"exe": "./day{day}_go", "build": True, "source": "main.go"},
    "rs": {"exe": "./day{day}_rs", "build": True, "source": "main.rs"},
    "ts": {"exe": "ts-node main.ts", "build": False, "source": "main.ts"},
    "rb": {"exe": "ruby main.rb", "build": False, "source": "main.rb"},
    "asm": {"exe": "./day{day}_asm", "build": True, "source": "main.asm"},
    "lisp": {"exe": "./day{day}_lisp", "build": True, "source": "main.lisp"},
    "jl": {"exe": "./day{day}_jl", "build": True, "source": "main.jl"},
    "hs": {"exe": "./day{day}_hs", "build": True, "source": "Main.hs"},
}

