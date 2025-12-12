#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

from run_all import do_installs, DAYS, ROOT


def main() -> None:
    ok = do_installs()
    if ok:
        print("Install/build completed successfully.")
    else:
        print("Install/build encountered errors; see above.")


if __name__ == "__main__":
    main()
