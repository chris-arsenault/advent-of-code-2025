# Advent of Code 2025 - The Polyglot Workshop

*A message from the North Pole Engineering Division*

---

Greetings, traveler!

The Elves have good news and bad news.

The good news is that you've discovered our secret workshop where we solve the daily puzzles that keep the North Pole running smoothly. As it turns out, different Elves prefer different tools for their work - some swear by the ancient scrolls of C, others prefer the elegant incantations of Haskell, and a few ambitious ones have even learned to speak directly to the machine spirits in Assembly.

The bad news? We need help documenting all of it before Christmas!

## What Is This Place?

This repository contains solutions to [Advent of Code 2025](https://adventofcode.com/2025) implemented in many programming languages. Each day's puzzle is solved multiple ways, showcasing how different languages approach the same problem.

**Languages in the Workshop:**
C | Python | Go | Rust | TypeScript | Ruby | Common Lisp | Julia | Haskell | x86-64 Assembly

## Quick Start

One of the younger Elves built a contraption that runs everything at once:

```bash
# Run all solutions (assumes dependencies installed)
./run_all.py

# Build everything first, then run
./run_all.py --install
```

The machine will compile what needs compiling, execute each solution, and compare outputs to ensure all the Elves got the same answers.

## Workshop Documents

The senior Elves have prepared several reference documents:

| Document | Purpose |
|----------|---------|
| [CLAUDE.md](CLAUDE.md) | Operating procedures for the workshop |
| [ALGORITHMS.md](ALGORITHMS.md) | How each puzzle is solved, with language-specific tips |
| [SHARED_ASM.md](SHARED_ASM.md) | Reference for the shared Assembly utilities |

## Workshop Layout

```
day1/ through day12/    The puzzle solutions, one folder per day
shared/                 Shared utilities (currently Assembly helpers)
run_all.py              The Elf-built test contraption
```

Each day folder contains:
- `problem.txt` - The puzzle description
- `input.txt` - The puzzle input
- Solutions in various languages (`main.py`, `main.c`, `Main.hs`, etc.)

## A Note from the Workshop Manager

These solutions prioritize clarity over defensive coding. The Elves assume the input is valid (it comes from a trusted source, after all) and focus on expressing the algorithm cleanly in each language's native style.

This is a teaching workshop, not a production facility. Please don't use these patterns for anything load-bearing - the reindeer have enough to worry about.

## License

MIT License - See [LICENSE](LICENSE)

The Elves believe in sharing knowledge freely. Use this code however you wish, though a mention of the North Pole Engineering Division is always appreciated.

---

*"Thank you for visiting the North Pole!"*
*- The Gift Shop Sign*
