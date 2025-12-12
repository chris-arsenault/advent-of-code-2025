import { readFileSync } from "fs";

type Grid = { rows: string[]; sr: number; sc: number };

function loadGrid(path: string): Grid {
  const rows = readFileSync(path, "utf8").trim().split(/\r?\n/);
  let sr = -1;
  let sc = -1;
  rows.forEach((row, r) => {
    const idx = row.indexOf("S");
    if (idx >= 0) {
      sr = r;
      sc = idx;
    }
  });
  return { rows, sr, sc };
}

function part1(g: Grid): number {
  const h = g.rows.length;
  const w = g.rows[0].length;
  let active = new Set<number>([g.sc]);
  let splits = 0;
  for (let r = g.sr; r < h; r++) {
    const next = new Set<number>();
    const queue: number[] = Array.from(active);
    const seen = new Set<number>();
    while (queue.length) {
      const c = queue.pop()!;
      if (seen.has(c)) continue;
      seen.add(c);
      const cell = g.rows[r][c];
      if (cell === "^") {
        splits++;
        if (c > 0) queue.push(c - 1);
        if (c + 1 < w) queue.push(c + 1);
      } else {
        next.add(c);
      }
    }
    active = next;
    if (!active.size) break;
  }
  return splits;
}

function part2(g: Grid): bigint {
  const h = g.rows.length;
  const w = g.rows[0].length;
  let active = new Map<number, bigint>([[g.sc, 1n]]);
  for (let r = g.sr; r < h; r++) {
    const next = new Map<number, bigint>();
    for (const [c, cnt] of active.entries()) {
      const cell = g.rows[r][c];
      if (cell === "^") {
        if (c > 0) next.set(c - 1, (next.get(c - 1) || 0n) + cnt);
        if (c + 1 < w) next.set(c + 1, (next.get(c + 1) || 0n) + cnt);
      } else {
        next.set(c, (next.get(c) || 0n) + cnt);
      }
    }
    active = next;
    if (!active.size) break;
  }
  let total = 0n;
  for (const v of active.values()) total += v;
  return total;
}

const g = loadGrid("input.txt");
const t0 = performance.now();
const p1 = part1(g);
const p2 = part2(g);
const elapsed = performance.now() - t0;
console.log(
  `splits=${p1} timelines=${p2} elapsed_ms=${elapsed.toFixed(3)}`,
);
