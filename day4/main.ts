import { readFileSync } from "fs";

type Point = { r: number; c: number };

const dirs: Point[] = [
  { r: -1, c: -1 },
  { r: -1, c: 0 },
  { r: -1, c: 1 },
  { r: 0, c: -1 },
  { r: 0, c: 1 },
  { r: 1, c: -1 },
  { r: 1, c: 0 },
  { r: 1, c: 1 },
];

function key(p: Point): string {
  return `${p.r},${p.c}`;
}

function parseGrid(lines: string[]): Set<string> {
  const rolls = new Set<string>();
  lines.forEach((line, r) => {
    for (let c = 0; c < line.length; c++) {
      if (line[c] === "@") {
        rolls.add(key({ r, c }));
      }
    }
  });
  return rolls;
}

function neighborCounts(rolls: Set<string>): Map<string, number> {
  const counts = new Map<string, number>();
  rolls.forEach((k) => {
    const [r, c] = k.split(",").map(Number);
    let cnt = 0;
    for (const d of dirs) {
      if (rolls.has(key({ r: r + d.r, c: c + d.c }))) cnt++;
    }
    counts.set(k, cnt);
  });
  return counts;
}

function part1(counts: Map<string, number>): number {
  let acc = 0;
  counts.forEach((v) => {
    if (v < 4) acc++;
  });
  return acc;
}

function part2(rolls: Set<string>, counts: Map<string, number>): number {
  const queue: string[] = [];
  const inQ = new Set<string>();
  counts.forEach((v, k) => {
    if (v < 4) {
      queue.push(k);
      inQ.add(k);
    }
  });
  let removed = 0;
  while (queue.length) {
    const k = queue.shift()!;
    if (!rolls.has(k)) continue;
    rolls.delete(k);
    removed++;
    const [r, c] = k.split(",").map(Number);
    for (const d of dirs) {
      const nk = key({ r: r + d.r, c: c + d.c });
      if (rolls.has(nk)) {
        const newCnt = (counts.get(nk) || 0) - 1;
        counts.set(nk, newCnt);
        if (newCnt < 4 && !inQ.has(nk)) {
          queue.push(nk);
          inQ.add(nk);
        }
      }
    }
  }
  return removed;
}

const lines = readFileSync("input.txt", "utf8").trim().split(/\r?\n/);
const rolls = parseGrid(lines);
const counts = neighborCounts(rolls);
const t0 = performance.now();
const a = part1(counts);
const removed = part2(new Set(rolls), new Map(counts));
const elapsed = performance.now() - t0;
console.log(
  `accessible=${a} removable_total=${removed} elapsed_ms=${elapsed.toFixed(3)}`,
);
