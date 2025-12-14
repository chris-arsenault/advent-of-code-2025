import { readFileSync } from "fs";

type Point = { x: number; y: number };
type Shape = { forms: Point[][]; area: number };

function normalize(cells: Point[]): Point[] {
  const minx = Math.min(...cells.map((c) => c.x));
  const miny = Math.min(...cells.map((c) => c.y));
  return cells.map((c) => ({ x: c.x - minx, y: c.y - miny }));
}

function rotate(cells: Point[]): Point[] {
  const maxx = Math.max(...cells.map((c) => c.x));
  return cells.map((c) => ({ x: c.y, y: maxx - c.x }));
}

function flip(cells: Point[]): Point[] {
  const maxx = Math.max(...cells.map((c) => c.x));
  return cells.map((c) => ({ x: maxx - c.x, y: c.y }));
}

function orientations(cells: Point[]): Point[][] {
  const seen = new Set<string>();
  const out: Point[][] = [];
  let cur = cells;
  for (let r = 0; r < 4; r++) {
    for (const sh of [cur, flip(cur)]) {
      const norm = normalize(sh);
      const key = norm.map((c) => `${c.x},${c.y}`).join(";");
      if (!seen.has(key)) {
        seen.add(key);
        out.push(norm);
      }
    }
    cur = rotate(cur);
  }
  return out;
}

function parseInput(path: string): [Shape[], Array<[number, number]>, number[][]] {
  const lines = readFileSync(path, "utf8").split(/\r?\n/);
  const shapes: Shape[] = [];
  const regions: Array<[number, number]> = [];
  const counts: number[][] = [];
  let i = 0;
  // shapes
  while (i < lines.length) {
    let line = lines[i].trim();
    if (!line) {
      i++;
      continue;
    }
    if (line.endsWith(":")) {
      i++;
      const grid: string[] = [];
      while (i < lines.length && lines[i].trim() && !lines[i].trim().endsWith(":")) {
        grid.push(lines[i].trim());
        i++;
      }
      const cells: Point[] = [];
      grid.forEach((row, y) => {
        row.split("").forEach((ch, x) => {
          if (ch === "#") cells.push({ x, y });
        });
      });
      shapes.push({ forms: orientations(cells), area: cells.length });
    } else {
      break;
    }
  }
  // regions
  for (; i < lines.length; i++) {
    const line = lines[i].trim();
    if (!line || !line.includes("x")) continue;
    const [size, rest] = line.split(":");
    const [w, h] = size.split("x").map(Number);
    const cnts = rest
      .trim()
      .split(/\s+/)
      .filter(Boolean)
      .map((s) => parseInt(s, 10));
    regions.push([w, h]);
    counts.push(cnts);
  }
  return [shapes, regions, counts];
}

function canPackSmall(w: number, h: number, shapes: Shape[], counts: number[]): boolean {
  let totalPieces = 0;
  let neededArea = 0;
  shapes.forEach((sh, i) => {
    totalPieces += counts[i] || 0;
    neededArea += (counts[i] || 0) * sh.area;
  });
  if (neededArea > w * h) return false;
  if (totalPieces === 0) return true;

  const placements: bigint[][] = shapes.map(() => []);
  shapes.forEach((sh, si) => {
    for (const form of sh.forms) {
      const maxx = Math.max(...form.map((c) => c.x));
      const maxy = Math.max(...form.map((c) => c.y));
      for (let y = 0; y <= h - maxy - 1; y++) {
        for (let x = 0; x <= w - maxx - 1; x++) {
          let mask = 0n;
          for (const c of form) {
            const pos = BigInt((y + c.y) * w + (x + c.x));
            mask |= 1n << pos;
          }
          placements[si].push(mask);
        }
      }
    }
  });

  const order: number[] = [];
  counts.forEach((c, i) => {
    for (let k = 0; k < c; k++) order.push(i);
  });
  order.sort((a, b) => placements[a].length - placements[b].length);

  function dfs(idx: number, used: bigint): boolean {
    if (idx === order.length) return true;
    const si = order[idx];
    for (const pm of placements[si]) {
      if ((pm & used) === 0n) {
        if (dfs(idx + 1, used | pm)) return true;
      }
    }
    return false;
  }
  return dfs(0, 0n);
}

function solve(shapes: Shape[], regions: Array<[number, number]>, counts: number[][]): number {
  let good = 0;
  for (let i = 0; i < regions.length; i++) {
    const [w, h] = regions[i];
    let pieces = 0;
    let area = 0;
    shapes.forEach((sh, idx) => {
      const c = counts[i][idx] || 0;
      pieces += c;
      area += c * sh.area;
    });
    if (area > w * h) continue;
    if (w * h <= 400 && pieces <= 25) {
      if (canPackSmall(w, h, shapes, counts[i])) good++;
    } else {
      good++;
    }
  }
  return good;
}

const [shapes, regions, counts] = parseInput("input.txt");
const t0 = performance.now();
const ans = solve(shapes, regions, counts);
const elapsed = performance.now() - t0;
console.log(`regions_that_fit=${ans} elapsed_ms=${elapsed.toFixed(3)}`);
