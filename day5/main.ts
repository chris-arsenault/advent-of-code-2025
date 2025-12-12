import { readFileSync } from "fs";

type Range = [bigint, bigint];

function mergeRanges(ranges: Range[]): Range[] {
  ranges.sort((a, b) =>
    a[0] === b[0] ? (a[1] < b[1] ? -1 : 1) : a[0] < b[0] ? -1 : 1,
  );
  const merged: Range[] = [];
  for (const r of ranges) {
    if (merged.length === 0 || r[0] > merged[merged.length - 1][1] + 1n) {
      merged.push([r[0], r[1]]);
    } else if (r[1] > merged[merged.length - 1][1]) {
      merged[merged.length - 1][1] = r[1];
    }
  }
  return merged;
}

function inAny(ranges: Range[], x: bigint): boolean {
  let lo = 0;
  let hi = ranges.length;
  while (lo < hi) {
    const mid = Math.floor((lo + hi) / 2);
    if (ranges[mid][0] <= x) {
      lo = mid + 1;
    } else {
      hi = mid;
    }
  }
  if (lo === 0) return false;
  const [a, b] = ranges[lo - 1];
  return a <= x && x <= b;
}

function parse(text: string): [Range[], bigint[]] {
  const norm = text.replace(/\r\n/g, "\n");
  const parts = norm.split(/\n\n/);
  const top = parts[0] ?? "";
  const bottom = parts[1] ?? "";
  const ranges: Range[] = [];
  for (const line of top.trim().split(/\r?\n/)) {
    if (!line.trim()) continue;
    const [a, b] = line.split("-").map((s) => BigInt(s));
    ranges.push([a, b]);
  }
  const ids = bottom
    .trim()
    .split(/\r?\n/)
    .filter(Boolean)
    .map((s) => BigInt(s.trim()));
  return [ranges, ids];
}

function solve(text: string): [bigint, bigint] {
  const [ranges, ids] = parse(text);
  const merged = mergeRanges(ranges);
  let fresh = 0n;
  for (const id of ids) {
    if (inAny(merged, id)) fresh++;
  }
  let total = 0n;
  for (const [a, b] of merged) {
    total += b - a + 1n;
  }
  return [fresh, total];
}

const text = readFileSync("input.txt", "utf8");
const t0 = performance.now();
const [p1, p2] = solve(text);
const elapsed = performance.now() - t0;
console.log(
  `available_fresh=${p1} total_fresh_ids=${p2} elapsed_ms=${elapsed.toFixed(
    3,
  )}`,
);
