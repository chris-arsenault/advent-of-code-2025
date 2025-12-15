import { readFileSync } from "fs";
import { performance } from "perf_hooks";

type Range = [bigint, bigint];

// Merge overlapping ranges using reduce (idiomatic TypeScript)
function mergeRanges(ranges: Range[]): Range[] {
  const sorted = [...ranges].sort((a, b) =>
    a[0] === b[0] ? (a[1] < b[1] ? -1 : 1) : a[0] < b[0] ? -1 : 1,
  );

  return sorted.reduce<Range[]>((merged, [a, b]) => {
    if (merged.length === 0 || a > merged[merged.length - 1][1] + 1n) {
      merged.push([a, b]);
    } else if (b > merged[merged.length - 1][1]) {
      merged[merged.length - 1][1] = b;
    }
    return merged;
  }, []);
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
  const ranges: Range[] = top
    .trim()
    .split(/\r?\n/)
    .filter((line) => line.trim())
    .map((line) => {
      const [a, b] = line.split("-").map((s) => BigInt(s));
      return [a, b] as Range;
    });
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

  // Part 1: Count IDs in any interval
  const fresh = ids.filter((id) => inAny(merged, id)).length;

  // Part 2: Total span using reduce
  const total = merged.reduce((sum, [a, b]) => sum + (b - a + 1n), 0n);

  return [BigInt(fresh), total];
}

const t0 = performance.now();
const text = readFileSync("input.txt", "utf8");
const [p1, p2] = solve(text);
const elapsed = performance.now() - t0;
console.log(
  `available_fresh=${p1} total_fresh_ids=${p2} elapsed_ms=${elapsed.toFixed(3)}`,
);
