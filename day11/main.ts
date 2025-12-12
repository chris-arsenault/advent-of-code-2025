import { readFileSync } from "fs";

type Graph = Map<string, string[]>;

function loadGraph(path: string): Graph {
  const g = new Map<string, string[]>();
  const lines = readFileSync(path, "utf8").trim().split(/\r?\n/);
  for (const line of lines) {
    const [from, rest] = line.split(":");
    const dests = rest.trim().split(/\s+/);
    g.set(from.trim(), dests);
  }
  return g;
}

function countPaths(
  g: Graph,
  start: string,
  target: string,
  memo: Map<string, bigint>,
): bigint {
  if (memo.has(start)) return memo.get(start)!;
  if (start === target) {
    memo.set(start, 1n);
    return 1n;
  }
  let total = 0n;
  for (const nxt of g.get(start) || []) {
    total += countPaths(g, nxt, target, memo);
  }
  memo.set(start, total);
  return total;
}

const g = loadGraph("input.txt");
const t0 = performance.now();
const p1 = countPaths(g, "you", "out", new Map());
const a1 = countPaths(g, "svr", "dac", new Map());
const a2 = countPaths(g, "dac", "fft", new Map());
const a3 = countPaths(g, "fft", "out", new Map());
const b1 = countPaths(g, "svr", "fft", new Map());
const b2 = countPaths(g, "fft", "dac", new Map());
const b3 = countPaths(g, "dac", "out", new Map());
const p2 = a1 * a2 * a3 + b1 * b2 * b3;
const elapsed = performance.now() - t0;
console.log(
  `paths_you_to_out=${p1} paths_svr_via_dac_fft=${p2} elapsed_ms=${elapsed.toFixed(
    3,
  )}`,
);
