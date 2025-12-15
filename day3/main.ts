import { readFileSync, existsSync } from "fs";
import { performance } from "perf_hooks";

function bestTwo(line: string): bigint {
  const d = line.trim();
  const n = d.length;
  if (n < 2) return 0n;
  const suffix: number[] = new Array(n + 1).fill(0);
  for (let i = n - 1; i >= 0; i--) {
    suffix[i] = Math.max(suffix[i + 1], d.charCodeAt(i));
  }
  let best = -1n;
  for (let i = 0; i < n - 1; i++) {
    const val =
      BigInt(d.charCodeAt(i) - 48) * 10n + BigInt(suffix[i + 1] - 48);
    if (val > best) best = val;
  }
  return best;
}

function bestK(line: string, k: number): bigint {
  const digits = line.trim().split("").map((c) => c.charCodeAt(0));
  let drop = digits.length - k;
  const stack: number[] = [];
  for (const d of digits) {
    while (drop > 0 && stack.length && stack[stack.length - 1] < d) {
      stack.pop();
      drop--;
    }
    stack.push(d);
  }
  const kept = stack.slice(0, k);
  return kept.reduce((acc, d) => acc * 10n + BigInt(d - 48), 0n);
}

const inputFile = existsSync("input_eric.txt") ? "input_eric.txt" : "input.txt";
const t0 = performance.now();
const lines = readFileSync(inputFile, "utf8").trim().split(/\r?\n/);
let p1 = 0n;
let p2 = 0n;
for (const line of lines) {
  p1 += bestTwo(line);
  p2 += bestK(line, 12);
}
const elapsed = performance.now() - t0;
console.log(
  `max-2-digit-sum=${p1} max-12-digit-sum=${p2} elapsed_ms=${elapsed.toFixed(
    3,
  )}`,
);
