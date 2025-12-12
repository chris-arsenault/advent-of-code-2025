import { readFileSync } from "fs";

function generateEvenHalf(maxN: bigint): bigint[] {
  const vals: bigint[] = [];
  const maxLen = maxN.toString().length;
  for (let halfLen = 1; halfLen <= Math.floor(maxLen / 2); halfLen++) {
    const start = BigInt(10) ** BigInt(halfLen - 1);
    const end = BigInt(10) ** BigInt(halfLen);
    for (let t = start; t < end; t++) {
      const n = t * BigInt(10) ** BigInt(halfLen) + t;
      if (n > maxN) break;
      vals.push(n);
    }
  }
  vals.sort((a, b) => (a < b ? -1 : a > b ? 1 : 0));
  return vals;
}

function generatePeriodic(maxN: bigint): bigint[] {
  const seen = new Set<bigint>();
  const maxLen = maxN.toString().length;
  for (let baseLen = 1; baseLen <= Math.floor((maxLen + 1) / 2); baseLen++) {
    const start = BigInt(10) ** BigInt(baseLen - 1);
    const end = BigInt(10) ** BigInt(baseLen);
    for (let t = start; t < end; t++) {
      const baseStr = t.toString();
      for (let rep = 2; baseLen * rep <= maxLen; rep++) {
        const nStr = baseStr.repeat(rep);
        const n = BigInt(nStr);
        if (n > maxN) break;
        seen.add(n);
      }
    }
  }
  return Array.from(seen).sort((a, b) => (a < b ? -1 : a > b ? 1 : 0));
}

function prefix(nums: bigint[]): bigint[] {
  const ps: bigint[] = [0n];
  for (const n of nums) {
    ps.push(ps[ps.length - 1] + n);
  }
  return ps;
}

function rangeSum(nums: bigint[], ps: bigint[], lo: bigint, hi: bigint): bigint {
  let i = nums.findIndex((v) => v >= lo);
  if (i === -1) i = nums.length;
  let j = nums.findIndex((v) => v > hi);
  if (j === -1) j = nums.length;
  return ps[j] - ps[i];
}

function parseRanges(text: string): Array<[bigint, bigint]> {
  const ranges: Array<[bigint, bigint]> = [];
  for (const part of text.replace(/\n/g, ",").split(",")) {
    const p = part.trim();
    if (!p) continue;
    const [a, b] = p.split("-");
    ranges.push([BigInt(a), BigInt(b)]);
  }
  return ranges;
}

function solve(text: string): [bigint, bigint] {
  const ranges = parseRanges(text);
  const maxN = ranges.reduce((m, [, b]) => (b > m ? b : m), 0n);
  const evens = generateEvenHalf(maxN);
  const evPS = prefix(evens);
  const per = generatePeriodic(maxN);
  const perPS = prefix(per);
  let p1 = 0n;
  let p2 = 0n;
  for (const [a, b] of ranges) {
    p1 += rangeSum(evens, evPS, a, b);
    p2 += rangeSum(per, perPS, a, b);
  }
  return [p1, p2];
}

const text = readFileSync("input.txt", "utf8");
const t0 = performance.now();
const [p1, p2] = solve(text);
const elapsed = performance.now() - t0;
console.log(
  `repeated-halves-sum=${p1} repeated-pattern-sum=${p2} elapsed_ms=${elapsed.toFixed(
    3,
  )}`,
);
