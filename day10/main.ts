import { readFileSync } from "fs";

type Machine = { pattern: string; buttons: number[][]; targets: number[] };

function parseMachine(line: string): Machine {
  const pat = /\[(.*?)\]/.exec(line)![1];
  const buttonMatches = [...line.matchAll(/\(([^)]*)\)/g)];
  const buttons = buttonMatches.map((m) =>
    m[1].trim()
      ? m[1]
          .split(",")
          .map((s) => parseInt(s.trim(), 10))
      : [],
  );
  const targets = /\{([^}]*)\}/
    .exec(line)![1]
    .split(",")
    .map((s) => parseInt(s.trim(), 10));
  return { pattern: pat, buttons, targets };
}

function loadMachines(path: string): Machine[] {
  return readFileSync(path, "utf8")
    .trim()
    .split(/\r?\n/)
    .filter(Boolean)
    .map(parseMachine);
}

function part1(ms: Machine[]): number {
  let total = 0;
  for (const m of ms) {
    const lights = m.pattern.length;
    let target = 0;
    for (let i = 0; i < lights; i++) {
      if (m.pattern[i] === "#") target |= 1 << i;
    }
    const masks = m.buttons.map((b) => {
      let mask = 0;
      for (const idx of b) {
        if (idx < lights) mask ^= 1 << idx;
      }
      return mask;
    });
    const n = masks.length;
    let best = Number.POSITIVE_INFINITY;
    for (let mask = 0; mask < 1 << n; mask++) {
      let state = 0;
      let presses = 0;
      for (let i = 0; i < n; i++) {
        if ((mask >> i) & 1) {
          state ^= masks[i];
          presses++;
        }
      }
      if (state === target && presses < best) best = presses;
    }
    total += best;
  }
  return total;
}

function part2(ms: Machine[]): number {
  let total = 0;
  const EPS = 1e-9;

  for (const m of ms) {
    const counters = m.targets.length;
    const buttons = m.buttons.length;
    if (counters === 0 || buttons === 0) continue;

    // Matrix counters x buttons
    const mat: number[][] = Array.from({ length: counters }, () =>
      Array(buttons).fill(0),
    );
    m.buttons.forEach((b, cidx) => {
      for (const t of b) {
        if (t >= 0 && t < counters) mat[t][cidx] = 1;
      }
    });
    const rhs = m.targets.map((v) => v * 1.0);

    // RREF
    const pivotCols = Array(counters).fill(-1);
    let row = 0;
    for (let col = 0; col < buttons && row < counters; col++) {
      let pivot = -1;
      let bestVal = 0;
      for (let r = row; r < counters; r++) {
        const val = Math.abs(mat[r][col]);
        if (val > EPS && val > bestVal) {
          bestVal = val;
          pivot = r;
        }
      }
      if (pivot === -1) continue;
      if (pivot !== row) {
        [mat[row], mat[pivot]] = [mat[pivot], mat[row]];
        [rhs[row], rhs[pivot]] = [rhs[pivot], rhs[row]];
      }
      const piv = mat[row][col];
      for (let c = col; c < buttons; c++) mat[row][c] /= piv;
      rhs[row] /= piv;
      for (let r = 0; r < counters; r++) {
        if (r === row) continue;
        const f = mat[r][col];
        if (Math.abs(f) < EPS) continue;
        for (let c = col; c < buttons; c++) mat[r][c] -= f * mat[row][c];
        rhs[r] -= f * rhs[row];
      }
      pivotCols[row] = col;
      row++;
    }
    const rank = row;
    let inconsistent = false;
    for (let r = rank; r < counters; r++) {
      let maxv = 0;
      for (let c = 0; c < buttons; c++) maxv = Math.max(maxv, Math.abs(mat[r][c]));
      if (maxv < EPS && Math.abs(rhs[r]) > EPS) {
        inconsistent = true;
        break;
      }
    }
    if (inconsistent) continue;

    const used = Array(buttons).fill(false);
    for (let i = 0; i < rank; i++) if (pivotCols[i] >= 0) used[pivotCols[i]] = true;
    const freeCols = [];
    for (let c = 0; c < buttons; c++) if (!used[c]) freeCols.push(c);
    const freeCount = freeCols.length;

    const coef: number[][] = Array.from({ length: rank }, () =>
      Array(freeCount).fill(0),
    );
    for (let r = 0; r < rank; r++) {
      for (let f = 0; f < freeCount; f++) {
        coef[r][f] = mat[r][freeCols[f]];
      }
    }

    const sumTargets = m.targets.reduce((a, b) => a + b, 0);
    let best = sumTargets;
    const freeVals = Array(freeCount).fill(0);

    const evaluate = (cur: number) => {
      if (cur >= best) return;
      let sum = cur;
      for (let r = 0; r < rank; r++) {
        let v = rhs[r];
        for (let f = 0; f < freeCount; f++) v -= coef[r][f] * freeVals[f];
        if (v < -EPS) return;
        const iv = Math.round(v);
        if (Math.abs(iv - v) > EPS) return;
        sum += iv;
        if (sum >= best) return;
      }
      if (sum < best) best = sum;
    };

    evaluate(0);

    const quick = (idx: number, cur: number, cap: number) => {
      if (cur >= best) return;
      if (idx === freeCount) {
        evaluate(cur);
        return;
      }
      for (let v = 0; v <= cap; v++) {
        if (cur + v >= best) break;
        freeVals[idx] = v;
        quick(idx + 1, cur + v, cap);
      }
    };

    let seedCap = 400;
    if (best < seedCap) seedCap = best;
    if (freeCount > 0 && seedCap > 0) quick(0, 0, seedCap);

    const dfs = (idx: number, cur: number) => {
      if (cur >= best) return;
      if (idx === freeCount) {
        evaluate(cur);
        return;
      }
      const maxv = best - cur;
      for (let v = 0; v <= maxv; v++) {
        freeVals[idx] = v;
        dfs(idx + 1, cur + v);
      }
    };

    if (freeCount > 0) dfs(0, 0);
    else evaluate(0);

    total += best;
  }
  return total;
}

const machines = loadMachines("input.txt");
const t0 = performance.now();
const p1 = part1(machines);
const p2 = part2(machines);
const elapsed = performance.now() - t0;
console.log(
  `min_lights_presses=${p1} min_counter_presses=${p2} elapsed_ms=${elapsed.toFixed(
    3,
  )}`,
);
