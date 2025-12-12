import { readFileSync } from "fs";

function loadGrid(path: string): string[] {
  const lines = readFileSync(path, "utf8").split(/\r?\n/).filter((l) => l.length);
  const maxw = Math.max(...lines.map((l) => l.length));
  return lines.map((l) => l.padEnd(maxw, " "));
}

function splitBlocks(grid: string[]): Array<[number, number]> {
  const h = grid.length;
  const w = grid[0].length;
  const emptyCol: boolean[] = Array(w).fill(true);
  for (let c = 0; c < w; c++) {
    for (let r = 0; r < h; r++) {
      if (grid[r][c] !== " ") {
        emptyCol[c] = false;
        break;
      }
    }
  }
  const blocks: Array<[number, number]> = [];
  let c = 0;
  while (c < w) {
    while (c < w && emptyCol[c]) c++;
    if (c >= w) break;
    const start = c;
    while (c < w && !emptyCol[c]) c++;
    blocks.push([start, c]);
  }
  return blocks;
}

function problemOp(opRow: string): string {
  for (const ch of opRow) {
    if (ch === "+" || ch === "*") return ch;
  }
  return "+";
}

function evalNums(nums: bigint[], op: string): bigint {
  if (op === "+") return nums.reduce((a, b) => a + b, 0n);
  return nums.reduce((a, b) => a * b, 1n);
}

function part1(grid: string[], blocks: Array<[number, number]>): bigint {
  const opRow = grid[grid.length - 1];
  let total = 0n;
  for (const [s, e] of blocks) {
    const op = problemOp(opRow.slice(s, e));
    const nums: bigint[] = [];
    for (const row of grid.slice(0, -1)) {
      const tok = row.slice(s, e).trim();
      if (tok) nums.push(BigInt(tok));
    }
    total += evalNums(nums, op);
  }
  return total;
}

function part2(grid: string[], blocks: Array<[number, number]>): bigint {
  const h = grid.length - 1;
  const opRow = grid[grid.length - 1];
  let total = 0n;
  for (const [s, e] of blocks) {
    const op = problemOp(opRow.slice(s, e));
    const nums: bigint[] = [];
    for (let c = e - 1; c >= s; c--) {
      const digits: string[] = [];
      for (let r = 0; r < h; r++) {
        const ch = grid[r][c];
        if (/[0-9]/.test(ch)) digits.push(ch);
      }
      if (digits.length) nums.push(BigInt(digits.join("")));
    }
    total += evalNums(nums, op);
  }
  return total;
}

const grid = loadGrid("input.txt");
const blocks = splitBlocks(grid);
const t0 = performance.now();
const p1 = part1(grid, blocks);
const p2 = part2(grid, blocks);
const elapsed = performance.now() - t0;
console.log(
  `grand_total=${p1} quantum_total=${p2} elapsed_ms=${elapsed.toFixed(3)}`,
);
