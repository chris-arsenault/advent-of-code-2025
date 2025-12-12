import { readFileSync } from "fs";

type Edge = { dist: number; a: number; b: number };

class DSU {
  parent: number[];
  size: number[];
  constructor(n: number) {
    this.parent = Array.from({ length: n }, (_, i) => i);
    this.size = Array(n).fill(1);
  }
  find(x: number): number {
    while (this.parent[x] !== x) {
      this.parent[x] = this.parent[this.parent[x]];
      x = this.parent[x];
    }
    return x;
  }
  unite(a: number, b: number): boolean {
    let ra = this.find(a);
    let rb = this.find(b);
    if (ra === rb) return false;
    if (this.size[ra] < this.size[rb]) {
      [ra, rb] = [rb, ra];
    }
    this.parent[rb] = ra;
    this.size[ra] += this.size[rb];
    return true;
  }
}

function part1(n: number, edges: Edge[]): bigint {
  const dsu = new DSU(n);
  const limit = Math.min(edges.length, 1000);
  for (let i = 0; i < limit; i++) {
    dsu.unite(edges[i].a, edges[i].b);
  }
  const comps: bigint[] = [];
  for (let i = 0; i < n; i++) {
    if (dsu.find(i) === i) comps.push(BigInt(dsu.size[i]));
  }
  comps.sort((a, b) => (a > b ? -1 : 1));
  while (comps.length < 3) comps.push(1n);
  return comps[0] * comps[1] * comps[2];
}

function part2(xs: number[], edges: Edge[], n: number): bigint {
  const dsu = new DSU(n);
  let components = n;
  let last = 0n;
  for (const e of edges) {
    if (dsu.unite(e.a, e.b)) {
      components--;
      last = BigInt(xs[e.a]) * BigInt(xs[e.b]);
      if (components === 1) break;
    }
  }
  return last;
}

const lines = readFileSync("input.txt", "utf8").trim().split(/\r?\n/);
const xs: number[] = [];
const ys: number[] = [];
const zs: number[] = [];
for (const line of lines) {
  const [x, y, z] = line.split(",").map((v) => parseInt(v, 10));
  xs.push(x);
  ys.push(y);
  zs.push(z);
}
const n = xs.length;
const edges: Edge[] = [];
for (let i = 0; i < n; i++) {
  for (let j = i + 1; j < n; j++) {
    const dx = xs[i] - xs[j];
    const dy = ys[i] - ys[j];
    const dz = zs[i] - zs[j];
    const d2 = dx * dx + dy * dy + dz * dz;
    edges.push({ dist: d2, a: i, b: j });
  }
}
edges.sort((a, b) => a.dist - b.dist);
const t0 = performance.now();
const p1 = part1(n, edges);
const p2 = part2(xs, edges, n);
const elapsed = performance.now() - t0;
console.log(
  `top3_product=${p1} final_join_x_product=${p2} elapsed_ms=${elapsed.toFixed(
    3,
  )}`,
);
