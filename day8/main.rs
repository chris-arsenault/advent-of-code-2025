use std::fs;
use std::time::Instant;

#[derive(Clone, Copy)]
struct Edge {
    dist: i64,
    a: usize,
    b: usize,
}

struct DSU {
    p: Vec<usize>,
    s: Vec<usize>,
}

impl DSU {
    fn new(n: usize) -> Self {
        DSU {
            p: (0..n).collect(),
            s: vec![1; n],
        }
    }
    fn find(&mut self, x: usize) -> usize {
        if self.p[x] != x {
            self.p[x] = self.find(self.p[x]);
        }
        self.p[x]
    }
    fn unite(&mut self, a: usize, b: usize) -> bool {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra == rb {
            return false;
        }
        let (mut ra, mut rb) = if self.s[ra] < self.s[rb] {
            (rb, ra)
        } else {
            (ra, rb)
        };
        self.p[rb] = ra;
        self.s[ra] += self.s[rb];
        true
    }
}

fn part1(n: usize, edges: &[Edge]) -> u64 {
    let mut dsu = DSU::new(n);
    let limit = edges.len().min(1000);
    for i in 0..limit {
        dsu.unite(edges[i].a, edges[i].b);
    }
    let mut comps = Vec::new();
    for i in 0..n {
        if dsu.find(i) == i {
            comps.push(dsu.s[i] as u64);
        }
    }
    comps.sort_by(|a, b| b.cmp(a));
    while comps.len() < 3 {
        comps.push(1);
    }
    comps[0] * comps[1] * comps[2]
}

fn part2(xs: &[i64], edges: &[Edge], n: usize) -> u64 {
    let mut dsu = DSU::new(n);
    let mut components = n;
    let mut last = 0u64;
    for e in edges {
        if dsu.unite(e.a, e.b) {
            components -= 1;
            last = (xs[e.a] as u64) * (xs[e.b] as u64);
            if components == 1 {
                break;
            }
        }
    }
    last
}

fn main() {
    let text = fs::read_to_string("input.txt").expect("input");
    let mut xs = Vec::new();
    let mut ys = Vec::new();
    let mut zs = Vec::new();
    for line in text.lines() {
        let parts: Vec<_> = line.split(',').collect();
        xs.push(parts[0].parse::<i64>().unwrap());
        ys.push(parts[1].parse::<i64>().unwrap());
        zs.push(parts[2].parse::<i64>().unwrap());
    }
    let n = xs.len();
    let mut edges = Vec::new();
    for i in 0..n {
        for j in i + 1..n {
            let dx = xs[i] - xs[j];
            let dy = ys[i] - ys[j];
            let dz = zs[i] - zs[j];
            let d2 = dx * dx + dy * dy + dz * dz;
            edges.push(Edge {
                dist: d2,
                a: i,
                b: j,
            });
        }
    }
    edges.sort_by_key(|e| e.dist);
    let start = Instant::now();
    let p1 = part1(n, &edges);
    let p2 = part2(&xs, &edges, n);
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    println!(
        "top3_product={} final_join_x_product={} elapsed_ms={:.3}",
        p1, p2, elapsed
    );
}
