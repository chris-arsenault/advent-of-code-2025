use petgraph::graph::UnGraph;
use petgraph::unionfind::UnionFind;
use std::fs;
use std::time::Instant;

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

    let mut graph: UnGraph<(), i64> = UnGraph::new_undirected();
    let nodes: Vec<_> = (0..n).map(|_| graph.add_node(())).collect();

    let mut edges: Vec<(i64, usize, usize)> = Vec::new();
    for i in 0..n {
        for j in i + 1..n {
            let dx = xs[i] - xs[j];
            let dy = ys[i] - ys[j];
            let dz = zs[i] - zs[j];
            let d2 = dx * dx + dy * dy + dz * dz;
            graph.add_edge(nodes[i], nodes[j], d2);
            edges.push((d2, i, j));
        }
    }
    edges.sort_by_key(|e| e.0);

    let start = Instant::now();

    // Part 1: Product of top 3 component sizes after first 1000 edges
    let mut uf1 = UnionFind::<usize>::new(n);
    let limit = edges.len().min(1000);
    for i in 0..limit {
        let (_, a, b) = edges[i];
        uf1.union(a, b);
    }
    let mut sizes: Vec<u64> = (0..n)
        .filter(|&i| uf1.find(i) == i)
        .map(|i| {
            (0..n).filter(|&j| uf1.find(j) == i).count() as u64
        })
        .collect();
    sizes.sort_by(|a, b| b.cmp(a));
    while sizes.len() < 3 {
        sizes.push(1);
    }
    let p1 = sizes[0] * sizes[1] * sizes[2];

    // Part 2: Product of x-coords of final MST edge
    let mut uf2 = UnionFind::<usize>::new(n);
    let mut components = n;
    let mut last = 0u64;
    for (_, a, b) in &edges {
        if uf2.union(*a, *b) {
            components -= 1;
            last = (xs[*a] as u64) * (xs[*b] as u64);
            if components == 1 {
                break;
            }
        }
    }
    let p2 = last;

    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    println!(
        "top3_product={} final_join_x_product={} elapsed_ms={:.3}",
        p1, p2, elapsed
    );
}
