use std::collections::HashMap;
use std::fs;
use std::time::Instant;

fn load_graph(path: &str) -> HashMap<String, Vec<String>> {
    let mut g = HashMap::new();
    for line in fs::read_to_string(path).expect("input").lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let parts: Vec<_> = line.split(':').collect();
        let from = parts[0].trim().to_string();
        let dests = parts[1]
            .split_whitespace()
            .map(|s| s.to_string())
            .collect::<Vec<_>>();
        g.insert(from, dests);
    }
    g
}

fn count_paths<'a>(
    g: &'a HashMap<String, Vec<String>>,
    start: &'a str,
    target: &str,
    memo: &mut HashMap<&'a str, u64>,
) -> u64 {
    if let Some(&v) = memo.get(start) {
        return v;
    }
    if start == target {
        memo.insert(start, 1);
        return 1;
    }
    let mut total = 0;
    if let Some(nexts) = g.get(start) {
        for n in nexts {
            total += count_paths(g, n, target, memo);
        }
    }
    memo.insert(start, total);
    total
}

fn main() {
    let t0 = Instant::now();

    let g = load_graph("input.txt");
    let p1 = count_paths(&g, "you", "out", &mut HashMap::new());
    let a1 = count_paths(&g, "svr", "dac", &mut HashMap::new());
    let a2 = count_paths(&g, "dac", "fft", &mut HashMap::new());
    let a3 = count_paths(&g, "fft", "out", &mut HashMap::new());
    let b1 = count_paths(&g, "svr", "fft", &mut HashMap::new());
    let b2 = count_paths(&g, "fft", "dac", &mut HashMap::new());
    let b3 = count_paths(&g, "dac", "out", &mut HashMap::new());
    let p2 = a1 * a2 * a3 + b1 * b2 * b3;

    let elapsed_ms = t0.elapsed().as_secs_f64() * 1000.0;
    println!(
        "paths_you_to_out={} paths_svr_via_dac_fft={} elapsed_ms={:.3}",
        p1, p2, elapsed_ms
    );
}
