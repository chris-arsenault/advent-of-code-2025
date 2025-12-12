use std::fs;
use std::time::Instant;

fn merge_ranges(mut ranges: Vec<(i64, i64)>) -> Vec<(i64, i64)> {
    ranges.sort_by_key(|r| (r.0, r.1));
    let mut merged: Vec<(i64, i64)> = Vec::new();
    for (a, b) in ranges {
        if merged.is_empty() || a > merged.last().unwrap().1 + 1 {
            merged.push((a, b));
        } else {
            let last = merged.last_mut().unwrap();
            if b > last.1 {
                last.1 = b;
            }
        }
    }
    merged
}

fn in_any(ranges: &[(i64, i64)], x: i64) -> bool {
    match ranges.binary_search_by_key(&(x + 1), |r| r.0) {
        Ok(idx) => ranges[idx].0 <= x && x <= ranges[idx].1,
        Err(idx) => {
            if idx == 0 {
                false
            } else {
                let (a, b) = ranges[idx - 1];
                a <= x && x <= b
            }
        }
    }
}

fn parse(path: &str) -> (Vec<(i64, i64)>, Vec<i64>) {
    let raw = fs::read_to_string(path).expect("input");
    let text = raw.replace("\r\n", "\n");
    let mut parts = text.splitn(2, "\n\n");
    let top = parts.next().unwrap_or("");
    let bottom = parts.next().unwrap_or("");
    let ranges = top
        .lines()
        .filter_map(|line| {
            let line = line.trim();
            if line.is_empty() {
                None
            } else {
                let bits: Vec<_> = line.split('-').collect();
                Some((bits[0].parse().unwrap(), bits[1].parse().unwrap()))
            }
        })
        .collect();
    let ids = bottom
        .lines()
        .filter_map(|line| {
            let line = line.trim();
            if line.is_empty() {
                None
            } else {
                Some(line.parse().unwrap())
            }
        })
        .collect();
    (ranges, ids)
}

fn solve(path: &str) -> (i64, i64) {
    let (ranges, ids) = parse(path);
    let merged = merge_ranges(ranges);
    let mut fresh = 0;
    for id in ids {
        if in_any(&merged, id) {
            fresh += 1;
        }
    }
    let mut total = 0;
    for (a, b) in merged {
        total += b - a + 1;
    }
    (fresh, total)
}

fn main() {
    let start = Instant::now();
    let (p1, p2) = solve("input.txt");
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    println!(
        "available_fresh={} total_fresh_ids={} elapsed_ms={:.3}",
        p1, p2, elapsed
    );
}
