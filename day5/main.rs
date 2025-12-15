use std::collections::BTreeMap;
use std::fs;
use std::time::Instant;

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

// Build a BTreeMap of merged intervals: start -> end
fn build_interval_map(ranges: Vec<(i64, i64)>) -> BTreeMap<i64, i64> {
    let mut sorted = ranges;
    sorted.sort_by_key(|r| (r.0, r.1));

    let mut map: BTreeMap<i64, i64> = BTreeMap::new();
    for (a, b) in sorted {
        // Find if we can merge with existing intervals
        // Get the interval that starts at or before 'a'
        let mut merged = false;
        if let Some((&start, &end)) = map.range(..=a).next_back() {
            if a <= end + 1 {
                // Can merge: extend the existing interval
                let new_end = end.max(b);
                map.insert(start, new_end);
                merged = true;
            }
        }
        if !merged {
            map.insert(a, b);
        }
    }
    map
}

// Check if a point is contained in any interval using BTreeMap range queries
fn contains_point(map: &BTreeMap<i64, i64>, x: i64) -> bool {
    // Find the interval with the largest start <= x
    if let Some((&_start, &end)) = map.range(..=x).next_back() {
        x <= end
    } else {
        false
    }
}

fn solve(path: &str) -> (i64, i64) {
    let (ranges, ids) = parse(path);
    let interval_map = build_interval_map(ranges);

    // Part 1: Count IDs in any interval
    let fresh = ids.iter().filter(|&&id| contains_point(&interval_map, id)).count() as i64;

    // Part 2: Total span of merged intervals
    let total: i64 = interval_map.iter().map(|(a, b)| b - a + 1).sum();

    (fresh, total)
}

fn main() {
    let start = Instant::now();
    let (p1, p2) = solve("input.txt");
    println!(
        "available_fresh={} total_fresh_ids={} elapsed_ms={:.3}",
        p1, p2, start.elapsed().as_secs_f64() * 1000.0
    );
}
