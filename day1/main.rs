use std::fs::read_to_string;
use std::time::Instant;

fn simulate(lines: &[String]) -> (i32, i32, i32) {
    let mut pos: i32 = 50;
    let mut zero_hits = 0;
    let mut crossings = 0;

    for raw in lines {
        let line = raw.trim();
        if line.is_empty() {
            continue;
        }
        let sign = if line.as_bytes()[0] == b'R' { 1 } else { -1 };
        let mag: i32 = line[1..].parse().unwrap_or(0);

        let mut first = if sign == 1 { 100 - pos } else { pos };
        if first == 0 {
            first = 100;
        }
        if mag >= first {
            crossings += 1 + (mag - first) / 100;
        }

        pos = (pos + sign * mag).rem_euclid(100);
        if pos == 0 {
            zero_hits += 1;
        }
    }
    (zero_hits, crossings, pos)
}

fn main() {
    let start = Instant::now();
    let input = read_to_string("input.txt").expect("input");
    let lines: Vec<String> = input.lines().map(|s| s.to_string()).collect();

    let (zero_hits, crossings, pos) = simulate(&lines);

    println!(
        "zero_landings={} crossings={} final_pos={} elapsed_ms={:.3}",
        zero_hits,
        crossings,
        pos,
        start.elapsed().as_secs_f64() * 1000.0
    );
}
