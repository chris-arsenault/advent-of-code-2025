use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::time::Instant;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    r: i32,
    c: i32,
}

fn parse_grid(lines: &[&str]) -> HashSet<Point> {
    let mut rolls = HashSet::new();
    for (r, line) in lines.iter().enumerate() {
        for (c, ch) in line.chars().enumerate() {
            if ch == '@' {
                rolls.insert(Point {
                    r: r as i32,
                    c: c as i32,
                });
            }
        }
    }
    rolls
}

fn neighbor_counts(rolls: &HashSet<Point>) -> HashMap<Point, i32> {
    let dirs = [
        Point { r: -1, c: -1 },
        Point { r: -1, c: 0 },
        Point { r: -1, c: 1 },
        Point { r: 0, c: -1 },
        Point { r: 0, c: 1 },
        Point { r: 1, c: -1 },
        Point { r: 1, c: 0 },
        Point { r: 1, c: 1 },
    ];
    let mut counts = HashMap::new();
    for &p in rolls {
        let mut cnt = 0;
        for d in dirs.iter() {
            if rolls.contains(&Point {
                r: p.r + d.r,
                c: p.c + d.c,
            }) {
                cnt += 1;
            }
        }
        counts.insert(p, cnt);
    }
    counts
}

fn part1(counts: &HashMap<Point, i32>) -> i32 {
    counts.values().filter(|&&v| v < 4).count() as i32
}

fn part2(mut rolls: HashSet<Point>, mut counts: HashMap<Point, i32>) -> i32 {
    let dirs = [
        Point { r: -1, c: -1 },
        Point { r: -1, c: 0 },
        Point { r: -1, c: 1 },
        Point { r: 0, c: -1 },
        Point { r: 0, c: 1 },
        Point { r: 1, c: -1 },
        Point { r: 1, c: 0 },
        Point { r: 1, c: 1 },
    ];
    let mut q: VecDeque<Point> = VecDeque::new();
    let mut in_q: HashSet<Point> = HashSet::new();
    for (&p, &c) in counts.iter() {
        if c < 4 {
            q.push_back(p);
            in_q.insert(p);
        }
    }
    let mut removed = 0;
    while let Some(p) = q.pop_front() {
        if !rolls.remove(&p) {
            continue;
        }
        removed += 1;
        for d in dirs.iter() {
            let np = Point {
                r: p.r + d.r,
                c: p.c + d.c,
            };
            if let Some(entry) = counts.get_mut(&np) {
                *entry -= 1;
                if *entry < 4 && !in_q.contains(&np) {
                    q.push_back(np);
                    in_q.insert(np);
                }
            }
        }
    }
    removed
}

fn main() {
    let text = fs::read_to_string("input.txt").expect("input");
    let lines: Vec<&str> = text.lines().collect();
    let rolls = parse_grid(&lines);
    let counts = neighbor_counts(&rolls);

    let start = Instant::now();
    let a = part1(&counts);
    let removed = part2(rolls.clone(), counts.clone());
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;

    println!(
        "accessible={} removable_total={} elapsed_ms={:.3}",
        a, removed, elapsed
    );
}
