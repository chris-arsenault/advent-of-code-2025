use std::collections::{HashSet, HashMap, VecDeque};
use std::fs;
use std::time::Instant;

fn load_grid(path: &str) -> (Vec<String>, usize, usize) {
    let content = fs::read_to_string(path).expect("input");
    let mut sr = 0usize;
    let mut sc = 0usize;
    let rows: Vec<String> = content
        .lines()
        .enumerate()
        .map(|(r, line)| {
            if let Some(idx) = line.find('S') {
                sr = r;
                sc = idx;
            }
            line.to_string()
        })
        .collect();
    (rows, sr, sc)
}

fn part1(grid: &[String], sr: usize, sc: usize) -> i32 {
    let h = grid.len();
    let w = grid[0].len();
    let mut active = HashSet::new();
    active.insert(sc);
    let mut splits = 0;
    for r in sr..h {
        let mut next = HashSet::new();
        let mut queue: VecDeque<usize> = active.iter().cloned().collect();
        let mut seen = HashSet::new();
        while let Some(c) = queue.pop_front() {
            if !seen.insert(c) {
                continue;
            }
            let cell = grid[r].as_bytes()[c];
            if cell == b'^' {
                splits += 1;
                if c > 0 {
                    queue.push_back(c - 1);
                }
                if c + 1 < w {
                    queue.push_back(c + 1);
                }
            } else {
                next.insert(c);
            }
        }
        active = next;
        if active.is_empty() {
            break;
        }
    }
    splits
}

fn part2(grid: &[String], sr: usize, sc: usize) -> u64 {
    let h = grid.len();
    let w = grid[0].len();
    let mut active: HashMap<usize, u64> = HashMap::new();
    active.insert(sc, 1);
    for r in sr..h {
        let mut next: HashMap<usize, u64> = HashMap::new();
        for (&c, &cnt) in active.iter() {
            let cell = grid[r].as_bytes()[c];
            if cell == b'^' {
                if c > 0 {
                    *next.entry(c - 1).or_insert(0) += cnt;
                }
                if c + 1 < w {
                    *next.entry(c + 1).or_insert(0) += cnt;
                }
            } else {
                *next.entry(c).or_insert(0) += cnt;
            }
        }
        active = next;
        if active.is_empty() {
            break;
        }
    }
    active.values().sum()
}

fn main() {
    let (grid, sr, sc) = load_grid("input.txt");
    let start = Instant::now();
    let p1 = part1(&grid, sr, sc);
    let p2 = part2(&grid, sr, sc);
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    println!("splits={} timelines={} elapsed_ms={:.3}", p1, p2, elapsed);
}
