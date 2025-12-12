use std::fs;
use std::time::Instant;

fn load_grid(path: &str) -> Vec<String> {
    let content = fs::read_to_string(path).expect("input");
    let mut lines: Vec<String> = content.lines().map(|s| s.to_string()).collect();
    let maxw = lines.iter().map(|l| l.len()).max().unwrap_or(0);
    for l in lines.iter_mut() {
        if l.len() < maxw {
            l.push_str(&" ".repeat(maxw - l.len()));
        }
    }
    lines
}

fn split_blocks(grid: &[String]) -> Vec<(usize, usize)> {
    let h = grid.len();
    let w = grid[0].len();
    let mut empty_col = vec![true; w];
    for c in 0..w {
        for r in 0..h {
            if grid[r].as_bytes()[c] != b' ' {
                empty_col[c] = false;
                break;
            }
        }
    }
    let mut blocks = Vec::new();
    let mut c = 0;
    while c < w {
        while c < w && empty_col[c] {
            c += 1;
        }
        if c >= w {
            break;
        }
        let start = c;
        while c < w && !empty_col[c] {
            c += 1;
        }
        blocks.push((start, c));
    }
    blocks
}

fn problem_op(col: &[u8]) -> u8 {
    for &ch in col {
        if ch == b'+' || ch == b'*' {
            return ch;
        }
    }
    b'+'
}

fn part1(grid: &[String], blocks: &[(usize, usize)]) -> i64 {
    let op_row = grid.last().unwrap().as_bytes();
    let mut total = 0i64;
    for &(s, e) in blocks {
        let op = problem_op(&op_row[s..e]);
        let mut nums: Vec<i64> = Vec::new();
        for row in &grid[..grid.len() - 1] {
            let tok = row[s..e].trim();
            if !tok.is_empty() {
                nums.push(tok.parse().unwrap());
            }
        }
        let mut acc = if op == b'*' { 1 } else { 0 };
        for n in nums {
            if op == b'+' {
                acc += n;
            } else {
                acc *= n;
            }
        }
        total += acc;
    }
    total
}

fn part2(grid: &[String], blocks: &[(usize, usize)]) -> i64 {
    let h = grid.len() - 1;
    let op_row = grid.last().unwrap().as_bytes();
    let mut total = 0i64;
    for &(s, e) in blocks {
        let op = problem_op(&op_row[s..e]);
        let mut nums: Vec<i64> = Vec::new();
        for c in (s..e).rev() {
            let mut digits = Vec::new();
            for r in 0..h {
                let ch = grid[r].as_bytes()[c];
                if ch.is_ascii_digit() {
                    digits.push(ch);
                }
            }
            if !digits.is_empty() {
                let mut v = 0i64;
                for d in digits {
                    v = v * 10 + (d - b'0') as i64;
                }
                nums.push(v);
            }
        }
        let mut acc = if op == b'*' { 1 } else { 0 };
        for n in nums {
            if op == b'+' {
                acc += n;
            } else {
                acc *= n;
            }
        }
        total += acc;
    }
    total
}

fn main() {
    let grid = load_grid("input.txt");
    let blocks = split_blocks(&grid);
    let start = Instant::now();
    let p1 = part1(&grid, &blocks);
    let p2 = part2(&grid, &blocks);
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    println!(
        "grand_total={} quantum_total={} elapsed_ms={:.3}",
        p1, p2, elapsed
    );
}
