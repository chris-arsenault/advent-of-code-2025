use std::fs;
use std::time::Instant;

fn best_two(line: &str) -> i64 {
    let bytes = line.trim().as_bytes();
    if bytes.len() < 2 {
        return 0;
    }
    let n = bytes.len();
    let mut suffix = vec![b'0'; n + 1];
    for i in (0..n).rev() {
        suffix[i] = suffix[i + 1].max(bytes[i]);
    }
    let mut best: i64 = -1;
    for i in 0..n - 1 {
        let val = ((bytes[i] - b'0') as i64) * 10 + (suffix[i + 1] - b'0') as i64;
        if val > best {
            best = val;
        }
    }
    best
}

fn best_k(line: &str, k: usize) -> i64 {
    let mut digits: Vec<u8> = line.trim().bytes().collect();
    let mut drop = digits.len().saturating_sub(k);
    let mut stack: Vec<u8> = Vec::with_capacity(k);
    for d in digits.drain(..) {
        while drop > 0 && !stack.is_empty() && *stack.last().unwrap() < d {
            stack.pop();
            drop -= 1;
        }
        stack.push(d);
    }
    stack.truncate(k);
    stack.iter().fold(0i64, |acc, &d| acc * 10 + (d - b'0') as i64)
}

fn main() {
    let path = if fs::metadata("input_eric.txt").is_ok() {
        "input_eric.txt"
    } else {
        "input.txt"
    };
    let content = fs::read_to_string(path).expect("input");
    let lines: Vec<&str> = content.lines().collect();

    let start = Instant::now();
    let mut p1: i64 = 0;
    let mut p2: i64 = 0;
    for line in lines {
        p1 += best_two(line);
        p2 += best_k(line, 12);
    }
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    println!(
        "max-2-digit-sum={} max-12-digit-sum={} elapsed_ms={:.3}",
        p1, p2, elapsed
    );
}
