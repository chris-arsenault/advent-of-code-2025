use std::fs::read_to_string;
use std::time::Instant;

fn generate_even_half(max_n: u64) -> Vec<u64> {
    let mut vals = Vec::new();
    let max_len = max_n.to_string().len();
    for half_len in 1..=max_len / 2 {
        let start = 10u64.pow((half_len - 1) as u32);
        let end = 10u64.pow(half_len as u32);
        for t in start..end {
            let n = t * 10u64.pow(half_len as u32) + t;
            if n > max_n {
                break;
            }
            vals.push(n);
        }
    }
    vals.sort_unstable();
    vals
}

fn generate_periodic(max_n: u64) -> Vec<u64> {
    use std::collections::HashSet;
    let mut seen = HashSet::new();
    let max_len = max_n.to_string().len();
    for base_len in 1..=((max_len + 1) / 2) {
        let start = 10u64.pow((base_len - 1) as u32);
        let end = 10u64.pow(base_len as u32);
        for base in start..end {
            let base_str = base.to_string();
            for rep in 2.. {
                let total_len = base_len * rep;
                if total_len > max_len {
                    break;
                }
                let n_str = base_str.repeat(rep);
                let n: u64 = n_str.parse().unwrap();
                if n > max_n {
                    break;
                }
                seen.insert(n);
            }
        }
    }
    let mut vals: Vec<u64> = seen.into_iter().collect();
    vals.sort_unstable();
    vals
}

fn prefix(nums: &[u64]) -> Vec<u128> {
    let mut ps = Vec::with_capacity(nums.len() + 1);
    ps.push(0);
    for &n in nums {
        let last = *ps.last().unwrap();
        ps.push(last + n as u128);
    }
    ps
}

fn range_sum(nums: &[u64], ps: &[u128], lo: u64, hi: u64) -> u128 {
    let i = nums.partition_point(|&x| x < lo);
    let j = nums.partition_point(|&x| x <= hi);
    ps[j] - ps[i]
}

fn parse_ranges(text: &str) -> Vec<(u64, u64)> {
    text.replace('\n', ",")
        .split(',')
        .filter_map(|part| {
            if part.trim().is_empty() {
                None
            } else {
                let bits: Vec<_> = part.split('-').collect();
                Some((bits[0].parse().unwrap(), bits[1].parse().unwrap()))
            }
        })
        .collect()
}

fn solve(text: &str) -> (u128, u128) {
    let ranges = parse_ranges(text);
    let max_n = ranges.iter().map(|&(_, b)| b).max().unwrap_or(0);
    let evens = generate_even_half(max_n);
    let ev_ps = prefix(&evens);
    let per = generate_periodic(max_n);
    let per_ps = prefix(&per);

    let mut p1: u128 = 0;
    let mut p2: u128 = 0;
    for (a, b) in ranges {
        p1 += range_sum(&evens, &ev_ps, a, b);
        p2 += range_sum(&per, &per_ps, a, b);
    }
    (p1, p2)
}

fn main() {
    let start = Instant::now();
    let text = read_to_string("input.txt").expect("input");
    let (p1, p2) = solve(&text);
    println!(
        "repeated-halves-sum={} repeated-pattern-sum={} elapsed_ms={:.3}",
        p1,
        p2,
        start.elapsed().as_secs_f64() * 1000.0
    );
}
