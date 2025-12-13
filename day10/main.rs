use regex::Regex;
use std::fs;
use std::time::Instant;

#[derive(Clone)]
struct Machine {
    pattern: String,
    buttons: Vec<Vec<usize>>,
    targets: Vec<i32>,
}

fn parse_machine(line: &str) -> Machine {
    let pat_re = Regex::new(r"\[(.*?)\]").unwrap();
    let button_re = Regex::new(r"\(([^)]*)\)").unwrap();
    let target_re = Regex::new(r"\{([^}]*)\}").unwrap();

    let pattern = pat_re.captures(line).unwrap()[1].to_string();
    let buttons = button_re
        .captures_iter(line)
        .map(|cap| {
            let s = cap[1].trim();
            if s.is_empty() {
                Vec::new()
            } else {
                s.split(',')
                    .map(|t| t.trim().parse::<usize>().unwrap())
                    .collect::<Vec<usize>>()
            }
        })
        .collect::<Vec<_>>();
    let targets = target_re.captures(line).unwrap()[1]
        .split(',')
        .map(|t| t.trim().parse::<i32>().unwrap())
        .collect::<Vec<i32>>();
    Machine {
        pattern,
        buttons,
        targets,
    }
}

fn load(path: &str) -> Vec<Machine> {
    fs::read_to_string(path)
        .expect("input")
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(parse_machine)
        .collect()
}

fn part1(machines: &[Machine]) -> i32 {
    let mut total = 0;
    for m in machines {
        let lights = m.pattern.len();
        let mut target = 0u32;
        for (i, ch) in m.pattern.as_bytes().iter().enumerate() {
            if *ch == b'#' {
                target |= 1 << i;
            }
        }
        let button_masks: Vec<u32> = m
            .buttons
            .iter()
            .map(|b| {
                let mut mask = 0u32;
                for &idx in b {
                    if idx < lights {
                        mask ^= 1 << idx;
                    }
                }
                mask
            })
            .collect();
        let n = button_masks.len();
        let mut best: Option<i32> = None;
        for mask in 0..(1u32 << n) {
            let mut state = 0u32;
            let presses = mask.count_ones() as i32;
            for i in 0..n {
                if (mask >> i) & 1 == 1 {
                    state ^= button_masks[i];
                }
            }
            if state == target {
                if best.is_none() || presses < best.unwrap() {
                    best = Some(presses);
                }
            }
        }
        total += best.unwrap_or(0);
    }
    total
}

fn part2(machines: &[Machine]) -> i32 {
    let mut total = 0;
    const EPS: f64 = 1e-9;

    for m in machines {
        let counters = m.targets.len();
        let buttons = m.buttons.len();
        if counters == 0 || buttons == 0 {
            continue;
        }

        // Build matrix counters x buttons
        let mut mat = vec![vec![0.0f64; buttons]; counters];
        for (cidx, b) in m.buttons.iter().enumerate() {
            for &t in b {
                if t < counters {
                    mat[t][cidx] = 1.0;
                }
            }
        }
        let mut rhs: Vec<f64> = m.targets.iter().map(|v| *v as f64).collect();

        // RREF
        let mut pivot_cols = vec![-1isize; counters];
        let mut row = 0;
        for col in 0..buttons {
            if row >= counters {
                break;
            }
            let mut pivot = -1;
            let mut best = 0.0;
            for r in row..counters {
                let val = mat[r][col].abs();
                if val > EPS && val > best {
                    best = val;
                    pivot = r as isize;
                }
            }
            if pivot == -1 {
                continue;
            }
            let p = pivot as usize;
            mat.swap(row, p);
            rhs.swap(row, p);
            let piv = mat[row][col];
            for c in col..buttons {
                mat[row][c] /= piv;
            }
            rhs[row] /= piv;
            for r in 0..counters {
                if r == row {
                    continue;
                }
                let f = mat[r][col];
                if f.abs() < EPS {
                    continue;
                }
                for c in col..buttons {
                    mat[r][c] -= f * mat[row][c];
                }
                rhs[r] -= f * rhs[row];
            }
            pivot_cols[row] = col as isize;
            row += 1;
        }
        let rank = row;
        let mut inconsistent = false;
        for r in rank..counters {
            let mut maxv = 0.0;
            for c in 0..buttons {
                let v = mat[r][c].abs();
                if v > maxv {
                    maxv = v;
                }
            }
            if maxv < EPS && rhs[r].abs() > EPS {
                inconsistent = true;
                break;
            }
        }
        if inconsistent {
            continue;
        }

        let mut used = vec![false; buttons];
        for r in 0..rank {
            if pivot_cols[r] >= 0 {
                used[pivot_cols[r] as usize] = true;
            }
        }
        let free_cols: Vec<usize> = (0..buttons).filter(|&c| !used[c]).collect();
        let free_count = free_cols.len();

        let mut coef = vec![vec![0.0f64; free_count]; rank];
        for r in 0..rank {
            for (fidx, &fc) in free_cols.iter().enumerate() {
                coef[r][fidx] = mat[r][fc];
            }
        }

        let sum_targets: i32 = m.targets.iter().sum();
        let mut best = sum_targets;
        let mut free_vals = vec![0i32; free_count];

        fn evaluate(
            cur: i32,
            rank: usize,
            free_count: usize,
            rhs: &[f64],
            coef: &[Vec<f64>],
            free_vals: &[i32],
            eps: f64,
            best: &mut i32,
        ) {
            if cur >= *best {
                return;
            }
            let mut sum = cur;
            for r in 0..rank {
                let mut v = rhs[r];
                for f in 0..free_count {
                    v -= coef[r][f] * free_vals[f] as f64;
                }
                if v < -eps {
                    return;
                }
                let iv = v.round() as i32;
                if (v - iv as f64).abs() > eps {
                    return;
                }
                sum += iv;
                if sum >= *best {
                    return;
                }
            }
            if sum < *best {
                *best = sum;
            }
        }

        evaluate(0, rank, free_count, &rhs, &coef, &free_vals, EPS, &mut best);

        fn quick(
            idx: usize,
            cur: i32,
            cap: i32,
            free_count: usize,
            free_vals: &mut [i32],
            rank: usize,
            rhs: &[f64],
            coef: &[Vec<f64>],
            eps: f64,
            best: &mut i32,
        ) {
            if cur >= *best {
                return;
            }
            if idx == free_count {
                evaluate(cur, rank, free_count, rhs, coef, free_vals, eps, best);
                return;
            }
            for v in 0..=cap {
                if cur + v >= *best {
                    break;
                }
                free_vals[idx] = v;
                quick(
                    idx + 1,
                    cur + v,
                    cap,
                    free_count,
                    free_vals,
                    rank,
                    rhs,
                    coef,
                    eps,
                    best,
                );
            }
        }

        let mut seed_cap = 400;
        if best < seed_cap {
            seed_cap = best;
        }
        if free_count > 0 && seed_cap > 0 {
            quick(
                0,
                0,
                seed_cap,
                free_count,
                &mut free_vals,
                rank,
                &rhs,
                &coef,
                EPS,
                &mut best,
            );
        }

        fn dfs(
            idx: usize,
            cur: i32,
            free_count: usize,
            free_vals: &mut [i32],
            rank: usize,
            rhs: &[f64],
            coef: &[Vec<f64>],
            eps: f64,
            best: &mut i32,
        ) {
            if cur >= *best {
                return;
            }
            if idx == free_count {
                evaluate(cur, rank, free_count, rhs, coef, free_vals, eps, best);
                return;
            }
            let maxv = *best - cur;
            for v in 0..=maxv {
                free_vals[idx] = v;
                dfs(
                    idx + 1,
                    cur + v,
                    free_count,
                    free_vals,
                    rank,
                    rhs,
                    coef,
                    eps,
                    best,
                );
            }
        }

        if free_count > 0 {
            dfs(
                0,
                0,
                free_count,
                &mut free_vals,
                rank,
                &rhs,
                &coef,
                EPS,
                &mut best,
            );
        } else {
            evaluate(0, rank, free_count, &rhs, &coef, &free_vals, EPS, &mut best);
        }

        total += best;
    }
    total
}

fn main() {
    let machines = load("input.txt");
    let start = Instant::now();
    let p1 = part1(&machines);
    let p2 = part2(&machines);
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    println!(
        "min_lights_presses={} min_counter_presses={} elapsed_ms={:.3}",
        p1, p2, elapsed
    );
}
