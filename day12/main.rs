use std::fs;
use std::time::Instant;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

#[derive(Clone)]
struct Shape {
    forms: Vec<Vec<Point>>,
    area: i32,
}

fn normalize(cells: &[Point]) -> Vec<Point> {
    let minx = cells.iter().map(|c| c.x).min().unwrap();
    let miny = cells.iter().map(|c| c.y).min().unwrap();
    cells.iter().map(|c| Point { x: c.x - minx, y: c.y - miny }).collect()
}

fn rotate(cells: &[Point]) -> Vec<Point> {
    let maxx = cells.iter().map(|c| c.x).max().unwrap();
    cells.iter().map(|c| Point { x: c.y, y: maxx - c.x }).collect()
}

fn flip(cells: &[Point]) -> Vec<Point> {
    let maxx = cells.iter().map(|c| c.x).max().unwrap();
    cells.iter().map(|c| Point { x: maxx - c.x, y: c.y }).collect()
}

fn orientations(cells: &[Point]) -> Vec<Vec<Point>> {
    use std::collections::HashSet;
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    let mut cur = cells.to_vec();
    for _ in 0..4 {
        for shape in [&cur[..], &flip(&cur)[..]] {
            let norm = normalize(shape);
            let mut key = String::new();
            for c in norm.iter() {
                key.push_str(&format!("{},{};", c.x, c.y));
            }
            if seen.insert(key) {
                result.push(norm);
            }
        }
        cur = rotate(&cur);
    }
    result
}

fn parse_input(path: &str) -> (Vec<Shape>, Vec<(i32, i32)>, Vec<Vec<i32>>) {
    let text = fs::read_to_string(path).expect("input");
    let mut shapes = Vec::new();
    let mut regions = Vec::new();
    let mut counts = Vec::new();
    let mut lines = text.lines().peekable();
    let mut reading_regions = false;
    while let Some(line) = lines.next() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if !reading_regions && line.contains('x') {
            reading_regions = true;
        }
        if !reading_regions {
            if line.ends_with(':') {
                let mut grid = Vec::new();
                for _ in 0..3 {
                    if let Some(l) = lines.next() {
                        let lt = l.trim();
                        if lt.is_empty() {
                            break;
                        }
                        grid.push(lt.to_string());
                    }
                }
                let mut cells = Vec::new();
                for (y, row) in grid.iter().enumerate() {
                    for (x, ch) in row.chars().enumerate() {
                        if ch == '#' {
                            cells.push(Point { x: x as i32, y: y as i32 });
                        }
                    }
                }
                let area = cells.len() as i32;
                shapes.push(Shape { forms: orientations(&cells), area });
            }
        } else {
            let mut parts = line.split(':');
            let size = parts.next().unwrap();
            let counts_part = parts.next().unwrap_or("");
            let mut size_bits = size.split('x');
            let w: i32 = size_bits.next().unwrap().parse().unwrap();
            let h: i32 = size_bits.next().unwrap().parse().unwrap();
            regions.push((w, h));
            let row: Vec<i32> = counts_part
                .split_whitespace()
                .filter_map(|s| s.parse().ok())
                .collect();
            counts.push(row);
        }
    }
    (shapes, regions, counts)
}

fn can_pack_small(w: i32, h: i32, shapes: &[Shape], counts: &[i32]) -> bool {
    let mut total_pieces = 0;
    let mut needed_area = 0;
    for (i, &c) in counts.iter().enumerate() {
        total_pieces += c;
        needed_area += c * shapes[i].area;
    }
    if needed_area > w * h {
        return false;
    }
    if total_pieces == 0 {
        return true;
    }
    let mut placements: Vec<Vec<u128>> = vec![Vec::new(); shapes.len()];
    for (si, sh) in shapes.iter().enumerate() {
        for form in sh.forms.iter() {
            let maxx = form.iter().map(|c| c.x).max().unwrap();
            let maxy = form.iter().map(|c| c.y).max().unwrap();
            for y in 0..=h - maxy - 1 {
                for x in 0..=w - maxx - 1 {
                    let mut mask: u128 = 0;
                    for c in form {
                        let pos = (y + c.y) * w + (x + c.x);
                        mask |= 1u128 << pos;
                    }
                    placements[si].push(mask);
                }
            }
        }
    }
    let mut order = Vec::new();
    for (i, &c) in counts.iter().enumerate() {
        for _ in 0..c {
            order.push(i);
        }
    }
    order.sort_by_key(|&i| placements[i].len());

    fn dfs(idx: usize, order: &[usize], placements: &[Vec<u128>], used: u128) -> bool {
        if idx == order.len() {
            return true;
        }
        let si = order[idx];
        for &pm in placements[si].iter() {
            if pm & used == 0 {
                if dfs(idx + 1, order, placements, used | pm) {
                    return true;
                }
            }
        }
        false
    }

    dfs(0, &order, &placements, 0)
}

fn solve(shapes: &[Shape], regions: &[(i32, i32)], counts: &[Vec<i32>]) -> i32 {
    let mut good = 0;
    for (i, &(w, h)) in regions.iter().enumerate() {
        let mut pieces = 0;
        let mut area = 0;
        for (j, &c) in counts[i].iter().enumerate() {
            pieces += c;
            area += c * shapes[j].area;
        }
        if area > w * h {
            continue;
        }
        if (w * h) <= 400 && pieces <= 25 {
            if can_pack_small(w, h, shapes, &counts[i]) {
                good += 1;
            }
        } else {
            good += 1;
        }
    }
    good
}

fn main() {
    let (shapes, regions, counts) = parse_input("input.xt");
    let start = Instant::now();
    let ans = solve(&shapes, &regions, &counts);
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    println!("regions_that_fit={} elapsed_ms={:.3}", ans, elapsed);
}
