use std::fs;
use std::time::Instant;

type Point = (i64, i64);

fn load_points(path: &str) -> Vec<Point> {
    let content = fs::read_to_string(path).expect("input");
    content
        .lines()
        .filter(|l| !l.is_empty())
        .map(|line| {
            let parts: Vec<_> = line.split(',').collect();
            (
                parts[0].trim().parse().unwrap(),
                parts[1].trim().parse().unwrap(),
            )
        })
        .collect()
}

fn max_rect_any(pts: &[Point]) -> i64 {
    let mut best = 0i64;
    for i in 0..pts.len() {
        for j in i + 1..pts.len() {
            let dx = (pts[i].0 - pts[j].0).abs();
            let dy = (pts[i].1 - pts[j].1).abs();
            let area = (dx + 1) * (dy + 1);
            if area > best {
                best = area;
            }
        }
    }
    best
}

fn point_on_edge(px: i64, py: i64, x1: i64, y1: i64, x2: i64, y2: i64) -> bool {
    if x1 == x2 {
        px == x1 && py >= y1.min(y2) && py <= y1.max(y2)
    } else if y1 == y2 {
        py == y1 && px >= x1.min(x2) && px <= x1.max(x2)
    } else {
        false
    }
}

fn point_inside(px: i64, py: i64, poly: &[Point]) -> bool {
    let n = poly.len();
    let mut inside = false;
    let mut j = n - 1;
    for i in 0..n {
        let (x1, y1) = poly[j];
        let (x2, y2) = poly[i];
        if point_on_edge(px, py, x1, y1, x2, y2) {
            return true;
        }
        if (y1 > py) != (y2 > py) {
            let x_intersect = (x2 - x1) * (py - y1) / (y2 - y1) + x1;
            if px < x_intersect {
                inside = !inside;
            }
        }
        j = i;
    }
    inside
}

fn edge_crosses_interior(xlo: i64, xhi: i64, ylo: i64, yhi: i64, x1: i64, y1: i64, x2: i64, y2: i64) -> bool {
    if x1 == x2 {
        if x1 <= xlo || x1 >= xhi {
            return false;
        }
        let ya = y1.min(y2);
        let yb = y1.max(y2);
        if yb <= ylo || ya >= yhi {
            return false;
        }
        return ya < yhi && yb > ylo;
    } else if y1 == y2 {
        if y1 <= ylo || y1 >= yhi {
            return false;
        }
        let xa = x1.min(x2);
        let xb = x1.max(x2);
        if xb <= xlo || xa >= xhi {
            return false;
        }
        return xa < xhi && xb > xlo;
    }
    false
}

fn rect_inside_polygon(xlo: i64, xhi: i64, ylo: i64, yhi: i64, poly: &[Point]) -> bool {
    if !point_inside(xlo, ylo, poly) { return false; }
    if !point_inside(xlo, yhi, poly) { return false; }
    if !point_inside(xhi, ylo, poly) { return false; }
    if !point_inside(xhi, yhi, poly) { return false; }

    let n = poly.len();
    let mut j = n - 1;
    for i in 0..n {
        let (x1, y1) = poly[j];
        let (x2, y2) = poly[i];
        if edge_crosses_interior(xlo, xhi, ylo, yhi, x1, y1, x2, y2) {
            return false;
        }
        j = i;
    }
    true
}

fn max_rect_inside(pts: &[Point], poly: &[Point]) -> i64 {
    let mut best = 0i64;
    for i in 0..pts.len() {
        for j in i + 1..pts.len() {
            if pts[i].0 == pts[j].0 || pts[i].1 == pts[j].1 {
                continue;
            }
            let xlo = pts[i].0.min(pts[j].0);
            let xhi = pts[i].0.max(pts[j].0);
            let ylo = pts[i].1.min(pts[j].1);
            let yhi = pts[i].1.max(pts[j].1);

            if rect_inside_polygon(xlo, xhi, ylo, yhi, poly) {
                let area = (xhi - xlo + 1) * (yhi - ylo + 1);
                if area > best {
                    best = area;
                }
            }
        }
    }
    best
}

fn main() {
    let pts = load_points("input.txt");

    let start = Instant::now();
    let p1 = max_rect_any(&pts);
    let p2 = max_rect_inside(&pts, &pts);
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    println!(
        "max_rect_area={} max_green_rect_area={} elapsed_ms={:.3}",
        p1, p2, elapsed
    );
}
