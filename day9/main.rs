use std::fs;
use std::time::Instant;

#[derive(Clone, Copy)]
struct Point {
    x: i64,
    y: i64,
}

fn load_points(path: &str) -> Vec<Point> {
    let content = fs::read_to_string(path).expect("input");
    content
        .lines()
        .filter(|l| !l.is_empty())
        .map(|line| {
            let parts: Vec<_> = line.split(',').collect();
            Point {
                x: parts[0].trim().parse().unwrap(),
                y: parts[1].trim().parse().unwrap(),
            }
        })
        .collect()
}

fn max_rect_any(pts: &[Point]) -> i64 {
    let mut best = 0;
    for i in 0..pts.len() {
        for j in i + 1..pts.len() {
            let dx = (pts[i].x - pts[j].x).abs();
            let dy = (pts[i].y - pts[j].y).abs();
            let area = (dx + 1) * (dy + 1);
            if area > best {
                best = area;
            }
        }
    }
    best
}

fn point_inside(px: i64, py: i64, poly: &[Point]) -> bool {
    let n = poly.len();
    let mut inside = false;
    let mut j = n - 1;
    for i in 0..n {
        let (x1, y1) = (poly[j].x, poly[j].y);
        let (x2, y2) = (poly[i].x, poly[i].y);
        // Check if on edge
        if x1 == x2 {
            if px == x1 && py >= y1.min(y2) && py <= y1.max(y2) {
                return true;
            }
        } else if y1 == y2 {
            if py == y1 && px >= x1.min(x2) && px <= x1.max(x2) {
                return true;
            }
        }
        // Ray casting
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
        // Vertical edge
        if x1 <= xlo || x1 >= xhi {
            return false;
        }
        let ya = y1.min(y2);
        let yb = y1.max(y2);
        if yb <= ylo || ya >= yhi {
            return false;
        }
        ya < yhi && yb > ylo
    } else if y1 == y2 {
        // Horizontal edge
        if y1 <= ylo || y1 >= yhi {
            return false;
        }
        let xa = x1.min(x2);
        let xb = x1.max(x2);
        if xb <= xlo || xa >= xhi {
            return false;
        }
        xa < xhi && xb > xlo
    } else {
        false
    }
}

fn rect_inside_polygon(xlo: i64, xhi: i64, ylo: i64, yhi: i64, poly: &[Point]) -> bool {
    // Check all 4 corners are inside
    if !point_inside(xlo, ylo, poly) { return false; }
    if !point_inside(xlo, yhi, poly) { return false; }
    if !point_inside(xhi, ylo, poly) { return false; }
    if !point_inside(xhi, yhi, poly) { return false; }

    // Check no polygon edge crosses the interior
    let n = poly.len();
    let mut j = n - 1;
    for i in 0..n {
        if edge_crosses_interior(xlo, xhi, ylo, yhi, poly[j].x, poly[j].y, poly[i].x, poly[i].y) {
            return false;
        }
        j = i;
    }
    true
}

fn max_rect_inside(pts: &[Point], poly: &[Point]) -> i64 {
    let mut best = 0;
    for i in 0..pts.len() {
        for j in i + 1..pts.len() {
            if pts[i].x == pts[j].x || pts[i].y == pts[j].y {
                continue;
            }
            let xlo = pts[i].x.min(pts[j].x);
            let xhi = pts[i].x.max(pts[j].x);
            let ylo = pts[i].y.min(pts[j].y);
            let yhi = pts[i].y.max(pts[j].y);

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
    let poly = pts.clone();
    let start = Instant::now();
    let p1 = max_rect_any(&pts);
    let p2 = max_rect_inside(&pts, &poly);
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    println!(
        "max_rect_area={} max_green_rect_area={} elapsed_ms={:.3}",
        p1, p2, elapsed
    );
}
