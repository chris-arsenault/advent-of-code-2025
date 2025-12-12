use std::fs;
use std::time::Instant;

#[derive(Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
}

fn load_points(path: &str) -> Vec<Point> {
    let content = fs::read_to_string(path).expect("input");
    content
        .lines()
        .map(|line| {
            let parts: Vec<_> = line.split(',').collect();
            Point {
                x: parts[0].parse().unwrap(),
                y: parts[1].parse().unwrap(),
            }
        })
        .collect()
}

fn max_rect_any(pts: &[Point]) -> i32 {
    let mut best = 0;
    for i in 0..pts.len() {
        for j in i + 1..pts.len() {
            if pts[i].x == pts[j].x || pts[i].y == pts[j].y {
                continue;
            }
            let area = (pts[i].x - pts[j].x).abs() * (pts[i].y - pts[j].y).abs();
            if area > best {
                best = area;
            }
        }
    }
    best
}

fn point_in_poly(p: Point, poly: &[Point]) -> bool {
    let mut inside = false;
    let n = poly.len();
    let mut j = n - 1;
    for i in 0..n {
        let pi = poly[i];
        let pj = poly[j];
        let cond = ((pi.y > p.y) != (pj.y > p.y))
            && (p.x < (pj.x - pi.x) * (p.y - pi.y) / (pj.y - pi.y) + pi.x);
        if cond {
            inside = !inside;
        }
        j = i;
    }
    inside
}

fn max_rect_inside(pts: &[Point], poly: &[Point]) -> i32 {
    let mut best = 0;
    for i in 0..pts.len() {
        for j in i + 1..pts.len() {
            if pts[i].x == pts[j].x || pts[i].y == pts[j].y {
                continue;
            }
            let x1 = pts[i].x.min(pts[j].x);
            let x2 = pts[i].x.max(pts[j].x);
            let y1 = pts[i].y.min(pts[j].y);
            let y2 = pts[i].y.max(pts[j].y);
            let corners = [
                Point { x: x1, y: y1 },
                Point { x: x1, y: y2 },
                Point { x: x2, y: y1 },
                Point { x: x2, y: y2 },
            ];
            if corners.iter().all(|&c| point_in_poly(c, poly)) {
                let area = (x2 - x1) * (y2 - y1);
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
