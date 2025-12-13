use geo::{Coord, Polygon, Contains, Rect};
use std::collections::HashSet;
use std::fs;
use std::time::Instant;

fn load_points(path: &str) -> Vec<Coord<f64>> {
    let content = fs::read_to_string(path).expect("input");
    content
        .lines()
        .filter(|l| !l.is_empty())
        .map(|line| {
            let parts: Vec<_> = line.split(',').collect();
            Coord {
                x: parts[0].trim().parse().unwrap(),
                y: parts[1].trim().parse().unwrap(),
            }
        })
        .collect()
}

fn max_rect_any(pts: &[Coord<f64>]) -> i64 {
    let mut best = 0i64;
    for i in 0..pts.len() {
        for j in i + 1..pts.len() {
            let dx = (pts[i].x - pts[j].x).abs() as i64;
            let dy = (pts[i].y - pts[j].y).abs() as i64;
            let area = (dx + 1) * (dy + 1);
            if area > best {
                best = area;
            }
        }
    }
    best
}

fn max_rect_inside(pts: &[Coord<f64>], poly: &Polygon<f64>) -> i64 {
    let mut best = 0i64;
    for i in 0..pts.len() {
        for j in i + 1..pts.len() {
            if (pts[i].x - pts[j].x).abs() < 0.5 || (pts[i].y - pts[j].y).abs() < 0.5 {
                continue;
            }
            let xlo = pts[i].x.min(pts[j].x);
            let xhi = pts[i].x.max(pts[j].x);
            let ylo = pts[i].y.min(pts[j].y);
            let yhi = pts[i].y.max(pts[j].y);

            let rect = Rect::new(Coord { x: xlo, y: ylo }, Coord { x: xhi, y: yhi });

            if poly.contains(&rect) {
                let area = ((xhi - xlo) as i64 + 1) * ((yhi - ylo) as i64 + 1);
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
    let poly_coords: Vec<_> = pts.iter().cloned().collect();
    let poly = Polygon::new(poly_coords.into(), vec![]);

    let start = Instant::now();
    let p1 = max_rect_any(&pts);
    let p2 = max_rect_inside(&pts, &poly);
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    println!(
        "max_rect_area={} max_green_rect_area={} elapsed_ms={:.3}",
        p1, p2, elapsed
    );
}
