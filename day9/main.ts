import { readFileSync } from "fs";

type Point = { x: number; y: number };

function loadPoints(path: string): Point[] {
  return readFileSync(path, "utf8")
    .trim()
    .split(/\r?\n/)
    .map((line) => {
      const [x, y] = line.split(",").map(Number);
      return { x, y };
    });
}

function maxRectAny(pts: Point[]): number {
  let best = 0;
  for (let i = 0; i < pts.length; i++) {
    for (let j = i + 1; j < pts.length; j++) {
      if (pts[i].x === pts[j].x || pts[i].y === pts[j].y) continue;
      const area = Math.abs(pts[i].x - pts[j].x) * Math.abs(pts[i].y - pts[j].y);
      if (area > best) best = area;
    }
  }
  return best;
}

function pointInPoly(p: Point, poly: Point[]): boolean {
  let inside = false;
  for (let i = 0, j = poly.length - 1; i < poly.length; j = i++) {
    const pi = poly[i];
    const pj = poly[j];
    const cond =
      pi.y > p.y !== pj.y > p.y &&
      p.x < ((pj.x - pi.x) * (p.y - pi.y)) / (pj.y - pi.y) + pi.x;
    if (cond) inside = !inside;
  }
  return inside;
}

function maxRectInside(pts: Point[], poly: Point[]): number {
  let best = 0;
  for (let i = 0; i < pts.length; i++) {
    for (let j = i + 1; j < pts.length; j++) {
      if (pts[i].x === pts[j].x || pts[i].y === pts[j].y) continue;
      const x1 = Math.min(pts[i].x, pts[j].x);
      const x2 = Math.max(pts[i].x, pts[j].x);
      const y1 = Math.min(pts[i].y, pts[j].y);
      const y2 = Math.max(pts[i].y, pts[j].y);
      const corners = [
        { x: x1, y: y1 },
        { x: x1, y: y2 },
        { x: x2, y: y1 },
        { x: x2, y: y2 },
      ];
      if (corners.every((c) => pointInPoly(c, poly))) {
        const area = (x2 - x1) * (y2 - y1);
        if (area > best) best = area;
      }
    }
  }
  return best;
}

const pts = loadPoints("input.txt");
const poly = pts;
const t0 = performance.now();
const p1 = maxRectAny(pts);
const p2 = maxRectInside(pts, poly);
const elapsed = performance.now() - t0;
console.log(
  `max_rect_area=${p1} max_green_rect_area=${p2} elapsed_ms=${elapsed.toFixed(3)}`,
);
