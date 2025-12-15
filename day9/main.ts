import { readFileSync } from "fs";
import { performance } from "perf_hooks";

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
      const area = (Math.abs(pts[i].x - pts[j].x) + 1) * (Math.abs(pts[i].y - pts[j].y) + 1);
      if (area > best) best = area;
    }
  }
  return best;
}

function pointOnEdge(px: number, py: number, x1: number, y1: number, x2: number, y2: number): boolean {
  if (x1 === x2) {
    return px === x1 && py >= Math.min(y1, y2) && py <= Math.max(y1, y2);
  } else if (y1 === y2) {
    return py === y1 && px >= Math.min(x1, x2) && px <= Math.max(x1, x2);
  }
  return false;
}

function pointInside(px: number, py: number, poly: Point[]): boolean {
  const n = poly.length;
  let inside = false;
  let j = n - 1;
  for (let i = 0; i < n; i++) {
    const x1 = poly[j].x, y1 = poly[j].y;
    const x2 = poly[i].x, y2 = poly[i].y;
    if (pointOnEdge(px, py, x1, y1, x2, y2)) return true;
    if ((y1 > py) !== (y2 > py)) {
      const xIntersect = (x2 - x1) * (py - y1) / (y2 - y1) + x1;
      if (px < xIntersect) inside = !inside;
    }
    j = i;
  }
  return inside;
}

function edgeCrossesInterior(xlo: number, xhi: number, ylo: number, yhi: number, x1: number, y1: number, x2: number, y2: number): boolean {
  if (x1 === x2) {
    if (x1 <= xlo || x1 >= xhi) return false;
    const ya = Math.min(y1, y2);
    const yb = Math.max(y1, y2);
    if (yb <= ylo || ya >= yhi) return false;
    return ya < yhi && yb > ylo;
  } else if (y1 === y2) {
    if (y1 <= ylo || y1 >= yhi) return false;
    const xa = Math.min(x1, x2);
    const xb = Math.max(x1, x2);
    if (xb <= xlo || xa >= xhi) return false;
    return xa < xhi && xb > xlo;
  }
  return false;
}

function rectInsidePolygon(xlo: number, xhi: number, ylo: number, yhi: number, poly: Point[]): boolean {
  if (!pointInside(xlo, ylo, poly)) return false;
  if (!pointInside(xlo, yhi, poly)) return false;
  if (!pointInside(xhi, ylo, poly)) return false;
  if (!pointInside(xhi, yhi, poly)) return false;

  const n = poly.length;
  let j = n - 1;
  for (let i = 0; i < n; i++) {
    const x1 = poly[j].x, y1 = poly[j].y;
    const x2 = poly[i].x, y2 = poly[i].y;
    if (edgeCrossesInterior(xlo, xhi, ylo, yhi, x1, y1, x2, y2)) return false;
    j = i;
  }
  return true;
}

function maxRectInside(pts: Point[], poly: Point[]): number {
  let best = 0;
  for (let i = 0; i < pts.length; i++) {
    for (let j = i + 1; j < pts.length; j++) {
      if (pts[i].x === pts[j].x || pts[i].y === pts[j].y) continue;
      const xlo = Math.min(pts[i].x, pts[j].x);
      const xhi = Math.max(pts[i].x, pts[j].x);
      const ylo = Math.min(pts[i].y, pts[j].y);
      const yhi = Math.max(pts[i].y, pts[j].y);

      if (rectInsidePolygon(xlo, xhi, ylo, yhi, poly)) {
        const area = (xhi - xlo + 1) * (yhi - ylo + 1);
        if (area > best) best = area;
      }
    }
  }
  return best;
}

const t0 = performance.now();
const pts = loadPoints("input.txt");
const p1 = maxRectAny(pts);
const p2 = maxRectInside(pts, pts);
const elapsed = performance.now() - t0;
console.log(`max_rect_area=${p1} max_green_rect_area=${p2} elapsed_ms=${elapsed.toFixed(3)}`);
