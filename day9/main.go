package main

import (
	"bufio"
	"fmt"
	"os"
	"time"
)

type Point struct{ x, y int }

func loadPoints(path string) []Point {
	f, _ := os.Open(path)
	defer f.Close()
	var pts []Point
	sc := bufio.NewScanner(f)
	for sc.Scan() {
		var x, y int
		fmt.Sscanf(sc.Text(), "%d,%d", &x, &y)
		pts = append(pts, Point{x, y})
	}
	return pts
}

func maxRectAny(pts []Point) int {
	best := 0
	for i := 0; i < len(pts); i++ {
		for j := i + 1; j < len(pts); j++ {
			if pts[i].x == pts[j].x || pts[i].y == pts[j].y {
				continue
			}
			area := (pts[i].x - pts[j].x)
			if area < 0 {
				area = -area
			}
			h := (pts[i].y - pts[j].y)
			if h < 0 {
				h = -h
			}
			a := area * h
			if a > best {
				best = a
			}
		}
	}
	return best
}

func pointInPoly(p Point, poly []Point) bool {
	inside := false
	for i, j := 0, len(poly)-1; i < len(poly); j, i = i, i+1 {
		pi := poly[i]
		pj := poly[j]
		if ((pi.y > p.y) != (pj.y > p.y)) &&
			(p.x < (pj.x-pi.x)*(p.y-pi.y)/(pj.y-pi.y)+pi.x) {
			inside = !inside
		}
	}
	return inside
}

func segIntersect(a1, a2, b1, b2 Point) bool {
	orient := func(p, q, r Point) int {
		val := (q.y-p.y)*(r.x-q.x) - (q.x-p.x)*(r.y-q.y)
		if val == 0 {
			return 0
		}
		if val > 0 {
			return 1
		}
		return 2
	}
	onSeg := func(p, q, r Point) bool {
		return q.x <= max(p.x, r.x) && q.x >= min(p.x, r.x) && q.y <= max(p.y, r.y) && q.y >= min(p.y, r.y)
	}
	o1 := orient(a1, a2, b1)
	o2 := orient(a1, a2, b2)
	o3 := orient(b1, b2, a1)
	o4 := orient(b1, b2, a2)

	if o1 != o2 && o3 != o4 {
		return true
	}
	if o1 == 0 && onSeg(a1, b1, a2) {
		return true
	}
	if o2 == 0 && onSeg(a1, b2, a2) {
		return true
	}
	if o3 == 0 && onSeg(b1, a1, b2) {
		return true
	}
	if o4 == 0 && onSeg(b1, a2, b2) {
		return true
	}
	return false
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func rectEdges(x1, y1, x2, y2 int) [4][2]Point {
	return [4][2]Point{
		{{x1, y1}, {x2, y1}},
		{{x2, y1}, {x2, y2}},
		{{x2, y2}, {x1, y2}},
		{{x1, y2}, {x1, y1}},
	}
}

func insidePolygonRect(poly []Point, x1, y1, x2, y2 int) bool {
	corners := []Point{{x1, y1}, {x1, y2}, {x2, y1}, {x2, y2}}
	for _, c := range corners {
		if !pointInPoly(c, poly) && c != poly[0] {
			return false
		}
	}
	edges := rectEdges(x1, y1, x2, y2)
	for i := 0; i < len(poly); i++ {
		p1 := poly[i]
		p2 := poly[(i+1)%len(poly)]
		for _, e := range edges {
			if segIntersect(p1, p2, e[0], e[1]) {
				return false
			}
		}
	}
	return true
}

func maxRectInside(reds []Point, poly []Point) int {
	best := 0
	for i := 0; i < len(reds); i++ {
		for j := i + 1; j < len(reds); j++ {
			if reds[i].x == reds[j].x || reds[i].y == reds[j].y {
				continue
			}
			x1 := min(reds[i].x, reds[j].x)
			x2 := max(reds[i].x, reds[j].x)
			y1 := min(reds[i].y, reds[j].y)
			y2 := max(reds[i].y, reds[j].y)
			if insidePolygonRect(poly, x1, y1, x2, y2) {
				area := (x2 - x1) * (y2 - y1)
				if area > best {
					best = area
				}
			}
		}
	}
	return best
}

func main() {
	reds := loadPoints("input.txt")
	poly := reds
	t0 := time.Now()
	p1 := maxRectAny(reds)
	p2 := maxRectInside(reds, poly)
	elapsed := time.Since(t0).Seconds() * 1000
	fmt.Printf("max_rect_area=%d max_green_rect_area=%d elapsed_ms=%.3f\n", p1, p2, elapsed)
}
