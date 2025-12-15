package main

import (
	"bufio"
	"fmt"
	"os"
	"time"
)

type Point struct{ x, y int64 }

func loadPoints(path string) []Point {
	f, _ := os.Open(path)
	defer f.Close()
	var pts []Point
	sc := bufio.NewScanner(f)
	for sc.Scan() {
		var x, y int64
		fmt.Sscanf(sc.Text(), "%d,%d", &x, &y)
		pts = append(pts, Point{x, y})
	}
	return pts
}

func abs64(x int64) int64 {
	if x < 0 {
		return -x
	}
	return x
}

func min64(a, b int64) int64 {
	if a < b {
		return a
	}
	return b
}

func max64(a, b int64) int64 {
	if a > b {
		return a
	}
	return b
}

func maxRectAny(pts []Point) int64 {
	var best int64 = 0
	for i := 0; i < len(pts); i++ {
		for j := i + 1; j < len(pts); j++ {
			dx := abs64(pts[i].x - pts[j].x)
			dy := abs64(pts[i].y - pts[j].y)
			area := (dx + 1) * (dy + 1)
			if area > best {
				best = area
			}
		}
	}
	return best
}

func pointInside(px, py int64, poly []Point) bool {
	n := len(poly)
	inside := false
	j := n - 1
	for i := 0; i < n; i++ {
		x1, y1 := poly[j].x, poly[j].y
		x2, y2 := poly[i].x, poly[i].y
		// Check if on edge
		if x1 == x2 {
			if px == x1 && py >= min64(y1, y2) && py <= max64(y1, y2) {
				return true
			}
		} else if y1 == y2 {
			if py == y1 && px >= min64(x1, x2) && px <= max64(x1, x2) {
				return true
			}
		}
		// Ray casting
		if (y1 > py) != (y2 > py) {
			xIntersect := (x2-x1)*(py-y1)/(y2-y1) + x1
			if px < xIntersect {
				inside = !inside
			}
		}
		j = i
	}
	return inside
}

func edgeCrossesInterior(xlo, xhi, ylo, yhi, x1, y1, x2, y2 int64) bool {
	if x1 == x2 {
		// Vertical edge
		if x1 <= xlo || x1 >= xhi {
			return false
		}
		ya := min64(y1, y2)
		yb := max64(y1, y2)
		if yb <= ylo || ya >= yhi {
			return false
		}
		return ya < yhi && yb > ylo
	} else if y1 == y2 {
		// Horizontal edge
		if y1 <= ylo || y1 >= yhi {
			return false
		}
		xa := min64(x1, x2)
		xb := max64(x1, x2)
		if xb <= xlo || xa >= xhi {
			return false
		}
		return xa < xhi && xb > xlo
	}
	return false
}

func rectInsidePolygon(xlo, xhi, ylo, yhi int64, poly []Point) bool {
	// Check all 4 corners are inside
	if !pointInside(xlo, ylo, poly) {
		return false
	}
	if !pointInside(xlo, yhi, poly) {
		return false
	}
	if !pointInside(xhi, ylo, poly) {
		return false
	}
	if !pointInside(xhi, yhi, poly) {
		return false
	}
	// Check no polygon edge crosses the interior
	n := len(poly)
	j := n - 1
	for i := 0; i < n; i++ {
		if edgeCrossesInterior(xlo, xhi, ylo, yhi, poly[j].x, poly[j].y, poly[i].x, poly[i].y) {
			return false
		}
		j = i
	}
	return true
}

func maxRectInside(pts []Point, poly []Point) int64 {
	var best int64 = 0
	for i := 0; i < len(pts); i++ {
		for j := i + 1; j < len(pts); j++ {
			if pts[i].x == pts[j].x || pts[i].y == pts[j].y {
				continue
			}
			xlo := min64(pts[i].x, pts[j].x)
			xhi := max64(pts[i].x, pts[j].x)
			ylo := min64(pts[i].y, pts[j].y)
			yhi := max64(pts[i].y, pts[j].y)
			if rectInsidePolygon(xlo, xhi, ylo, yhi, poly) {
				area := (xhi - xlo + 1) * (yhi - ylo + 1)
				if area > best {
					best = area
				}
			}
		}
	}
	return best
}

func main() {
	start := time.Now()
	pts := loadPoints("input.txt")
	poly := pts
	p1 := maxRectAny(pts)
	p2 := maxRectInside(pts, poly)
	elapsed := time.Since(start).Seconds() * 1000
	fmt.Printf("max_rect_area=%d max_green_rect_area=%d elapsed_ms=%.3f\n", p1, p2, elapsed)
}
