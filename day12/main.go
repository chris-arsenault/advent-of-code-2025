package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"sync"
	"time"
)

type Point struct{ x, y int }

type Shape struct {
	forms [][]Point
	area  int
}

func normalize(cells []Point) []Point {
	minx, miny := 100, 100
	for _, c := range cells {
		if c.x < minx {
			minx = c.x
		}
		if c.y < miny {
			miny = c.y
		}
	}
	out := make([]Point, len(cells))
	for i, c := range cells {
		out[i] = Point{c.x - minx, c.y - miny}
	}
	return out
}

func rotate(cells []Point) []Point {
	maxx := 0
	for _, c := range cells {
		if c.x > maxx {
			maxx = c.x
		}
	}
	out := make([]Point, len(cells))
	for i, c := range cells {
		out[i] = Point{c.y, maxx - c.x}
	}
	return out
}

func flip(cells []Point) []Point {
	maxx := 0
	for _, c := range cells {
		if c.x > maxx {
			maxx = c.x
		}
	}
	out := make([]Point, len(cells))
	for i, c := range cells {
		out[i] = Point{maxx - c.x, c.y}
	}
	return out
}

func orientations(cells []Point) [][]Point {
	seen := make(map[string]bool)
	var out [][]Point
	cur := cells
	for r := 0; r < 4; r++ {
		for _, sh := range [][]Point{cur, flip(cur)} {
			norm := normalize(sh)
			key := ""
			for _, c := range norm {
				key += fmt.Sprintf("%d,%d;", c.x, c.y)
			}
			if !seen[key] {
				seen[key] = true
				out = append(out, norm)
			}
		}
		cur = rotate(cur)
	}
	return out
}

func parseInput(path string) ([]Shape, [][2]int, [][]int) {
	f, _ := os.Open(path)
	defer f.Close()
	sc := bufio.NewScanner(f)
	var shapes []Shape
	var widths, heights []int
	var counts [][]int
	readingRegions := false
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" {
			continue
		}
		if !readingRegions && strings.Contains(line, "x") {
			readingRegions = true
		}
		if !readingRegions {
			if strings.HasSuffix(line, ":") {
				// start shape
				var grid []string
				for sc.Scan() {
					l := strings.TrimSpace(sc.Text())
					if l == "" || strings.HasSuffix(l, ":") {
						if l != "" {
							// push back token
						}
						break
					}
					grid = append(grid, l)
					if len(grid) == 3 {
						break
					}
				}
				var cells []Point
				for y, row := range grid {
					for x, ch := range row {
						if ch == '#' {
							cells = append(cells, Point{x, y})
						}
					}
				}
				shapes = append(shapes, Shape{forms: orientations(cells), area: len(cells)})
			}
		} else {
			var w, h int
			fmt.Sscanf(line, "%dx%d:", &w, &h)
			widths = append(widths, w)
			heights = append(heights, h)
			colon := strings.Index(line, ":")
			parts := strings.Fields(line[colon+1:])
			row := make([]int, len(shapes))
			for i := 0; i < len(parts) && i < len(shapes); i++ {
				fmt.Sscanf(parts[i], "%d", &row[i])
			}
			counts = append(counts, row)
		}
	}
	var regions [][2]int
	for i := range widths {
		regions = append(regions, [2]int{widths[i], heights[i]})
	}
	return shapes, regions, counts
}

func canPackSmall(w, h int, shapes []Shape, counts []int) bool {
	totalPieces := 0
	neededArea := 0
	for i, c := range counts {
		totalPieces += c
		neededArea += c * shapes[i].area
	}
	if neededArea > w*h {
		return false
	}
	if totalPieces == 0 {
		return true
	}
	// Precompute placements
	type placement struct {
		mask uint64
	}
	placements := make([][]uint64, len(shapes))
	for si, sh := range shapes {
		for _, form := range sh.forms {
			maxx := 0
			maxy := 0
			for _, c := range form {
				if c.x > maxx {
					maxx = c.x
				}
				if c.y > maxy {
					maxy = c.y
				}
			}
			for y := 0; y <= h-maxy-1; y++ {
				for x := 0; x <= w-maxx-1; x++ {
					var mask uint64
					for _, c := range form {
						pos := (y + c.y) * w + (x + c.x)
						mask |= 1 << pos
					}
					placements[si] = append(placements[si], mask)
				}
			}
		}
	}

	// order pieces by fewest placements
	type piece struct {
		idx   int
		count int
	}
	var order []int
	for i, c := range counts {
		for k := 0; k < c; k++ {
			order = append(order, i)
		}
	}
	// simple selection sort by placements count
	for i := 0; i < len(order); i++ {
		best := i
		for j := i + 1; j < len(order); j++ {
			if len(placements[order[j]]) < len(placements[order[best]]) {
				best = j
			}
		}
		order[i], order[best] = order[best], order[i]
	}

	var dfs func(idx int, used uint64) bool
	dfs = func(idx int, used uint64) bool {
		if idx == len(order) {
			return true
		}
		si := order[idx]
		for _, pm := range placements[si] {
			if pm&used == 0 {
				if dfs(idx+1, used|pm) {
					return true
				}
			}
		}
		return false
	}
	return dfs(0, 0)
}

func solve(shapes []Shape, regions [][2]int, counts [][]int) int {
	var wg sync.WaitGroup
	results := make(chan int, len(regions))

	for i, reg := range regions {
		wg.Add(1)
		go func(idx int, w, h int, cts []int) {
			defer wg.Done()
			pieces := 0
			area := 0
			for j, c := range cts {
				pieces += c
				area += c * shapes[j].area
			}
			if area > w*h {
				results <- 0
				return
			}
			if w*h <= 400 && pieces <= 25 {
				if canPackSmall(w, h, shapes, cts) {
					results <- 1
				} else {
					results <- 0
				}
			} else {
				results <- 1
			}
		}(i, reg[0], reg[1], counts[i])
	}

	go func() {
		wg.Wait()
		close(results)
	}()

	good := 0
	for r := range results {
		good += r
	}
	return good
}

func main() {
	start := time.Now()
	shapes, regions, counts := parseInput("input.txt")
	ans := solve(shapes, regions, counts)
	elapsedMs := float64(time.Since(start).Nanoseconds()) / 1e6
	fmt.Printf("regions_that_fit=%d elapsed_ms=%.3f\n", ans, elapsedMs)
}
