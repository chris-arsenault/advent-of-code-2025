package main

import (
	"bufio"
	"fmt"
	"os"
	"time"
)

type Grid struct {
	rows []string
	sr   int
	sc   int
}

func loadGrid(path string) Grid {
	file, _ := os.Open(path)
	defer file.Close()
	var rows []string
	sr, sc := -1, -1
	scanner := bufio.NewScanner(file)
	r := 0
	for scanner.Scan() {
		line := scanner.Text()
		rows = append(rows, line)
		if sr == -1 {
			if idx := indexRune(line, 'S'); idx != -1 {
				sr = r
				sc = idx
			}
		}
		r++
	}
	return Grid{rows: rows, sr: sr, sc: sc}
}

func indexRune(s string, ch rune) int {
	for i, r := range s {
		if r == ch {
			return i
		}
	}
	return -1
}

func part1(g Grid) int {
	h := len(g.rows)
	w := len(g.rows[0])
	active := map[int]bool{g.sc: true}
	splits := 0
	for r := g.sr; r < h; r++ {
		next := make(map[int]bool)
		seen := make(map[int]bool)
		queue := make([]int, 0)
		for c := range active {
			queue = append(queue, c)
		}
		for len(queue) > 0 {
			c := queue[0]
			queue = queue[1:]
			if seen[c] {
				continue
			}
			seen[c] = true
			cell := g.rows[r][c]
			if cell == '^' {
				splits++
				if c > 0 {
					queue = append(queue, c-1)
				}
				if c+1 < w {
					queue = append(queue, c+1)
				}
			} else {
				next[c] = true
			}
		}
		active = next
		if len(active) == 0 {
			break
		}
	}
	return splits
}

func part2(g Grid) uint64 {
	h := len(g.rows)
	w := len(g.rows[0])
	active := map[int]uint64{g.sc: 1}
	for r := g.sr; r < h; r++ {
		next := make(map[int]uint64)
		for c, cnt := range active {
			cell := g.rows[r][c]
			if cell == '^' {
				if c > 0 {
					next[c-1] += cnt
				}
				if c+1 < w {
					next[c+1] += cnt
				}
			} else {
				next[c] += cnt
			}
		}
		active = next
		if len(active) == 0 {
			break
		}
	}
	var timelines uint64
	for _, v := range active {
		timelines += v
	}
	return timelines
}

func main() {
	g := loadGrid("input.txt")
	start := time.Now()
	p1 := part1(g)
	p2 := part2(g)
	elapsed := time.Since(start).Seconds() * 1000
	fmt.Printf("splits=%d timelines=%d elapsed_ms=%.3f\n", p1, p2, elapsed)
}
