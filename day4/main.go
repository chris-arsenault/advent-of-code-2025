package main

import (
	"bufio"
	"fmt"
	"os"
	"time"
)

type Point struct{ r, c int }

var dirs = []Point{
	{-1, -1}, {-1, 0}, {-1, 1},
	{0, -1}, {0, 1},
	{1, -1}, {1, 0}, {1, 1},
}

func parseGrid(lines []string) map[Point]bool {
	rolls := make(map[Point]bool)
	for r, line := range lines {
		for c, ch := range line {
			if ch == '@' {
				rolls[Point{r, c}] = true
			}
		}
	}
	return rolls
}

func neighborCounts(rolls map[Point]bool) map[Point]int {
	counts := make(map[Point]int)
	for p := range rolls {
		cnt := 0
		for _, d := range dirs {
			if rolls[Point{p.r + d.r, p.c + d.c}] {
				cnt++
			}
		}
		counts[p] = cnt
	}
	return counts
}

func part1(counts map[Point]int) int {
	acc := 0
	for _, v := range counts {
		if v < 4 {
			acc++
		}
	}
	return acc
}

func part2(rolls map[Point]bool, counts map[Point]int) int {
	type queueItem Point
	q := make([]queueItem, 0)
	inQ := make(map[Point]bool)
	for p, c := range counts {
		if c < 4 {
			q = append(q, queueItem(p))
			inQ[p] = true
		}
	}
	removed := 0
	for len(q) > 0 {
		p := q[0]
		q = q[1:]
		pt := Point(p)
		if !rolls[pt] {
			continue
		}
		delete(rolls, pt)
		removed++
		for _, d := range dirs {
			np := Point{pt.r + d.r, pt.c + d.c}
			if _, ok := rolls[np]; ok {
				counts[np]--
				if counts[np] < 4 && !inQ[np] {
					q = append(q, queueItem(np))
					inQ[np] = true
				}
			}
		}
	}
	return removed
}

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()
	var lines []string
	sc := bufio.NewScanner(file)
	for sc.Scan() {
		lines = append(lines, sc.Text())
	}

	rolls := parseGrid(lines)
	counts := neighborCounts(rolls)

	start := time.Now()
	a := part1(counts)
	removed := part2(rolls, counts)
	elapsed := time.Since(start).Seconds() * 1000
	fmt.Printf("accessible=%d removable_total=%d elapsed_ms=%.3f\n", a, removed, elapsed)
}
