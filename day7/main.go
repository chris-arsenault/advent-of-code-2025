package main

import (
	"bufio"
	"fmt"
	"os"
	"sync"
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

type beamResult struct {
	splits int
	next   []int
}

func part1(g Grid) int {
	h := len(g.rows)
	w := len(g.rows[0])
	active := map[int]bool{g.sc: true}
	totalSplits := 0

	for r := g.sr; r < h; r++ {
		results := make(chan beamResult, len(active))
		var wg sync.WaitGroup

		for c := range active {
			wg.Add(1)
			go func(col int) {
				defer wg.Done()
				var nextCols []int
				splits := 0
				seen := map[int]bool{}
				queue := []int{col}
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
						nextCols = append(nextCols, c)
					}
				}
				results <- beamResult{splits: splits, next: nextCols}
			}(c)
		}

		go func() {
			wg.Wait()
			close(results)
		}()

		next := make(map[int]bool)
		for res := range results {
			totalSplits += res.splits
			for _, c := range res.next {
				next[c] = true
			}
		}
		active = next
		if len(active) == 0 {
			break
		}
	}
	return totalSplits
}

func part2(g Grid) uint64 {
	h := len(g.rows)
	w := len(g.rows[0])
	active := map[int]uint64{g.sc: 1}

	type countResult struct {
		updates map[int]uint64
	}

	for r := g.sr; r < h; r++ {
		results := make(chan countResult, len(active))
		var wg sync.WaitGroup

		for c, cnt := range active {
			wg.Add(1)
			go func(col int, count uint64) {
				defer wg.Done()
				upd := make(map[int]uint64)
				cell := g.rows[r][col]
				if cell == '^' {
					if col > 0 {
						upd[col-1] = count
					}
					if col+1 < w {
						upd[col+1] = count
					}
				} else {
					upd[col] = count
				}
				results <- countResult{updates: upd}
			}(c, cnt)
		}

		go func() {
			wg.Wait()
			close(results)
		}()

		next := make(map[int]uint64)
		for res := range results {
			for c, cnt := range res.updates {
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
	start := time.Now()
	g := loadGrid("input.txt")
	p1 := part1(g)
	p2 := part2(g)
	elapsed := time.Since(start).Seconds() * 1000
	fmt.Printf("splits=%d timelines=%d elapsed_ms=%.3f\n", p1, p2, elapsed)
}
