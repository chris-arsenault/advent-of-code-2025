package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"time"
)

func loadGraph(path string) map[string][]string {
	f, _ := os.Open(path)
	defer f.Close()
	g := make(map[string][]string)
	sc := bufio.NewScanner(f)
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" {
			continue
		}
		parts := strings.SplitN(line, ":", 2)
		from := strings.TrimSpace(parts[0])
		rest := strings.Fields(parts[1])
		g[from] = rest
	}
	return g
}

func countPaths(g map[string][]string, start, target string, memo map[string]uint64) uint64 {
	if v, ok := memo[start]; ok {
		return v
	}
	if start == target {
		memo[start] = 1
		return 1
	}
	var total uint64
	for _, nxt := range g[start] {
		total += countPaths(g, nxt, target, memo)
	}
	memo[start] = total
	return total
}

func main() {
	g := loadGraph("input.txt")
	t0 := time.Now()
	p1 := countPaths(g, "you", "out", make(map[string]uint64))
	a1 := countPaths(g, "svr", "dac", make(map[string]uint64))
	a2 := countPaths(g, "dac", "fft", make(map[string]uint64))
	a3 := countPaths(g, "fft", "out", make(map[string]uint64))
	b1 := countPaths(g, "svr", "fft", make(map[string]uint64))
	b2 := countPaths(g, "fft", "dac", make(map[string]uint64))
	b3 := countPaths(g, "dac", "out", make(map[string]uint64))
	p2 := a1*a2*a3 + b1*b2*b3
	elapsed := time.Since(t0).Seconds() * 1000
	fmt.Printf("paths_you_to_out=%d paths_svr_via_dac_fft=%d elapsed_ms=%.3f\n", p1, p2, elapsed)
}
