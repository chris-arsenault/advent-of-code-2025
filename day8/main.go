package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"time"
)

type Edge struct {
	dist int64
	a    int
	b    int
}

type DSU struct {
	p []int
	s []int
}

func NewDSU(n int) *DSU {
	p := make([]int, n)
	s := make([]int, n)
	for i := 0; i < n; i++ {
		p[i] = i
		s[i] = 1
	}
	return &DSU{p: p, s: s}
}

func (d *DSU) find(x int) int {
	for d.p[x] != x {
		d.p[x] = d.p[d.p[x]]
		x = d.p[x]
	}
	return x
}

func (d *DSU) unite(a, b int) bool {
	ra := d.find(a)
	rb := d.find(b)
	if ra == rb {
		return false
	}
	if d.s[ra] < d.s[rb] {
		ra, rb = rb, ra
	}
	d.p[rb] = ra
	d.s[ra] += d.s[rb]
	return true
}

func part1(n int, edges []Edge) uint64 {
	dsu := NewDSU(n)
	limit := len(edges)
	if limit > 1000 {
		limit = 1000
	}
	for i := 0; i < limit; i++ {
		dsu.unite(edges[i].a, edges[i].b)
	}
	var comps []uint64
	for i := 0; i < n; i++ {
		if dsu.find(i) == i {
			comps = append(comps, uint64(dsu.s[i]))
		}
	}
	sort.Slice(comps, func(i, j int) bool { return comps[i] > comps[j] })
	for len(comps) < 3 {
		comps = append(comps, 1)
	}
	return comps[0] * comps[1] * comps[2]
}

func part2(xs []int, edges []Edge, n int) uint64 {
	dsu := NewDSU(n)
	var last uint64
	components := n
	for _, e := range edges {
		if dsu.unite(e.a, e.b) {
			components--
			last = uint64(xs[e.a]) * uint64(xs[e.b])
			if components == 1 {
				break
			}
		}
	}
	return last
}

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()
	var xs, ys, zs []int
	sc := bufio.NewScanner(file)
	for sc.Scan() {
		var x, y, z int
		fmt.Sscanf(sc.Text(), "%d,%d,%d", &x, &y, &z)
		xs = append(xs, x)
		ys = append(ys, y)
		zs = append(zs, z)
	}
	n := len(xs)
	var edges []Edge
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			dx := xs[i] - xs[j]
			dy := ys[i] - ys[j]
			dz := zs[i] - zs[j]
			d2 := int64(dx*dx + dy*dy + dz*dz)
			edges = append(edges, Edge{dist: d2, a: i, b: j})
		}
	}
	sort.Slice(edges, func(i, j int) bool { return edges[i].dist < edges[j].dist })
	t0 := time.Now()
	p1 := part1(n, edges)
	p2 := part2(xs, edges, n)
	elapsed := time.Since(t0).Seconds() * 1000
	fmt.Printf("top3_product=%d final_join_x_product=%d elapsed_ms=%.3f\n", p1, p2, elapsed)
}
