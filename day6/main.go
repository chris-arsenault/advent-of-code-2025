package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"time"
)

func loadGrid(path string) []string {
	file, _ := os.Open(path)
	defer file.Close()
	var lines []string
	sc := bufio.NewScanner(file)
	for sc.Scan() {
		lines = append(lines, sc.Text())
	}
	maxw := 0
	for _, l := range lines {
		if len(l) > maxw {
			maxw = len(l)
		}
	}
	for i, l := range lines {
		lines[i] = l + strings.Repeat(" ", maxw-len(l))
	}
	return lines
}

func splitBlocks(grid []string) [][2]int {
	h := len(grid)
	w := len(grid[0])
	emptyCol := make([]bool, w)
	for c := 0; c < w; c++ {
		ok := true
		for r := 0; r < h; r++ {
			if grid[r][c] != ' ' {
				ok = false
				break
			}
		}
		emptyCol[c] = ok
	}
	var blocks [][2]int
	c := 0
	for c < w {
		for c < w && emptyCol[c] {
			c++
		}
		if c >= w {
			break
		}
		start := c
		for c < w && !emptyCol[c] {
			c++
		}
		blocks = append(blocks, [2]int{start, c})
	}
	return blocks
}

func problemOp(col string) byte {
	for i := 0; i < len(col); i++ {
		if col[i] == '+' || col[i] == '*' {
			return col[i]
		}
	}
	return '+'
}

func part1(grid []string, blocks [][2]int) int64 {
	opRow := grid[len(grid)-1]
	var total int64
	for _, b := range blocks {
		op := problemOp(opRow[b[0]:b[1]])
		var nums []int64
		for _, row := range grid[:len(grid)-1] {
			tok := strings.TrimSpace(row[b[0]:b[1]])
			if tok != "" {
				var v int64
				fmt.Sscanf(tok, "%d", &v)
				nums = append(nums, v)
			}
		}
		acc := int64(0)
		if op == '*' {
			acc = 1
		}
		for _, n := range nums {
			if op == '+' {
				acc += n
			} else {
				acc *= n
			}
		}
		total += acc
	}
	return total
}

func part2(grid []string, blocks [][2]int) int64 {
	h := len(grid) - 1
	opRow := grid[len(grid)-1]
	var total int64
	for _, b := range blocks {
		op := problemOp(opRow[b[0]:b[1]])
		var nums []int64
		for c := b[1] - 1; c >= b[0]; c-- {
			var digits []byte
			for r := 0; r < h; r++ {
				ch := grid[r][c]
				if ch >= '0' && ch <= '9' {
					digits = append(digits, ch)
				}
			}
			if len(digits) > 0 {
				var v int64
				for _, d := range digits {
					v = v*10 + int64(d-'0')
				}
				nums = append(nums, v)
			}
		}
		acc := int64(0)
		if op == '*' {
			acc = 1
		}
		for _, n := range nums {
			if op == '+' {
				acc += n
			} else {
				acc *= n
			}
		}
		total += acc
	}
	return total
}

func main() {
	t0 := time.Now()
	grid := loadGrid("input.txt")
	blocks := splitBlocks(grid)
	p1 := part1(grid, blocks)
	p2 := part2(grid, blocks)
	elapsed := time.Since(t0).Seconds() * 1000
	fmt.Printf("grand_total=%d quantum_total=%d elapsed_ms=%.3f\n", p1, p2, elapsed)
}
