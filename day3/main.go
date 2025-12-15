package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"
)

func bestTwo(line string) int64 {
	d := []byte(strings.TrimSpace(line))
	n := len(d)
	if n < 2 {
		return 0
	}
	suffix := make([]byte, n+1)
	for i := n - 1; i >= 0; i-- {
		if d[i] > suffix[i+1] {
			suffix[i] = d[i]
		} else {
			suffix[i] = suffix[i+1]
		}
	}
	best := int64(-1)
	for i := 0; i < n-1; i++ {
		val := int64(d[i]-'0')*10 + int64(suffix[i+1]-'0')
		if val > best {
			best = val
		}
	}
	return best
}

func bestK(line string, k int) int64 {
	digits := []byte(strings.TrimSpace(line))
	drop := len(digits) - k
	stack := make([]byte, 0, k)
	for _, ch := range digits {
		for drop > 0 && len(stack) > 0 && stack[len(stack)-1] < ch {
			stack = stack[:len(stack)-1]
			drop--
		}
		stack = append(stack, ch)
	}
	stack = stack[:k]
	var val int64
	for _, ch := range stack {
		val = val*10 + int64(ch-'0')
	}
	return val
}

func main() {
	base := "input_eric.txt"
	if _, err := os.Stat(base); os.IsNotExist(err) {
		base = "input.txt"
	}
	start := time.Now()
	file, _ := os.Open(filepath.Join(base))
	defer file.Close()

	var lines []string
	sc := bufio.NewScanner(file)
	for sc.Scan() {
		lines = append(lines, sc.Text())
	}

	var p1 int64
	var p2 int64
	for _, line := range lines {
		p1 += bestTwo(line)
		p2 += bestK(line, 12)
	}
	elapsed := time.Since(start).Seconds() * 1000
	fmt.Printf("max-2-digit-sum=%d max-12-digit-sum=%d elapsed_ms=%.3f\n", p1, p2, elapsed)
}
