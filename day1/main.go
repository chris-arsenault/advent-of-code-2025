package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"time"
)

func simulate(lines []string) (int, int, int) {
	pos := 50
	zeroHits := 0
	crossings := 0

	for _, raw := range lines {
		line := strings.TrimSpace(raw)
		if line == "" {
			continue
		}
		sign := -1
		if line[0] == 'R' {
			sign = 1
		}
		var mag int
		fmt.Sscanf(line[1:], "%d", &mag)

		first := pos
		if sign == 1 {
			first = 100 - pos
		}
		if first == 0 {
			first = 100
		}
		if mag >= first {
			crossings += 1 + (mag-first)/100
		}

		pos = (pos + sign*mag) % 100
		if pos < 0 {
			pos += 100
		}
		if pos == 0 {
			zeroHits++
		}
	}
	return zeroHits, crossings, pos
}

func main() {
	start := time.Now()
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	zeroHits, crossings, pos := simulate(lines)

	elapsed := time.Since(start).Seconds() * 1000
	fmt.Printf("zero_landings=%d crossings=%d final_pos=%d elapsed_ms=%.3f\n", zeroHits, crossings, pos, elapsed)
}
