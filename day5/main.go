package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
	"time"
)

func mergeRanges(ranges [][2]int64) [][2]int64 {
	sort.Slice(ranges, func(i, j int) bool {
		if ranges[i][0] == ranges[j][0] {
			return ranges[i][1] < ranges[j][1]
		}
		return ranges[i][0] < ranges[j][0]
	})
	merged := make([][2]int64, 0)
	for _, r := range ranges {
		if len(merged) == 0 || r[0] > merged[len(merged)-1][1]+1 {
			merged = append(merged, r)
		} else {
			if r[1] > merged[len(merged)-1][1] {
				merged[len(merged)-1][1] = r[1]
			}
		}
	}
	return merged
}

func inAny(ranges [][2]int64, x int64) bool {
	i := sort.Search(len(ranges), func(i int) bool { return ranges[i][0] > x })
	if i > 0 && ranges[i-1][0] <= x && x <= ranges[i-1][1] {
		return true
	}
	return false
}

func parse(path string) ([][2]int64, []int64) {
	data, _ := os.ReadFile(path)
	text := strings.ReplaceAll(string(data), "\r\n", "\n")
	parts := strings.SplitN(text, "\n\n", 2)
	if len(parts) < 2 {
		parts = append(parts, "")
	}
	var ranges [][2]int64
	sc := bufio.NewScanner(strings.NewReader(parts[0]))
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" {
			continue
		}
		bits := strings.Split(line, "-")
		a, _ := strconv.ParseInt(bits[0], 10, 64)
		b, _ := strconv.ParseInt(bits[1], 10, 64)
		ranges = append(ranges, [2]int64{a, b})
	}
	var ids []int64
	sc = bufio.NewScanner(strings.NewReader(parts[1]))
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" {
			continue
		}
		v, _ := strconv.ParseInt(line, 10, 64)
		ids = append(ids, v)
	}
	return ranges, ids
}

func solve(path string) (int, int64) {
	ranges, ids := parse(path)
	merged := mergeRanges(ranges)
	fresh := 0
	for _, id := range ids {
		if inAny(merged, id) {
			fresh++
		}
	}
	var total int64
	for _, r := range merged {
		total += r[1] - r[0] + 1
	}
	return fresh, total
}

func main() {
	start := time.Now()
	p1, p2 := solve("input.txt")
	elapsed := time.Since(start).Seconds() * 1000
	fmt.Printf("available_fresh=%d total_fresh_ids=%d elapsed_ms=%.3f\n", p1, p2, elapsed)
}
