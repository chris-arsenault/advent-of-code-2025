package main

import (
	"fmt"
	"math"
	"os"
	"sort"
	"strconv"
	"strings"
	"time"
)

func generateEvenHalf(maxN uint64) []uint64 {
	var vals []uint64
	maxLen := len(fmt.Sprintf("%d", maxN))
	for halfLen := 1; halfLen <= maxLen/2; halfLen++ {
		start := uint64(math.Pow10(halfLen - 1))
		end := uint64(math.Pow10(halfLen))
		for t := start; t < end; t++ {
			n := t*uint64(math.Pow10(halfLen)) + t
			if n > maxN {
				break
			}
			vals = append(vals, n)
		}
	}
	sort.Slice(vals, func(i, j int) bool { return vals[i] < vals[j] })
	return vals
}

func generatePeriodic(maxN uint64) []uint64 {
	seen := make(map[uint64]bool)
	maxLen := len(fmt.Sprintf("%d", maxN))
	for baseLen := 1; baseLen <= (maxLen+1)/2; baseLen++ {
		start := uint64(math.Pow10(baseLen - 1))
		end := uint64(math.Pow10(baseLen))
		for base := start; base < end; base++ {
			baseStr := fmt.Sprintf("%d", base)
			for rep := 2; baseLen*rep <= maxLen; rep++ {
				nStr := strings.Repeat(baseStr, rep)
				n, _ := strconv.ParseUint(nStr, 10, 64)
				if n > maxN {
					break
				}
				seen[n] = true
			}
		}
	}
	vals := make([]uint64, 0, len(seen))
	for v := range seen {
		vals = append(vals, v)
	}
	sort.Slice(vals, func(i, j int) bool { return vals[i] < vals[j] })
	return vals
}

func prefixSums(nums []uint64) []uint64 {
	ps := make([]uint64, len(nums)+1)
	for i, v := range nums {
		ps[i+1] = ps[i] + v
	}
	return ps
}

func rangeSum(nums []uint64, ps []uint64, lo, hi uint64) uint64 {
	i := sort.Search(len(nums), func(i int) bool { return nums[i] >= lo })
	j := sort.Search(len(nums), func(j int) bool { return nums[j] > hi })
	return ps[j] - ps[i]
}

func parseRanges(text string) [][2]uint64 {
	var ranges [][2]uint64
	parts := strings.FieldsFunc(text, func(r rune) bool { return r == ',' || r == '\n' })
	for _, part := range parts {
		if strings.TrimSpace(part) == "" {
			continue
		}
		bits := strings.Split(part, "-")
		a, _ := strconv.ParseUint(bits[0], 10, 64)
		b, _ := strconv.ParseUint(bits[1], 10, 64)
		ranges = append(ranges, [2]uint64{a, b})
	}
	return ranges
}

func solve(text string) (uint64, uint64) {
	ranges := parseRanges(text)
	var maxN uint64
	for _, r := range ranges {
		if r[1] > maxN {
			maxN = r[1]
		}
	}

	evens := generateEvenHalf(maxN)
	evensPS := prefixSums(evens)
	per := generatePeriodic(maxN)
	perPS := prefixSums(per)

	var p1, p2 uint64
	for _, r := range ranges {
		p1 += rangeSum(evens, evensPS, r[0], r[1])
		p2 += rangeSum(per, perPS, r[0], r[1])
	}
	return p1, p2
}

func main() {
	data, _ := os.ReadFile("input.txt")
	text := string(data)
	start := time.Now()
	p1, p2 := solve(text)
	elapsed := time.Since(start).Seconds() * 1000
	fmt.Printf("repeated-halves-sum=%d repeated-pattern-sum=%d elapsed_ms=%.3f\n", p1, p2, elapsed)
}
