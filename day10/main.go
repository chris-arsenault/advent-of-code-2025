package main

import (
	"fmt"
	"math"
	"os"
	"regexp"
	"strconv"
	"strings"
	"time"
)

type Machine struct {
	pattern string
	buttons [][]int
	targets []int
}

func parseMachine(line string) Machine {
	patRe := regexp.MustCompile(`\[(.*?)\]`)
	buttonRe := regexp.MustCompile(`\(([^)]*)\)`)
	targetRe := regexp.MustCompile(`\{([^}]*)\}`)

	pattern := patRe.FindStringSubmatch(line)[1]
	buttonParts := buttonRe.FindAllStringSubmatch(line, -1)
	buttons := [][]int{}
	for _, part := range buttonParts {
		if strings.TrimSpace(part[1]) == "" {
			buttons = append(buttons, []int{})
		} else {
			bits := strings.Split(part[1], ",")
			var bs []int
			for _, b := range bits {
				v, _ := strconv.Atoi(strings.TrimSpace(b))
				bs = append(bs, v)
			}
			buttons = append(buttons, bs)
		}
	}
	tStr := targetRe.FindStringSubmatch(line)[1]
	var targets []int
	for _, t := range strings.Split(tStr, ",") {
		v, _ := strconv.Atoi(strings.TrimSpace(t))
		targets = append(targets, v)
	}
	return Machine{pattern: pattern, buttons: buttons, targets: targets}
}

func loadMachines(path string) []Machine {
	data, _ := os.ReadFile(path)
	lines := strings.Split(string(data), "\n")
	var m []Machine
	for _, l := range lines {
		if strings.TrimSpace(l) == "" {
			continue
		}
		m = append(m, parseMachine(l))
	}
	return m
}

func part1(ms []Machine) int {
	total := 0
	for _, m := range ms {
		lights := len(m.pattern)
		target := 0
		for i, ch := range m.pattern {
			if ch == '#' {
				target |= 1 << i
			}
		}
		buttonMasks := make([]int, len(m.buttons))
		for i, b := range m.buttons {
			mask := 0
			for _, idx := range b {
				if idx < lights {
					mask ^= 1 << idx
				}
			}
			buttonMasks[i] = mask
		}
		best := -1
		n := len(buttonMasks)
		for mask := 0; mask < (1 << n); mask++ {
			state := 0
			presses := 0
			for i := 0; i < n; i++ {
				if mask>>i&1 == 1 {
					state ^= buttonMasks[i]
					presses++
				}
			}
			if state == target {
				if best == -1 || presses < best {
					best = presses
				}
			}
		}
		total += best
	}
	return total
}

func part2(ms []Machine) int {
	total := 0
	const EPS = 1e-9

	rref := func(mat [][]float64, rhs []float64, cols int) (int, []int, bool) {
		rows := len(mat)
		pivot := make([]int, rows)
		for i := range pivot {
			pivot[i] = -1
		}
		row := 0
		for col := 0; col < cols && row < rows; col++ {
			pivotRow := -1
			best := 0.0
			for r := row; r < rows; r++ {
				val := math.Abs(mat[r][col])
				if val > EPS && val > best {
					best = val
					pivotRow = r
				}
			}
			if pivotRow == -1 {
				continue
			}
			if pivotRow != row {
				mat[row], mat[pivotRow] = mat[pivotRow], mat[row]
				rhs[row], rhs[pivotRow] = rhs[pivotRow], rhs[row]
			}
			piv := mat[row][col]
			for c := col; c < cols; c++ {
				mat[row][c] /= piv
			}
			rhs[row] /= piv
			for r := 0; r < rows; r++ {
				if r == row {
					continue
				}
				f := mat[r][col]
				if math.Abs(f) < EPS {
					continue
				}
				for c := col; c < cols; c++ {
					mat[r][c] -= f * mat[row][c]
				}
				rhs[r] -= f * rhs[row]
			}
			pivot[row] = col
			row++
		}
		rank := row
		for r := rank; r < len(mat); r++ {
			maxv := 0.0
			for c := 0; c < cols; c++ {
				if v := math.Abs(mat[r][c]); v > maxv {
					maxv = v
				}
			}
			if maxv < EPS && math.Abs(rhs[r]) > EPS {
				return rank, pivot, true
			}
		}
		return rank, pivot, false
	}

	for _, m := range ms {
		counters := len(m.targets)
		buttons := len(m.buttons)
		if counters == 0 || buttons == 0 {
			continue
		}

		mat := make([][]float64, counters)
		for i := 0; i < counters; i++ {
			mat[i] = make([]float64, buttons)
		}
		for cidx, b := range m.buttons {
			for _, t := range b {
				if t >= 0 && t < counters {
					mat[t][cidx] = 1.0
				}
			}
		}

		rhs := make([]float64, counters)
		for i, v := range m.targets {
			rhs[i] = float64(v)
		}

		rank, pivotCols, bad := rref(mat, rhs, buttons)
		if bad {
			continue
		}

		used := make([]bool, buttons)
		for i := 0; i < rank; i++ {
			if pivotCols[i] >= 0 {
				used[pivotCols[i]] = true
			}
		}
		freeCols := []int{}
		for c := 0; c < buttons; c++ {
			if !used[c] {
				freeCols = append(freeCols, c)
			}
		}

		freeCount := len(freeCols)
		coef := make([][]float64, rank)
		for r := 0; r < rank; r++ {
			coef[r] = make([]float64, freeCount)
			for f := 0; f < freeCount; f++ {
				coef[r][f] = mat[r][freeCols[f]]
			}
		}

		sumTargets := 0
		for _, v := range m.targets {
			sumTargets += v
		}
		best := sumTargets
		freeVals := make([]int, freeCount)
		bounds := make([]int, freeCount)
		for f := 0; f < freeCount; f++ {
			bounds[f] = best
			for r := 0; r < rank; r++ {
				if coef[r][f] > EPS {
					limit := int(math.Floor(rhs[r]/coef[r][f] + EPS))
					if limit < bounds[f] {
						bounds[f] = limit
					}
				}
			}
		}

		evaluate := func(cur int) {
			if cur >= best {
				return
			}
			sum := cur
			for r := 0; r < rank; r++ {
				v := rhs[r]
				for f := 0; f < freeCount; f++ {
					v -= coef[r][f] * float64(freeVals[f])
				}
				if v < -EPS {
					return
				}
				iv := int(math.Round(v))
				if math.Abs(float64(iv)-v) > EPS {
					return
				}
				sum += iv
				if sum >= best {
					return
				}
			}
			if sum < best {
				best = sum
			}
		}

		evaluate(0)

		var quick func(idx, cur, cap int)
		quick = func(idx, cur, cap int) {
			if cur >= best {
				return
			}
			if idx == freeCount {
				evaluate(cur)
				return
			}
			lim := cap
			if bounds[idx] < lim {
				lim = bounds[idx]
			}
			if lim < 0 {
				return
			}
			for v := 0; v <= lim; v++ {
				freeVals[idx] = v
				quick(idx+1, cur+v, cap)
			}
		}

		seedCap := 400
		if best < seedCap {
			seedCap = best
		}
		if freeCount > 0 && seedCap > 0 {
			quick(0, 0, seedCap)
		}

		var dfs func(idx, cur int)
		dfs = func(idx, cur int) {
			if cur >= best {
				return
			}
			if idx == freeCount {
				evaluate(cur)
				return
			}
			maxv := best - cur
			if bounds[idx] < maxv {
				maxv = bounds[idx]
			}
			if maxv < 0 {
				return
			}
			for v := 0; v <= maxv; v++ {
				freeVals[idx] = v
				dfs(idx+1, cur+v)
			}
		}

		if freeCount > 0 {
			dfs(0, 0)
		} else {
			evaluate(0)
		}
		if best < math.MaxInt64 {
			total += best
		}
	}
	return total
}

func main() {
	ms := loadMachines("input.txt")
	t0 := time.Now()
	p1 := part1(ms)
	p2 := part2(ms)
	elapsed := time.Since(t0).Seconds() * 1000
	fmt.Printf("min_lights_presses=%d min_counter_presses=%d elapsed_ms=%.3f\n", p1, p2, elapsed)
}
