package main

import (
	"fmt"
	"math"
	"os"
	"regexp"
	"strconv"
	"strings"
	"time"

	"gonum.org/v1/gonum/mat"
)

var startTime = time.Now()

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

// GF(2) Gaussian elimination for Part 1 using gonum for matrix storage
func gf2Solve(buttons [][]int, target []uint8, lights int) int {
	nButtons := len(buttons)
	if nButtons == 0 {
		allZero := true
		for _, t := range target {
			if t != 0 {
				allZero = false
				break
			}
		}
		if allZero {
			return 0
		}
		return -1
	}

	// Build augmented matrix [A | b] over GF(2) using gonum for storage
	augData := make([]float64, lights*(nButtons+1))
	for bIdx, btn := range buttons {
		for _, light := range btn {
			if light < lights {
				augData[light*(nButtons+1)+bIdx] = 1
			}
		}
	}
	for i, t := range target {
		augData[i*(nButtons+1)+nButtons] = float64(t)
	}
	aug := mat.NewDense(lights, nButtons+1, augData)

	// Gaussian elimination over GF(2)
	pivotCols := []int{}
	row := 0
	for col := 0; col < nButtons; col++ {
		pivot := -1
		for r := row; r < lights; r++ {
			if int(aug.At(r, col))%2 == 1 {
				pivot = r
				break
			}
		}
		if pivot == -1 {
			continue
		}
		if pivot != row {
			for c := 0; c <= nButtons; c++ {
				v1, v2 := aug.At(row, c), aug.At(pivot, c)
				aug.Set(row, c, v2)
				aug.Set(pivot, c, v1)
			}
		}
		pivotCols = append(pivotCols, col)
		for r := 0; r < lights; r++ {
			if r != row && int(aug.At(r, col))%2 == 1 {
				for c := 0; c <= nButtons; c++ {
					aug.Set(r, c, float64((int(aug.At(r, c))+int(aug.At(row, c)))%2))
				}
			}
		}
		row++
	}

	rank := len(pivotCols)

	for r := rank; r < lights; r++ {
		if int(aug.At(r, nButtons))%2 == 1 {
			return -1
		}
	}

	pivotSet := make(map[int]bool)
	for _, pc := range pivotCols {
		pivotSet[pc] = true
	}
	freeCols := []int{}
	for c := 0; c < nButtons; c++ {
		if !pivotSet[c] {
			freeCols = append(freeCols, c)
		}
	}
	nFree := len(freeCols)

	best := -1
	for mask := 0; mask < (1 << nFree); mask++ {
		sol := make([]int, nButtons)
		for i, fc := range freeCols {
			sol[fc] = (mask >> i) & 1
		}

		for i := rank - 1; i >= 0; i-- {
			pc := pivotCols[i]
			val := int(aug.At(i, nButtons))
			for c := pc + 1; c < nButtons; c++ {
				val ^= int(aug.At(i, c)) * sol[c]
			}
			sol[pc] = val % 2
		}

		weight := 0
		for _, v := range sol {
			weight += v
		}
		if best == -1 || weight < best {
			best = weight
		}
	}

	return best
}

func part1(ms []Machine) int {
	total := 0
	for _, m := range ms {
		lights := len(m.pattern)
		target := make([]uint8, lights)
		for i, ch := range m.pattern {
			if ch == '#' {
				target[i] = 1
			}
		}
		result := gf2Solve(m.buttons, target, lights)
		if result >= 0 {
			total += result
		}
	}
	return total
}

// Part 2 using gonum for matrix storage and float64 RREF with DFS search
func part2(ms []Machine) int {
	total := 0
	const EPS = 1e-9

	for _, m := range ms {
		counters := len(m.targets)
		buttons := len(m.buttons)
		if counters == 0 || buttons == 0 {
			continue
		}

		// Build matrix using gonum
		data := make([]float64, counters*buttons)
		for cidx, b := range m.buttons {
			for _, t := range b {
				if t >= 0 && t < counters {
					data[t*buttons+cidx] = 1.0
				}
			}
		}
		A := mat.NewDense(counters, buttons, data)

		// Extract to slices for RREF
		matSlice := make([][]float64, counters)
		for i := 0; i < counters; i++ {
			matSlice[i] = make([]float64, buttons)
			for j := 0; j < buttons; j++ {
				matSlice[i][j] = A.At(i, j)
			}
		}

		rhs := make([]float64, counters)
		for i, v := range m.targets {
			rhs[i] = float64(v)
		}

		// RREF
		pivotCols := make([]int, counters)
		for i := range pivotCols {
			pivotCols[i] = -1
		}
		row := 0
		for col := 0; col < buttons && row < counters; col++ {
			pivotRow := -1
			best := 0.0
			for r := row; r < counters; r++ {
				val := math.Abs(matSlice[r][col])
				if val > EPS && val > best {
					best = val
					pivotRow = r
				}
			}
			if pivotRow == -1 {
				continue
			}
			if pivotRow != row {
				matSlice[row], matSlice[pivotRow] = matSlice[pivotRow], matSlice[row]
				rhs[row], rhs[pivotRow] = rhs[pivotRow], rhs[row]
			}
			piv := matSlice[row][col]
			for c := col; c < buttons; c++ {
				matSlice[row][c] /= piv
			}
			rhs[row] /= piv
			for r := 0; r < counters; r++ {
				if r == row {
					continue
				}
				f := matSlice[r][col]
				if math.Abs(f) < EPS {
					continue
				}
				for c := col; c < buttons; c++ {
					matSlice[r][c] -= f * matSlice[row][c]
				}
				rhs[r] -= f * rhs[row]
			}
			pivotCols[row] = col
			row++
		}

		rank := row

		// Check consistency
		bad := false
		for r := rank; r < counters; r++ {
			maxv := 0.0
			for c := 0; c < buttons; c++ {
				if v := math.Abs(matSlice[r][c]); v > maxv {
					maxv = v
				}
			}
			if maxv < EPS && math.Abs(rhs[r]) > EPS {
				bad = true
				break
			}
		}
		if bad {
			continue
		}

		// Free variables
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
				coef[r][f] = matSlice[r][freeCols[f]]
			}
		}

		sumTargets := 0
		for _, v := range m.targets {
			sumTargets += v
		}
		best := sumTargets
		freeVals := make([]int, freeCount)

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
			for v := 0; v <= cap; v++ {
				if cur+v >= best {
					break
				}
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
		total += best
	}
	return total
}

func main() {
	ms := loadMachines("input.txt")
	p1 := part1(ms)
	p2 := part2(ms)
	elapsedMs := float64(time.Since(startTime).Nanoseconds()) / 1e6
	fmt.Printf("min_lights_presses=%d min_counter_presses=%d elapsed_ms=%.3f\n", p1, p2, elapsedMs)
}
