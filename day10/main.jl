#!/usr/bin/env julia

using LinearAlgebra

function parse_between(line, a, b)
    lb = findfirst(==(a), line)
    rb = findlast(==(b), line)
    (lb === nothing || rb === nothing || rb <= lb) && return nothing
    line[lb+1:rb-1]
end

function split_numbers(s)
    nums = Int[]
    for part in split(s, ",")
        t = strip(part)
        isempty(t) || push!(nums, parse(Int, t))
    end
    nums
end

function parse_buttons(line)
    buttons = Vector{Int}[]
    pos = 1
    while true
        lb = findnext(==('('), line, pos)
        lb === nothing && break
        rb = findnext(==(')'), line, lb)
        rb === nothing && break
        push!(buttons, split_numbers(line[lb+1:rb-1]))
        pos = rb + 1
    end
    buttons
end

# Part 1: GF(2) linear system - find minimum weight solution
function solve_gf2(matrix::BitMatrix, target::BitVector)
    lights, buttons = size(matrix)
    mat = copy(matrix)
    tgt = copy(target)
    pivot_cols = fill(-1, lights)

    # Forward elimination
    rank = 0
    for col in 1:buttons
        rank == lights && break
        # Find pivot
        pivot_row = findfirst(i -> mat[i, col], rank+1:lights)
        pivot_row === nothing && continue
        pivot_row += rank

        # Swap rows
        if pivot_row != rank + 1
            mat[rank+1, :], mat[pivot_row, :] = mat[pivot_row, :], mat[rank+1, :]
            tgt[rank+1], tgt[pivot_row] = tgt[pivot_row], tgt[rank+1]
        end

        rank += 1
        pivot_cols[rank] = col

        # Eliminate below
        for r in rank+1:lights
            if mat[r, col]
                mat[r, :] .⊻= mat[rank, :]
                tgt[r] ⊻= tgt[rank]
            end
        end
    end

    # Back substitution to RREF
    for i in rank:-1:1
        col = pivot_cols[i]
        col < 1 && continue
        for r in 1:i-1
            if mat[r, col]
                mat[r, :] .⊻= mat[i, :]
                tgt[r] ⊻= tgt[i]
            end
        end
    end

    # Check consistency
    for r in rank+1:lights
        if tgt[r]
            return -1  # No solution
        end
    end

    # Find free variables
    pivot_set = Set(pivot_cols[1:rank])
    free_cols = [c for c in 1:buttons if !(c in pivot_set)]
    free_count = length(free_cols)

    # Enumerate free variable assignments to find minimum weight
    if free_count > 20
        # Too many free vars - just use one solution
        sol = falses(buttons)
        for i in rank:-1:1
            col = pivot_cols[i]
            val = tgt[i]
            for c in 1:buttons
                c == col && continue
                mat[i, c] && (val ⊻= sol[c])
            end
            sol[col] = val
        end
        return count(sol)
    end

    best = buttons + 1
    for mask in 0:(1 << free_count) - 1
        sol = falses(buttons)
        weight = 0

        # Set free variables
        for (k, col) in enumerate(free_cols)
            if (mask >> (k-1)) & 1 == 1
                sol[col] = true
                weight += 1
            end
        end

        # Back-substitute pivot variables
        for i in rank:-1:1
            col = pivot_cols[i]
            val = tgt[i]
            for c in 1:buttons
                c == col && continue
                mat[i, c] && (val ⊻= sol[c])
            end
            sol[col] = val
            weight += val
            weight >= best && break
        end

        weight < best && (best = weight)
    end

    best
end

function part1(lines)
    total = 0
    for line in lines
        pattern = parse_between(line, '[', ']')
        pattern === nothing && continue

        lights = length(pattern)
        buttons = parse_buttons(line)
        btn_count = length(buttons)
        btn_count == 0 && continue

        # Build matrix: mat[light, button] = 1 if button toggles light
        mat = falses(lights, btn_count)
        tgt = BitVector([c == '#' for c in pattern])

        for (idx, btn) in enumerate(buttons)
            for pos in btn
                if pos >= 0 && pos < lights
                    mat[pos+1, idx] ⊻= true
                end
            end
        end

        presses = solve_gf2(mat, tgt)
        presses >= 0 && (total += presses)
    end
    total
end

# Part 2: Non-negative integer linear system using Rational arithmetic
function solve_integer_system(matrix::Matrix{Rational{Int}}, targets::Vector{Rational{Int}}, buttons::Int)
    counters = size(matrix, 1)
    aug = hcat(matrix, targets)
    pivot_cols = fill(-1, counters)

    # Gaussian elimination to RREF
    rank = 0
    for col in 1:buttons
        rank == counters && break

        # Find pivot (largest absolute value for numerical stability)
        pivot_row = -1
        best_val = 0 // 1
        for r in rank+1:counters
            v = abs(aug[r, col])
            if v > 0 && v > best_val
                best_val = v
                pivot_row = r
            end
        end
        pivot_row == -1 && continue

        # Swap rows
        if pivot_row != rank + 1
            aug[rank+1, :], aug[pivot_row, :] = aug[pivot_row, :], aug[rank+1, :]
        end

        rank += 1
        pivot_cols[rank] = col

        # Scale pivot row
        piv = aug[rank, col]
        aug[rank, :] ./= piv

        # Eliminate all other rows
        for r in 1:counters
            r == rank && continue
            factor = aug[r, col]
            factor == 0 && continue
            aug[r, :] .-= factor .* aug[rank, :]
        end
    end

    # Check consistency
    for r in rank+1:counters
        row_zero = all(aug[r, 1:buttons] .== 0)
        if row_zero && aug[r, buttons+1] != 0
            return -1  # Inconsistent
        end
    end

    # Find free variables
    pivot_set = Set(pivot_cols[1:rank])
    free_cols = [c for c in 1:buttons if !(c in pivot_set)]
    free_count = length(free_cols)

    # Extract coefficients for free variables
    # For pivot row i: x[pivot_cols[i]] = rhs[i] - sum(coef[i,f] * x[free_cols[f]])
    rhs = [aug[r, buttons+1] for r in 1:rank]
    coef = zeros(Rational{Int}, rank, free_count)
    for r in 1:rank
        for (f, fc) in enumerate(free_cols)
            coef[r, f] = aug[r, fc]
        end
    end

    # DFS to find minimum sum non-negative integer solution
    best = typemax(Int)
    free_vals = zeros(Int, free_count)

    # Estimate max value for free variables
    max_rhs = maximum(rhs; init=0//1)
    cap = max(200, Int(ceil(max_rhs)) + 50)

    function dfs(fidx, current_sum)
        current_sum >= best && return

        if fidx > free_count
            total_press = current_sum
            for r in 1:rank
                v = rhs[r]
                for f in 1:free_count
                    v -= coef[r, f] * free_vals[f]
                end
                # Must be non-negative integer
                v < 0 && return
                denom = denominator(v)
                denom != 1 && return
                iv = Int(numerator(v))
                total_press += iv
                total_press >= best && return
            end
            total_press < best && (best = total_press)
            return
        end

        max_v = min(cap, best - current_sum - 1)
        for v in 0:max_v
            free_vals[fidx] = v
            dfs(fidx + 1, current_sum + v)
        end
    end

    dfs(1, 0)
    best == typemax(Int) ? 0 : best
end

function part2(lines)
    total = 0
    for line in lines
        tgt_str = parse_between(line, '{', '}')
        tgt_str === nothing && continue

        targets = split_numbers(tgt_str)
        buttons = parse_buttons(line)
        btn_count = length(buttons)
        counters = length(targets)

        (btn_count == 0 || counters == 0) && continue

        # Build matrix: mat[counter, button] = 1 if button increments counter
        mat = zeros(Rational{Int}, counters, btn_count)
        for (idx, btn) in enumerate(buttons)
            for pos in btn
                if pos >= 0 && pos < counters
                    mat[pos+1, idx] = 1 // 1
                end
            end
        end

        tgt_vec = Rational{Int}.(targets)
        presses = solve_integer_system(mat, tgt_vec, btn_count)
        presses >= 0 && (total += presses)
    end
    total
end

function main()
    lines = readlines("input.txt")
    t0 = time_ns()
    p1 = part1(lines)
    p2 = part2(lines)
    elapsed_ms = (time_ns() - t0) / 1e6
    println("min_lights_presses=$(p1) min_counter_presses=$(p2) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
