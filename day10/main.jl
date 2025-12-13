#!/usr/bin/env julia

function read_lines(path)
    readlines(path)
end

function parse_between(line, a, b)
    lb = findfirst(==(a), line)
    rb = findlast(==(b), line)
    lb === nothing || rb === nothing || rb <= lb && return nothing
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
    buttons = []
    pos = 1
    while true
        lb = findnext(==( '(' ), line, pos)
        lb === nothing && break
        rb = findnext(==( ')' ), line, lb)
        rb === nothing && break
        push!(buttons, split_numbers(line[lb+1:rb-1]))
        pos = rb + 1
    end
    buttons
end

# Part A
function rref_binary(matrix, target, lights, buttons)
    pivot_cols = fill(-1, lights)
    rank = 0
    for col in 1:buttons
        rank == lights && break
        pivot = findfirst(r -> matrix[r,col] == 1, rank+1:lights)
        pivot === nothing && continue
        if pivot != rank+1
            matrix[[rank+1,pivot],:] = matrix[[pivot,rank+1],:]
            target[[rank+1,pivot]] = target[[pivot,rank+1]]
        end
        pivot_cols[rank+1] = col
        for r in rank+2:lights
            if matrix[r,col] == 1
                matrix[r,:] .⊻= matrix[rank+1,:]
                target[r] ⊻= target[rank+1]
            end
        end
        rank += 1
    end
    for i in rank:-1:1
        col = pivot_cols[i]
        col < 1 && continue
        for r in 1:i-1
            if matrix[r,col] == 1
                matrix[r,:] .⊻= matrix[i,:]
                target[r] ⊻= target[i]
            end
        end
    end
    rank, pivot_cols
end

function solve_min_press(matrix, target, lights, buttons, rank, pivot_cols)
    pivot_set = Set(filter(x -> x >= 1, pivot_cols))
    free_cols = [c for c in 1:buttons if !(c in pivot_set)]
    free_count = length(free_cols)
    if free_count > 20
        sol = zeros(Int, buttons)
        for i in rank:-1:1
            col = pivot_cols[i]; val = target[i]
            for c in 1:buttons
                c == col && continue
                if matrix[i,c] == 1
                    val ⊻= sol[c]
                end
            end
            sol[col] = val
        end
        return sum(sol)
    else
        best = typemax(Int)
        combos = 1 << free_count
        sol = zeros(Int, buttons)
        for mask in 0:combos-1
            fill!(sol, 0)
            weight = 0
            for (k,col) in enumerate(free_cols)
                if (mask >> (k-1)) & 1 == 1
                    sol[col] = 1
                    weight += 1
                end
            end
            for i in rank:-1:1
                col = pivot_cols[i]; val = target[i]
                for c in 1:buttons
                    c == col && continue
                    matrix[i,c] == 1 && (val ⊻= sol[c])
                end
                sol[col] = val
                weight += val
                weight >= best && break
            end
            weight < best && (best = weight)
        end
        return best
    end
end

function part1(lines)
    total = 0
    for line in lines
        pattern = parse_between(line, '[', ']')
        pattern === nothing && continue
        lights = length(pattern)
        buttons = parse_buttons(line)
        btn_count = length(buttons)
        matrix = falses(lights, btn_count)
        target = falses(lights)
        for i in 1:lights
            target[i] = pattern[i] == '#'
        end
        for (idx,btn) in enumerate(buttons)
            for pos in btn
                pos < lights && (matrix[pos+1, idx] ⊻= true)
            end
        end
        rank,piv = rref_binary(matrix, target, lights, btn_count)
        ok = true
        for r in rank+1:lights
            if all(matrix[r,c]==false for c in 1:btn_count) && target[r]
                ok = false; break
            end
        end
        ok || continue
        total += solve_min_press(matrix, target, lights, btn_count, rank, piv)
    end
    total
end

# Part B simple integer search with free vars brute force cap
function integer_solution(buttons, targets)
    counters = length(targets)
    btn_count = length(buttons)
    # build matrix counters x buttons
    mat = zeros(Int, counters, btn_count)
    for (c,btn) in enumerate(buttons)
        for idx in btn
            idx < counters && (mat[idx+1,c] += 1)
        end
    end
    # naive search limited: try up to cap per button
    cap = 12
    best = typemax(Int)
    assigns = zeros(Int, btn_count)
    function dfs(i)
        nonlocal best
        if i > btn_count
            # check
            totals = zeros(Int, counters)
            for c in 1:btn_count
                if assigns[c] != 0
                    totals .+= assigns[c] .* mat[:,c]
                end
            end
            totals == targets || return
            presses = sum(assigns)
            presses < best && (best = presses)
            return
        end
        presses_so_far = sum(assigns[1:i-1])
        presses_so_far >= best && return
        for v in 0:cap
            assigns[i] = v
            dfs(i+1)
        end
    end
    dfs(1)
    best == typemax(Int) ? 0 : best
end

function part2(lines)
    total = 0
    for line in lines
        targets = parse_between(line, '{', '}')
        targets === nothing && continue
        tgt = split_numbers(targets)
        buttons = parse_buttons(line)
        presses = integer_solution(buttons, tgt)
        total += presses
    end
    total
end

function main()
    lines = read_lines("input.txt")
    t0 = time_ns()
    p1 = part1(lines)
    p2 = part2(lines)
    elapsed_ms = (time_ns() - t0)/1e6
    println("min_lights_presses=$(p1) min_counter_presses=$(p2) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
