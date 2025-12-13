#!/usr/bin/env julia

function load_grid(text)
    lines = [rstrip(l, '\n') for l in split(text, "\n") if !isempty(l)]
    width = maximum(length, lines)
    [l * repeat(" ", width - length(l)) for l in lines]
end

function split_blocks(grid)
    h = length(grid)
    w = length(grid[1])
    empty_col = [all(grid[r][c] == ' ' for r in 1:h) for c in 1:w]
    blocks = Tuple{Int,Int}[]
    c = 1
    while c <= w
        while c <= w && empty_col[c]; c += 1; end
        c > w && break
        start = c
        while c <= w && !empty_col[c]; c += 1; end
        push!(blocks, (start, c))
    end
    blocks
end

function problem_operator(row, start, stop)
    for i in start:stop-1
        ch = row[i]
        (ch == '+' || ch == '*') && return ch
    end
    error("no operator")
end

function eval_numbers(nums, op)
    op == '+' ? sum(nums) : prod(nums)
end

function part1(grid, blocks)
    op_row = grid[end]
    rows = grid[1:end-1]
    total = 0
    for (s,e) in blocks
        op = problem_operator(op_row, s, e)
        nums = Int[]
        for row in rows
            token = strip(row[s:e-1])
            isempty(token) || push!(nums, parse(Int, token))
        end
        total += eval_numbers(nums, op)
    end
    total
end

function part2(grid, blocks)
    h = length(grid)-1
    w = length(grid[1])
    op_row = grid[end]
    total = 0
    for (s,e) in blocks
        op = problem_operator(op_row, s, e)
        nums = Int[]
        for c in e-1:-1:s
            digits = Char[]
            for r in 1:h
                ch = grid[r][c]
                isdigit(ch) && push!(digits, ch)
            end
            if !isempty(digits)
                push!(nums, parse(Int, String(digits)))
            end
        end
        total += eval_numbers(nums, op)
    end
    total
end

function main()
    text = read("input.txt", String)
    grid = load_grid(text)
    blocks = split_blocks(grid)
    t0 = time_ns()
    p1 = part1(grid, blocks)
    p2 = part2(grid, blocks)
    elapsed_ms = (time_ns() - t0)/1e6
    println("grand_total=$(p1) quantum_total=$(p2) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
