#!/usr/bin/env julia

function load_grid(text)
    lines = [rstrip(l, '\n') for l in split(text, "\n") if !isempty(l)]
    width = maximum(length, lines)
    padded = [l * repeat(" ", width - length(l)) for l in lines]
    # Convert to matrix for eachcol support
    height = length(padded)
    mat = Matrix{Char}(undef, height, width)
    for (r, line) in enumerate(padded)
        for (c, ch) in enumerate(line)
            mat[r, c] = ch
        end
    end
    mat
end

function split_blocks(grid)
    h, w = size(grid)
    empty_col = [all(grid[r, c] == ' ' for r in 1:h) for c in 1:w]
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
    h, w = size(grid)
    op_row = grid[h, :]
    total = 0
    for (s, e) in blocks
        op = problem_operator(op_row, s, e)
        nums = Int[]
        for r in 1:h-1
            token = strip(String(grid[r, s:e-1]))
            isempty(token) || push!(nums, parse(Int, token))
        end
        total += eval_numbers(nums, op)
    end
    total
end

# Part 2: Use eachcol for idiomatic column iteration
function part2(grid, blocks)
    h, w = size(grid)
    op_row = grid[h, :]
    total = 0
    for (s, e) in blocks
        op = problem_operator(op_row, s, e)
        nums = Int[]
        # Iterate columns right-to-left using eachcol on submatrix
        block_cols = @view grid[1:h-1, s:e-1]
        for col in Iterators.reverse(eachcol(block_cols))
            digits = filter(isdigit, col)
            if !isempty(digits)
                push!(nums, parse(Int, String(digits)))
            end
        end
        total += eval_numbers(nums, op)
    end
    total
end

function main()
    t0 = time_ns()
    text = read("input.txt", String)
    grid = load_grid(text)
    blocks = split_blocks(grid)
    p1 = part1(grid, blocks)
    p2 = part2(grid, blocks)
    elapsed_ms = (time_ns() - t0)/1e6
    println("grand_total=$(p1) quantum_total=$(p2) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
