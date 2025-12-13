#!/usr/bin/env julia

function load_grid(lines)
    grid = String[]
    sr = -1; sc = -1
    for (r,line) in enumerate(lines)
        push!(grid, line)
        pos = findfirst(==('S'), line)
        if pos !== nothing
            sr = r; sc = pos
        end
    end
    sr < 0 && error("missing S")
    return grid, sr, sc
end

function part1(grid, sr, sc)
    h = length(grid); w = lastindex(grid[1])
    active = [sc]
    splits = 0
    for r in sr:h
        next = Int[]
        seen = Set{Int}()
        while !isempty(active)
            c = pop!(active)
            c in seen && continue
            push!(seen, c)
            cell = grid[r][c]
            if cell == '^'
                splits += 1
                c > 1 && push!(active, c-1)
                c < w && push!(active, c+1)
            else
                push!(next, c)
            end
        end
        active = next
        isempty(active) && break
    end
    splits
end

function part2(grid, sr, sc)
    h = length(grid); w = lastindex(grid[1])
    active = Dict{Int,Int}(sc => 1)
    for r in sr:h
        next = Dict{Int,Int}()
        for (c,count) in active
            cell = grid[r][c]
            if cell == '^'
                c > 1 && (next[c-1] = get(next,c-1,0) + count)
                c < w && (next[c+1] = get(next,c+1,0) + count)
            else
                next[c] = get(next,c,0) + count
            end
        end
        active = next
        isempty(active) && break
    end
    sum(values(active))
end

function main()
    lines = readlines("input.txt")
    grid, sr, sc = load_grid(lines)
    t0 = time_ns()
    p1 = part1(grid, sr, sc)
    p2 = part2(grid, sr, sc)
    elapsed_ms = (time_ns() - t0)/1e6
    println("splits=$(p1) timelines=$(p2) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
