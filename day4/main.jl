#!/usr/bin/env julia

const NEI = ((-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1))

function parse_grid(lines)
    rolls = Set{Tuple{Int,Int}}()
    for (r,line) in enumerate(lines)
        r0 = r - 1
        for (c,ch) in enumerate(line)
            c0 = c - 1
            ch == '@' && push!(rolls, (r0,c0))
        end
    end
    rolls
end

function neighbor_counts(rolls::Set{Tuple{Int,Int}})
    counts = Dict{Tuple{Int,Int},Int}()
    for p in rolls
        r,c = p
        cnt = 0
        for (dr,dc) in NEI
            cnt += ((r+dr,c+dc) in rolls) ? 1 : 0
        end
        counts[p] = cnt
    end
    counts
end

function part1(rolls)
    counts = neighbor_counts(rolls)
    count(x->x<4, values(counts))
end

function part2(rolls)
    counts = neighbor_counts(rolls)
    removed = Set{Tuple{Int,Int}}()
    queue = Tuple{Int,Int}[]
    for (p,cnt) in counts
        cnt < 4 && push!(queue, p)
    end
    while !isempty(queue)
        p = pop!(queue)
        p in removed && continue
        push!(removed, p)
        r,c = p
        for (dr,dc) in NEI
            nbr = (r+dr,c+dc)
            if nbr in rolls && !(nbr in removed)
                counts[nbr] -= 1
                counts[nbr] < 4 && push!(queue, nbr)
            end
        end
    end
    length(removed)
end

function main()
    lines = readlines("input.txt")
    rolls = parse_grid(lines)
    t0 = time_ns()
    p1 = part1(rolls)
    p2 = part2(rolls)
    elapsed_ms = (time_ns() - t0)/1e6
    println("accessible=$(p1) removable_total=$(p2) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
