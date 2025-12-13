#!/usr/bin/env julia

function parse_input(text)
    parts = split(text, r"\n\s*\n")
    top = split(parts[1], "\n")
    bottom = length(parts) > 1 ? split(parts[2], "\n") : String[]
    ranges = Tuple{Int,Int}[]
    ids = Int[]
    for line in top
        s = strip(line)
        isempty(s) && continue
        a,b = strip.(split(s, "-"))
        push!(ranges, (parse(Int,a), parse(Int,b)))
    end
    for line in bottom
        t = strip(line)
        isempty(t) || push!(ids, parse(Int,t))
    end
    ranges, ids
end

function merge_ranges(ranges)
    rs = sort(ranges, by=x->x[1])
    merged = Tuple{Int,Int}[]
    for (a,b) in rs
        if isempty(merged) || a > merged[end][2] + 1
            push!(merged, (a,b))
        else
            merged[end] = (merged[end][1], max(merged[end][2], b))
        end
    end
    merged
end

function in_any(ranges, x)
    lo, hi = 1, length(ranges)
    while lo <= hi
        mid = (lo+hi)>>>1
        a,b = ranges[mid]
        if x < a
            hi = mid-1
        elseif x > b
            lo = mid+1
        else
            return true
        end
    end
    false
end

function solve(text)
    ranges, ids = parse_input(text)
    merged = merge_ranges(ranges)
    fresh = count(id->in_any(merged,id), ids)
    total = sum(b-a+1 for (a,b) in merged)
    fresh, total
end

function main()
    text = read("input.txt", String)
    t0 = time_ns()
    p1,p2 = solve(text)
    elapsed_ms = (time_ns() - t0)/1e6
    println("available_fresh=$(p1) total_fresh_ids=$(p2) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
