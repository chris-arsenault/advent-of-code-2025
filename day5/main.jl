#!/usr/bin/env julia

using IntervalSets

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

# Merge overlapping intervals using IntervalSets union
function merge_intervals(ranges)
    isempty(ranges) && return ClosedInterval{Int}[]
    # Sort by start
    sorted = sort(ranges, by=x->x[1])
    merged = ClosedInterval{Int}[]
    for (a,b) in sorted
        iv = ClosedInterval(a, b)
        if isempty(merged) || leftendpoint(merged[end]) > b + 1 || rightendpoint(merged[end]) < a - 1
            # Check if can merge with last
            if !isempty(merged) && a <= rightendpoint(merged[end]) + 1
                merged[end] = ClosedInterval(leftendpoint(merged[end]), max(rightendpoint(merged[end]), b))
            else
                push!(merged, iv)
            end
        else
            merged[end] = ClosedInterval(leftendpoint(merged[end]), max(rightendpoint(merged[end]), b))
        end
    end
    merged
end

# Check if point is in any interval
function in_any(intervals, x)
    for iv in intervals
        x in iv && return true
    end
    false
end

function solve(text)
    ranges, ids = parse_input(text)
    merged = merge_intervals(ranges)

    # Part 1: Count IDs in any interval
    fresh = count(id -> in_any(merged, id), ids)

    # Part 2: Total span of merged intervals
    total = sum(rightendpoint(iv) - leftendpoint(iv) + 1 for iv in merged)

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
