#!/usr/bin/env julia

function best_two_digits(s)
    digs = [c - '0' for c in s]
    n = length(digs)
    n < 2 && return 0
    suffix = zeros(Int, n + 1)
    for i in n:-1:1
        suffix[i] = max(suffix[i+1], digs[i])
    end
    best = -1
    for i in 1:n-1
        cand = 10 * digs[i] + suffix[i+1]
        cand > best && (best = cand)
    end
    return best
end

function best_k_digits(s, k)
    digs = [c - '0' for c in s]
    drop = length(digs) - k
    stack = Int[]
    for d in digs
        while drop > 0 && !isempty(stack) && stack[end] < d
            pop!(stack); drop -= 1
        end
        push!(stack, d)
    end
    if length(stack) > k
        stack = stack[1:k]
    end
    parse(Int, join(stack))
end

function solve(lines; k=12)
    p1 = 0; p2 = 0
    for line in lines
        t = strip(line)
        isempty(t) && continue
        p1 += best_two_digits(t)
        p2 += best_k_digits(t, k)
    end
    return p1, p2
end

function main()
    path = isfile("input_eric.txt") ? "input_eric.txt" : "input.txt"
    lines = readlines(path)
    t0 = time_ns()
    p1, p2 = solve(lines, k=12)
    elapsed_ms = (time_ns() - t0) / 1e6
    println("max-2-digit-sum=$(p1) max-12-digit-sum=$(p2) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
