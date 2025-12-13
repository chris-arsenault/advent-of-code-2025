#!/usr/bin/env julia

function load_graph(lines)
    g = Dict{String,Vector{String}}()
    for line in lines
        t = strip(line)
        isempty(t) && continue
        parts = split(t, ":")
        src = strip(parts[1])
        dests = split(strip(parts[2]), ' ')
        g[src] = dests
    end
    g
end

function count_paths(g, start, target)
    memo = Dict{String,Int}()
    function dfs(node)
        node == target && return 1
        get!(memo, node) do
            total = 0
            for nxt in get(g, node, String[])
                total += dfs(nxt)
            end
            total
        end
    end
    dfs(start)
end

function main()
    lines = readlines("input.txt")
    t0 = time_ns()
    g = load_graph(lines)
    p1 = count_paths(g, "you", "out")
    a1 = count_paths(g, "svr", "dac")
    a2 = count_paths(g, "dac", "fft")
    a3 = count_paths(g, "fft", "out")
    b1 = count_paths(g, "svr", "fft")
    b2 = count_paths(g, "fft", "dac")
    b3 = count_paths(g, "dac", "out")
    p2 = a1*a2*a3 + b1*b2*b3
    elapsed_ms = (time_ns() - t0)/1e6
    println("paths_you_to_out=$(p1) paths_svr_via_dac_fft=$(p2) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
