#!/usr/bin/env julia

using Graphs

function load_points(lines)
    pts = []
    for line in lines
        t = strip(line)
        isempty(t) && continue
        x,y,z = parse.(Int, split(t, ","))
        push!(pts, (x,y,z))
    end
    pts
end

function build_graph(pts)
    n = length(pts)
    g = SimpleGraph(n)
    edges_with_weight = []
    for i in 1:n
        xi,yi,zi = pts[i]
        for j in i+1:n
            xj,yj,zj = pts[j]
            dx = xi - xj; dy = yi - yj; dz = zi - zj
            dist2 = dx*dx + dy*dy + dz*dz
            add_edge!(g, i, j)
            push!(edges_with_weight, (dist2, i, j))
        end
    end
    sort!(edges_with_weight, by = x->x[1])
    g, edges_with_weight
end

function part1(n, edges_with_weight; k=1000)
    g = SimpleGraph(n)
    for (idx, (_, a, b)) in enumerate(edges_with_weight)
        idx > k && break
        add_edge!(g, a, b)
    end
    comps = connected_components(g)
    sizes = sort([length(c) for c in comps], rev=true)
    while length(sizes) < 3
        push!(sizes, 1)
    end
    prod(sizes[1:3])
end

function part2(pts, edges_with_weight)
    n = length(pts)
    g = SimpleGraph(n)
    lastprod = 0
    for (_, a, b) in edges_with_weight
        if !has_edge(g, a, b)
            ra = length(connected_components(g))
            add_edge!(g, a, b)
            rb = length(connected_components(g))
            if rb < ra
                lastprod = pts[a][1] * pts[b][1]
                rb == 1 && break
            end
        end
    end
    lastprod
end

function main()
    lines = readlines("input.txt")
    pts = load_points(lines)
    g, edges_with_weight = build_graph(pts)
    t0 = time_ns()
    p1 = part1(length(pts), edges_with_weight, k=1000)
    p2 = part2(pts, edges_with_weight)
    elapsed_ms = (time_ns() - t0)/1e6
    println("top3_product=$(p1) final_join_x_product=$(p2) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
