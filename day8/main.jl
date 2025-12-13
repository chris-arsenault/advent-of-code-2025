#!/usr/bin/env julia

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

mutable struct DSU
    parent::Vector{Int}
    size::Vector{Int}
    components::Int
end

function DSU(n::Int)
    parent = collect(1:n)
    size = ones(Int, n)
    DSU(parent, size, n)
end

function find!(d::DSU, x::Int)
    p = d.parent[x]
    if p != x
        d.parent[x] = find!(d, p)
    end
    d.parent[x]
end

function union!(d::DSU, a::Int, b::Int)
    ra = find!(d,a); rb = find!(d,b)
    ra == rb && return false
    if d.size[ra] < d.size[rb]
        ra, rb = rb, ra
    end
    d.parent[rb] = ra
    d.size[ra] += d.size[rb]
    d.components -= 1
    true
end

function build_edges(pts)
    edges = []
    n = length(pts)
    for i in 1:n
        xi,yi,zi = pts[i]
        for j in i+1:n
            xj,yj,zj = pts[j]
            dx = xi - xj; dy = yi - yj; dz = zi - zj
            dist2 = dx*dx + dy*dy + dz*dz
            push!(edges, (dist2, i, j))
        end
    end
    sort!(edges, by = x->x[1])
    edges
end

function part1(n, edges; k=1000)
    d = DSU(n)
    for (idx,e) in enumerate(edges)
        idx > k && break
        _,a,b = e
        union!(d,a,b)
    end
    sizes = [d.size[i] for i in 1:n if find!(d,i) == i]
    sort!(sizes, rev=true)
    while length(sizes) < 3
        push!(sizes, 1)
    end
    prod(sizes[1:3])
end

function part2(pts, edges)
    n = length(pts)
    d = DSU(n)
    lastprod = 0
    for e in edges
        _,a,b = e
        if union!(d,a,b)
            lastprod = pts[a][1] * pts[b][1]
            d.components == 1 && break
        end
    end
    lastprod
end

function main()
    lines = readlines("input.txt")
    pts = load_points(lines)
    edges = build_edges(pts)
    t0 = time_ns()
    p1 = part1(length(pts), edges, k=1000)
    p2 = part2(pts, edges)
    elapsed_ms = (time_ns() - t0)/1e6
    println("top3_product=$(p1) final_join_x_product=$(p2) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
