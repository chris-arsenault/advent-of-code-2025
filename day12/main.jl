#!/usr/bin/env julia

normalize(cells) = begin
    minx = minimum(c[1] for c in cells)
    miny = minimum(c[2] for c in cells)
    [(x-minx, y-miny) for (x,y) in cells]
end

rotate(cells) = begin
    maxx = maximum(c[1] for c in cells)
    [(y, maxx - x) for (x,y) in cells]
end

flip(cells) = begin
    maxx = maximum(c[1] for c in cells)
    [(maxx - x, y) for (x,y) in cells]
end

function orientations(cells)
    seen = Set{Vector{Tuple{Int,Int}}}()
    forms = Vector{Vector{Tuple{Int,Int}}}()
    cur = cells
    for _ in 1:4
        for shape in (cur, flip(cur))
            norm = sort(normalize(shape))
            if !(norm in seen)
                push!(seen, norm); push!(forms, norm)
            end
        end
        cur = rotate(cur)
    end
    forms
end

function parse_input(lines)
    shapes = []
    regions = []
    collecting = false
    current = String[]
    idx = 1
    while idx <= length(lines)
        line = strip(lines[idx])
        if isempty(line)
            idx += 1; continue
        end
        if endswith(line, ":") && all(isdigit, ch for ch in line[1:end-1])
            if collecting && !isempty(current)
                cells = [(x-1,y-1) for (y,row) in enumerate(reverse(current)) for (x,ch) in enumerate(row) if ch=='#']
                push!(shapes, orientations(cells))
            end
            empty!(current); collecting = true
            idx += 1
            continue
        elseif occursin("x", line) && occursin(":", line)
            break
        elseif collecting
            push!(current, line)
        end
        idx += 1
    end
    if collecting && !isempty(current)
        cells = [(x-1,y-1) for (y,row) in enumerate(reverse(current)) for (x,ch) in enumerate(row) if ch=='#']
        push!(shapes, orientations(cells))
    end
    for j in idx:length(lines)
        t = strip(lines[j])
        isempty(t) && continue
        sizepart, countpart = split(t, ":")
        w,h = parse.(Int, split(sizepart, "x"))
        counts = [parse(Int, c) for c in split(strip(countpart))]
        push!(regions, (w,h,counts))
    end
    shapes, regions
end

function exact_cover(columns, rows)
    function search(cols, rows)
        isempty(cols) && return true
        # choose column with fewest rows
        col = first(cols)
        best = length(rows) + 1
        for c in cols
            cnt = count(r -> c in r, rows)
            if cnt < best
                best = cnt; col = c
            end
        end
        for r in rows
            col in r || continue
            newcols = setdiff(cols, r)
            newrows = [rw for rw in rows if isempty(intersect(Set(r), Set(rw)))]
            search(newcols, newrows) && return true
        end
        false
    end
    search(columns, rows)
end

function solve_region(w,h,shapes,counts)
    needed = sum(length(shp[1])*c for (shp,c) in zip(shapes, counts))
    needed > w*h && return false
    piece_cols = sum(counts)
    cell_cols = w*h
    piece_cols > 60 && return true
    rows = []
    offsets = cumsum([0; counts[1:end-1]])
    for (sidx,forms) in enumerate(shapes)
        copies = counts[sidx]
        for copy in 0:copies-1
            piececol = offsets[sidx] + copy + 1
            for form in forms
                maxx = maximum(c[1] for c in form)
                maxy = maximum(c[2] for c in form)
                for y in 0:h-maxy-1
                    for x in 0:w-maxx-1
                        cols = [piececol]
                        for (dx,dy) in form
                            push!(cols, piece_cols + (y+dy)*w + (x+dx) + 1)
                        end
                        push!(rows, cols)
                    end
                end
            end
        end
    end
    columns = collect(1:piece_cols+cell_cols)
    exact_cover(columns, rows)
end

function solve(lines)
    shapes, regions = parse_input(lines)
    fits = 0
    for (w,h,counts) in regions
        solve_region(w,h,shapes,counts) && (fits += 1)
    end
    fits
end

function main()
    lines = readlines("input.xt")
    t0 = time_ns()
    ans = solve(lines)
    elapsed_ms = (time_ns() - t0)/1e6
    println("regions_that_fit=$(ans) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
