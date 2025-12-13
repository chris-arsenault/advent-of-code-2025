#!/usr/bin/env julia

using PolygonOps

function load_points(lines)
    pts = Tuple{Int,Int}[]
    for line in lines
        t = strip(line)
        isempty(t) && continue
        x,y = parse.(Int, split(t, ","))
        push!(pts, (x,y))
    end
    pts
end

function max_rectangle_any(pts)
    best = 0
    n = length(pts)
    for i in 1:n
        x1,y1 = pts[i]
        for j in i+1:n
            x2,y2 = pts[j]
            dx = abs(x1 - x2)
            dy = abs(y1 - y2)
            area = (dx + 1) * (dy + 1)
            area > best && (best = area)
        end
    end
    best
end

function rect_inside_polygon(xlo, xhi, ylo, yhi, poly_coords)
    # Check all 4 corners are inside
    corners = [(xlo, ylo), (xlo, yhi), (xhi, ylo), (xhi, yhi)]
    for (cx, cy) in corners
        # inpolygon returns 1 for inside, 0 for on boundary, -1 for outside
        if inpolygon((cx, cy), poly_coords) == -1
            return false
        end
    end

    # Check no polygon edge crosses the interior
    n = length(poly_coords)
    for i in 1:n
        j = i == n ? 1 : i + 1
        x1, y1 = poly_coords[i]
        x2, y2 = poly_coords[j]

        if x1 == x2  # vertical edge
            if x1 > xlo && x1 < xhi
                ya, yb = minmax(y1, y2)
                if yb > ylo && ya < yhi
                    return false
                end
            end
        elseif y1 == y2  # horizontal edge
            if y1 > ylo && y1 < yhi
                xa, xb = minmax(x1, x2)
                if xb > xlo && xa < xhi
                    return false
                end
            end
        end
    end
    true
end

function max_rectangle_inside(pts, poly_coords)
    best = 0
    n = length(pts)
    for i in 1:n
        x1,y1 = pts[i]
        for j in i+1:n
            x2,y2 = pts[j]
            (x1 == x2 || y1 == y2) && continue
            xlo, xhi = minmax(x1, x2)
            ylo, yhi = minmax(y1, y2)
            if rect_inside_polygon(xlo, xhi, ylo, yhi, poly_coords)
                area = (xhi - xlo + 1) * (yhi - ylo + 1)
                area > best && (best = area)
            end
        end
    end
    best
end

function main()
    lines = readlines("input.txt")
    pts = load_points(lines)
    # PolygonOps expects Vector of Tuples
    poly_coords = [(Float64(x), Float64(y)) for (x,y) in pts]

    t0 = time_ns()
    p1 = max_rectangle_any(pts)
    p2 = max_rectangle_inside(pts, poly_coords)
    elapsed_ms = (time_ns() - t0)/1e6
    println("max_rect_area=$(p1) max_green_rect_area=$(p2) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
