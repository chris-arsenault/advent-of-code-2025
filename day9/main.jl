#!/usr/bin/env julia

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

function point_on_edge(px, py, x1, y1, x2, y2)
    if x1 == x2
        return px == x1 && py >= min(y1, y2) && py <= max(y1, y2)
    elseif y1 == y2
        return py == y1 && px >= min(x1, x2) && px <= max(x1, x2)
    end
    false
end

function point_inside(px, py, poly)
    n = length(poly)
    inside = false
    j = n
    for i in 1:n
        x1, y1 = poly[j]
        x2, y2 = poly[i]
        point_on_edge(px, py, x1, y1, x2, y2) && return true
        if (y1 > py) != (y2 > py)
            x_intersect = (x2 - x1) * (py - y1) ÷ (y2 - y1) + x1
            if px < x_intersect
                inside = !inside
            end
        end
        j = i
    end
    inside
end

function edge_crosses_interior(xlo, xhi, ylo, yhi, x1, y1, x2, y2)
    if x1 == x2
        (x1 <= xlo || x1 >= xhi) && return false
        ya, yb = minmax(y1, y2)
        (yb <= ylo || ya >= yhi) && return false
        return ya < yhi && yb > ylo
    elseif y1 == y2
        (y1 <= ylo || y1 >= yhi) && return false
        xa, xb = minmax(x1, x2)
        (xb <= xlo || xa >= xhi) && return false
        return xa < xhi && xb > xlo
    end
    false
end

function rect_inside_polygon(xlo, xhi, ylo, yhi, poly)
    point_inside(xlo, ylo, poly) || return false
    point_inside(xlo, yhi, poly) || return false
    point_inside(xhi, ylo, poly) || return false
    point_inside(xhi, yhi, poly) || return false

    n = length(poly)
    j = n
    for i in 1:n
        x1, y1 = poly[j]
        x2, y2 = poly[i]
        edge_crosses_interior(xlo, xhi, ylo, yhi, x1, y1, x2, y2) && return false
        j = i
    end
    true
end

function max_rectangle_inside(pts, poly)
    best = 0
    n = length(pts)
    for i in 1:n
        x1,y1 = pts[i]
        for j in i+1:n
            x2,y2 = pts[j]
            (x1 == x2 || y1 == y2) && continue
            xlo, xhi = minmax(x1, x2)
            ylo, yhi = minmax(y1, y2)
            if rect_inside_polygon(xlo, xhi, ylo, yhi, poly)
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

    t0 = time_ns()
    p1 = max_rectangle_any(pts)
    p2 = max_rectangle_inside(pts, pts)
    elapsed_ms = (time_ns() - t0)/1e6
    println("max_rect_area=$(p1) max_green_rect_area=$(p2) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
