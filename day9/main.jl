#!/usr/bin/env julia

function load_points(lines)
    pts = []
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
            x1 == x2 || y1 == y2 && continue
            area = abs(x1 - x2) * abs(y1 - y2)
            area > best && (best = area)
        end
    end
    best
end

function orientation(p,q,r)
    val = (q[2]-p[2])*(r[1]-q[1]) - (q[1]-p[1])*(r[2]-q[2])
    val > 0 ? 1 : (val < 0 ? -1 : 0)
end

function on_segment(p,q,r)
    min(p[1],r[1]) <= q[1] <= max(p[1],r[1]) &&
    min(p[2],r[2]) <= q[2] <= max(p[2],r[2])
end

function seg_intersect(p1,q1,p2,q2)
    o1=orientation(p1,q1,p2); o2=orientation(p1,q1,q2)
    o3=orientation(p2,q2,p1); o4=orientation(p2,q2,q1)
    if o1 != o2 && o3 != o4
        return true
    end
    (o1==0 && on_segment(p1,p2,q1)) || (o2==0 && on_segment(p1,q2,q1)) ||
    (o3==0 && on_segment(p2,p1,q2)) || (o4==0 && on_segment(p2,q1,q2))
end

function point_in_poly(poly, p)
    x,y = p
    inside = false
    n = length(poly)
    for i in 1:n
        j = i==1 ? n : i-1
        xi,yi = poly[i]; xj,yj = poly[j]
        if (yi != yj) && (min(yi,yj) <= y < max(yi,yj))
            xint = xi + (y - yi) * (xj - xi) / (yj - yi)
            if xint < x
                inside = !inside
            end
        end
    end
    inside
end

function rectangle_inside(poly, corners)
    for c in corners
        point_in_poly(poly, c) || return false
    end
    edges = [(poly[i], poly[i==length(poly) ? 1 : i+1]) for i in 1:length(poly)]
    rect_edges = [(corners[i], corners[i%4+1]) for i in 1:4]
    for (a,b) in rect_edges
        for (p,q) in edges
            seg_intersect(a,b,p,q) && return false
        end
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
            x1 == x2 || y1 == y2 && continue
            xmin,xmax = min(x1,x2), max(x1,x2)
            ymin,ymax = min(y1,y2), max(y1,y2)
            corners = [(xmin,ymin),(xmin,ymax),(xmax,ymax),(xmax,ymin)]
            if rectangle_inside(poly, corners)
                area = (xmax-xmin)*(ymax-ymin)
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
