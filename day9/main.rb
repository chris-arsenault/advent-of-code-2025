points = File.read('input.txt').split("\n").map do |line|
  x, y = line.split(',').map(&:to_i)
  [x, y]
end

def max_rect_any(pts)
  best = 0
  n = pts.size
  (0...n).each do |i|
    (i + 1...n).each do |j|
      x1, y1 = pts[i]
      x2, y2 = pts[j]
      next if x1 == x2 || y1 == y2
      area = ((x1 - x2).abs + 1) * ((y1 - y2).abs + 1)
      best = area if area > best
    end
  end
  best
end

def point_on_edge?(px, py, x1, y1, x2, y2)
  if x1 == x2
    return px == x1 && py >= [y1, y2].min && py <= [y1, y2].max
  elsif y1 == y2
    return py == y1 && px >= [x1, x2].min && px <= [x1, x2].max
  end
  false
end

def point_inside?(px, py, poly)
  n = poly.size
  inside = false
  j = n - 1
  (0...n).each do |i|
    x1, y1 = poly[j]
    x2, y2 = poly[i]
    return true if point_on_edge?(px, py, x1, y1, x2, y2)
    if (y1 > py) != (y2 > py)
      x_intersect = (x2 - x1) * (py - y1) / (y2 - y1) + x1
      inside = !inside if px < x_intersect
    end
    j = i
  end
  inside
end

def edge_crosses_interior?(xlo, xhi, ylo, yhi, x1, y1, x2, y2)
  if x1 == x2
    return false if x1 <= xlo || x1 >= xhi
    ya, yb = [y1, y2].minmax
    return false if yb <= ylo || ya >= yhi
    return ya < yhi && yb > ylo
  elsif y1 == y2
    return false if y1 <= ylo || y1 >= yhi
    xa, xb = [x1, x2].minmax
    return false if xb <= xlo || xa >= xhi
    return xa < xhi && xb > xlo
  end
  false
end

def rect_inside_polygon?(xlo, xhi, ylo, yhi, poly)
  return false unless point_inside?(xlo, ylo, poly)
  return false unless point_inside?(xlo, yhi, poly)
  return false unless point_inside?(xhi, ylo, poly)
  return false unless point_inside?(xhi, yhi, poly)

  n = poly.size
  j = n - 1
  (0...n).each do |i|
    x1, y1 = poly[j]
    x2, y2 = poly[i]
    return false if edge_crosses_interior?(xlo, xhi, ylo, yhi, x1, y1, x2, y2)
    j = i
  end
  true
end

def max_rect_inside(pts, poly)
  best = 0
  n = pts.size
  (0...n).each do |i|
    (i + 1...n).each do |j|
      x1, y1 = pts[i]
      x2, y2 = pts[j]
      next if x1 == x2 || y1 == y2

      xlo, xhi = [x1, x2].minmax
      ylo, yhi = [y1, y2].minmax

      if rect_inside_polygon?(xlo, xhi, ylo, yhi, poly)
        area = (xhi - xlo + 1) * (yhi - ylo + 1)
        best = area if area > best
      end
    end
  end
  best
end

t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p1 = max_rect_any(points)
p2 = max_rect_inside(points, points)
elapsed = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0) * 1000.0
puts "max_rect_area=#{p1} max_green_rect_area=#{p2} elapsed_ms=#{format('%.3f', elapsed)}"
