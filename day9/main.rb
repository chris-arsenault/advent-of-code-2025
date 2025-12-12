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
      area = (x1 - x2).abs * (y1 - y2).abs
      best = area if area > best
    end
  end
  best
end

def point_in_poly(p, poly)
  x, y = p
  inside = false
  j = poly.size - 1
  (0...poly.size).each do |i|
    xi, yi = poly[i]
    xj, yj = poly[j]
    if ((yi > y) != (yj > y)) && (x < (xj - xi) * (y - yi) / (yj - yi) + xi)
      inside = !inside
    end
    j = i
  end
  inside
end

def max_rect_inside(pts, poly)
  best = 0
  n = pts.size
  (0...n).each do |i|
    (i + 1...n).each do |j|
      x1, y1 = pts[i]
      x2, y2 = pts[j]
      next if x1 == x2 || y1 == y2
      a = [x1, x2].min
      b = [x1, x2].max
      c = [y1, y2].min
      d = [y1, y2].max
      corners = [[a, c], [a, d], [b, c], [b, d]]
      next unless corners.all? { |c| point_in_poly(c, poly) }
      area = (b - a) * (d - c)
      best = area if area > best
    end
  end
  best
end

t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p1 = max_rect_any(points)
p2 = max_rect_inside(points, points)
elapsed = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0) * 1000.0
puts "max_rect_area=#{p1} max_green_rect_area=#{p2} elapsed_ms=#{format('%.3f', elapsed)}"
