require 'rgeo'

factory = RGeo::Cartesian.factory

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

def max_rect_inside(pts, poly, factory)
  best = 0
  n = pts.size
  (0...n).each do |i|
    (i + 1...n).each do |j|
      x1, y1 = pts[i]
      x2, y2 = pts[j]
      next if x1 == x2 || y1 == y2

      xlo, xhi = [x1, x2].minmax
      ylo, yhi = [y1, y2].minmax

      rect = factory.polygon(
        factory.linear_ring([
          factory.point(xlo, ylo),
          factory.point(xhi, ylo),
          factory.point(xhi, yhi),
          factory.point(xlo, yhi),
          factory.point(xlo, ylo)
        ])
      )

      if poly.contains?(rect)
        area = (xhi - xlo) * (yhi - ylo)
        best = area if area > best
      end
    end
  end
  best
end

# Build polygon from points
ring_points = points.map { |x, y| factory.point(x, y) }
ring_points << ring_points.first  # Close the ring
poly = factory.polygon(factory.linear_ring(ring_points))

t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p1 = max_rect_any(points)
p2 = max_rect_inside(points, poly, factory)
elapsed = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0) * 1000.0
puts "max_rect_area=#{p1} max_green_rect_area=#{p2} elapsed_ms=#{format('%.3f', elapsed)}"
