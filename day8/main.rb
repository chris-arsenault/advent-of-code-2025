lines = File.read('input.txt').split("\n")
points = lines.map { |l| l.split(',').map(&:to_i) }
n = points.size

edges = []
(0...n).each do |i|
  ((i + 1)...n).each do |j|
    dx = points[i][0] - points[j][0]
    dy = points[i][1] - points[j][1]
    dz = points[i][2] - points[j][2]
    d2 = dx * dx + dy * dy + dz * dz
    edges << [d2, i, j]
  end
end
edges.sort_by!(&:first)

class DSU
  def initialize(n)
    @parent = {}
    @size = {}
    (0...n).each do |i|
      @parent[i] = i
      @size[i] = 1
    end
  end

  def find(x)
    if @parent[x] != x
      @parent[x] = find(@parent[x])
    end
    @parent[x]
  end

  def unite(a, b)
    ra = find(a)
    rb = find(b)
    return false if ra == rb
    ra, rb = rb, ra if @size[ra] < @size[rb]
    @parent[rb] = ra
    @size[ra] += @size[rb]
    true
  end

  def size(x)
    @size[find(x)]
  end
end

def part1(n, edges)
  dsu = DSU.new(n)
  edges.first(1000).each { |_, a, b| dsu.unite(a, b) }
  comps = (0...n).select { |i| dsu.find(i) == i }.map { |i| dsu.size(i) }
  comps.sort!.reverse!
  comps << 1 while comps.length < 3
  comps[0] * comps[1] * comps[2]
end

def part2(xs, edges, n)
  dsu = DSU.new(n)
  components = n
  last = 0
  edges.each do |_, a, b|
    if dsu.unite(a, b)
      components -= 1
      last = xs[a] * xs[b]
      break if components == 1
    end
  end
  last
end

t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p1 = part1(n, edges)
p2 = part2(points.map(&:first), edges, n)
elapsed = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0) * 1000.0
puts "top3_product=#{p1} final_join_x_product=#{p2} elapsed_ms=#{format('%.3f', elapsed)}"
