require 'set'

lines = File.read('input.txt').split("\n")
sr = sc = nil
lines.each_with_index do |line, r|
  idx = line.index('S')
  if idx
    sr = r
    sc = idx
    break
  end
end

def part1(lines, sr, sc)
  h = lines.size
  w = lines.first.size
  active = Set[sc]
  splits = 0
  (sr...h).each do |r|
    next_active = Set.new
    queue = active.to_a
    seen = Set.new
    until queue.empty?
      c = queue.shift
      next if seen.include?(c)
      seen.add(c)
      cell = lines[r][c]
      if cell == '^'
        splits += 1
        queue << c - 1 if c > 0
        queue << c + 1 if c + 1 < w
      else
        next_active.add(c)
      end
    end
    active = next_active
    break if active.empty?
  end
  splits
end

def part2(lines, sr, sc)
  h = lines.size
  w = lines.first.size
  active = { sc => 1 }
  (sr...h).each do |r|
    next_active = Hash.new(0)
    active.each do |c, cnt|
      cell = lines[r][c]
      if cell == '^'
        next_active[c - 1] += cnt if c > 0
        next_active[c + 1] += cnt if c + 1 < w
      else
        next_active[c] += cnt
      end
    end
    active = next_active
    break if active.empty?
  end
  active.values.sum
end

t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p1 = part1(lines, sr, sc)
p2 = part2(lines, sr, sc)
elapsed = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0) * 1000.0
puts "splits=#{p1} timelines=#{p2} elapsed_ms=#{format('%.3f', elapsed)}"
