lines = File.read('input.txt').split("\n")

DIRS = [
  [-1, -1], [-1, 0], [-1, 1],
  [0, -1], [0, 1],
  [1, -1], [1, 0], [1, 1]
]

def key(r, c) = "#{r},#{c}"

def parse_grid(lines)
  rolls = {}
  lines.each_with_index do |line, r|
    line.chars.each_with_index do |ch, c|
      rolls[key(r, c)] = true if ch == '@'
    end
  end
  rolls
end

def neighbor_counts(rolls)
  counts = {}
  rolls.keys.each do |k|
    r, c = k.split(',').map(&:to_i)
    cnt = DIRS.count { |dr, dc| rolls[key(r + dr, c + dc)] }
    counts[k] = cnt
  end
  counts
end

def part1(counts)
  counts.values.count { |v| v < 4 }
end

def part2(rolls, counts)
  q = []
  in_q = {}
  counts.each do |k, v|
    if v < 4
      q << k
      in_q[k] = true
    end
  end
  removed = 0
  until q.empty?
    k = q.shift
    next unless rolls.delete(k)
    removed += 1
    r, c = k.split(',').map(&:to_i)
    DIRS.each do |dr, dc|
      nk = key(r + dr, c + dc)
      next unless rolls[nk]
      counts[nk] -= 1
      if counts[nk] < 4 && !in_q[nk]
        q << nk
        in_q[nk] = true
      end
    end
  end
  removed
end

rolls = parse_grid(lines)
counts = neighbor_counts(rolls)
t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
a = part1(counts)
removed = part2(rolls.dup, counts.dup)
elapsed_ms = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0) * 1000.0
puts "accessible=#{a} removable_total=#{removed} elapsed_ms=#{format('%.3f', elapsed_ms)}"
