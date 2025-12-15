t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
grid = File.read('input.txt').split("\n").reject(&:empty?)
maxw = grid.map(&:length).max
grid.map! { |l| l.ljust(maxw, ' ') }

def split_blocks(grid)
  h = grid.size
  w = grid.first.size
  empty = Array.new(w, true)
  (0...w).each do |c|
    (0...h).each do |r|
      if grid[r][c] != ' '
        empty[c] = false
        break
      end
    end
  end
  blocks = []
  c = 0
  while c < w
    c += 1 while c < w && empty[c]
    break if c >= w
    start = c
    c += 1 while c < w && !empty[c]
    blocks << [start, c]
  end
  blocks
end

def problem_op(s)
  s.each_char do |ch|
    return ch if ch == '+' || ch == '*'
  end
  '+'
end

def eval_nums(nums, op)
  if op == '+'
    nums.sum
  else
    nums.reduce(1) { |a, b| a * b }
  end
end

def part1(grid, blocks)
  op_row = grid[-1]
  total = 0
  blocks.each do |s, e|
    op = problem_op(op_row[s...e])
    nums = []
    grid[0...-1].each do |row|
      tok = row[s...e].strip
      nums << tok.to_i unless tok.empty?
    end
    total += eval_nums(nums, op)
  end
  total
end

def part2(grid, blocks)
  h = grid.size - 1
  op_row = grid[-1]
  total = 0
  blocks.each do |s, e|
    op = problem_op(op_row[s...e])
    nums = []
    (e - 1).downto(s) do |c|
      digits = []
      (0...h).each do |r|
        ch = grid[r][c]
        digits << ch if ch =~ /[0-9]/
      end
      nums << digits.join.to_i unless digits.empty?
    end
    total += eval_nums(nums, op)
  end
  total
end

blocks = split_blocks(grid)
p1 = part1(grid, blocks)
p2 = part2(grid, blocks)
elapsed_ms = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0) * 1000.0
puts "grand_total=#{p1} quantum_total=#{p2} elapsed_ms=#{format('%.3f', elapsed_ms)}"
