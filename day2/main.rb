t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
text = File.read('input.txt')

def generate_even_half(max_n)
  vals = []
  max_len = max_n.to_s.length
  (1..(max_len / 2)).each do |half_len|
    start = 10**(half_len - 1)
    finish = 10**half_len
    (start...finish).each do |t|
      n = t * (10**half_len) + t
      break if n > max_n
      vals << n
    end
  end
  vals.sort
end

def generate_periodic(max_n)
  seen = {}
  max_len = max_n.to_s.length
  (1..((max_len + 1) / 2)).each do |base_len|
    start = 10**(base_len - 1)
    finish = 10**base_len
    (start...finish).each do |base|
      base_str = base.to_s
      rep = 2
      while base_len * rep <= max_len
        n_str = base_str * rep
        n = n_str.to_i
        break if n > max_n
        seen[n] = true
        rep += 1
      end
    end
  end
  seen.keys.sort
end

def prefix(nums)
  ps = [0]
  nums.each { |n| ps << ps[-1] + n }
  ps
end

def range_sum(nums, ps, lo, hi)
  i = nums.bsearch_index { |x| x >= lo } || nums.length
  j = nums.bsearch_index { |x| x > hi } || nums.length
  ps[j] - ps[i]
end

def parse_ranges(text)
  text.gsub("\n", ",").split(",").filter_map do |part|
    p = part.strip
    next if p.empty?
    a, b = p.split("-").map(&:to_i)
    [a, b]
  end
end

def solve(text)
  ranges = parse_ranges(text)
  max_n = ranges.map { |_, b| b }.max
  evens = generate_even_half(max_n)
  ev_ps = prefix(evens)
  per = generate_periodic(max_n)
  per_ps = prefix(per)
  p1 = p2 = 0
  ranges.each do |a, b|
    p1 += range_sum(evens, ev_ps, a, b)
    p2 += range_sum(per, per_ps, a, b)
  end
  [p1, p2]
end

p1, p2 = solve(text)
elapsed_ms = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0) * 1000.0
puts "repeated-halves-sum=#{p1} repeated-pattern-sum=#{p2} elapsed_ms=#{format('%.3f', elapsed_ms)}"
