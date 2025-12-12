g = {}
File.read('input.txt').split("\n").each do |line|
  next if line.strip.empty?
  from, rest = line.split(':')
  g[from.strip] = rest.strip.split(/\s+/)
end

def count_paths(g, start, target, memo = {})
  return memo[start] if memo.key?(start)
  return memo[start] = 1 if start == target
  total = 0
  (g[start] || []).each do |nxt|
    total += count_paths(g, nxt, target, memo)
  end
  memo[start] = total
end

t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p1 = count_paths(g, 'you', 'out')
a1 = count_paths(g, 'svr', 'dac')
a2 = count_paths(g, 'dac', 'fft')
a3 = count_paths(g, 'fft', 'out')
b1 = count_paths(g, 'svr', 'fft')
b2 = count_paths(g, 'fft', 'dac')
b3 = count_paths(g, 'dac', 'out')
p2 = a1 * a2 * a3 + b1 * b2 * b3
elapsed = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0) * 1000.0
puts "paths_you_to_out=#{p1} paths_svr_via_dac_fft=#{p2} elapsed_ms=#{format('%.3f', elapsed)}"
