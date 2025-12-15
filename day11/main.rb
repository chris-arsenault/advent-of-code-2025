t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)

g = {}
File.read('input.txt').split("\n").each do |line|
  next if line.strip.empty?
  from, rest = line.split(':')
  g[from.strip] = rest.strip.split(/\s+/)
end

def count_paths(g, start, target)
  memo = Hash.new do |h, node|
    h[node] = if node == target
                1
              else
                (g[node] || []).sum { |nxt| h[nxt] }
              end
  end
  memo[start]
end

p1 = count_paths(g, 'you', 'out')
a1 = count_paths(g, 'svr', 'dac')
a2 = count_paths(g, 'dac', 'fft')
a3 = count_paths(g, 'fft', 'out')
b1 = count_paths(g, 'svr', 'fft')
b2 = count_paths(g, 'fft', 'dac')
b3 = count_paths(g, 'dac', 'out')
p2 = a1 * a2 * a3 + b1 * b2 * b3

elapsed_ms = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0) * 1000
puts "paths_you_to_out=#{p1} paths_svr_via_dac_fft=#{p2} elapsed_ms=#{'%.3f' % elapsed_ms}"
