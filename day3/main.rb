t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
input_file = File.exist?('input_eric.txt') ? 'input_eric.txt' : 'input.txt'
lines = File.read(input_file).split("\n")

def best_two(s)
  digits = s.strip.chars
  return 0 if digits.length < 2
  n = digits.length
  suffix = Array.new(n + 1, '0')
  (n - 1).downto(0) do |i|
    suffix[i] = [suffix[i + 1], digits[i]].max
  end
  best = -1
  (0...(n - 1)).each do |i|
    val = digits[i].to_i * 10 + suffix[i + 1].to_i
    best = val if val > best
  end
  best
end

def best_k(s, k)
  digits = s.strip.chars
  drop = digits.length - k
  stack = []
  digits.each do |d|
    while drop > 0 && !stack.empty? && stack[-1] < d
      stack.pop
      drop -= 1
    end
    stack << d
  end
  stack = stack.first(k)
  stack.join.to_i
end

p1 = 0
p2 = 0
lines.each do |line|
  p1 += best_two(line)
  p2 += best_k(line, 12)
end
elapsed_ms = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0) * 1000.0
puts "max-2-digit-sum=#{p1} max-12-digit-sum=#{p2} elapsed_ms=#{format('%.3f', elapsed_ms)}"
