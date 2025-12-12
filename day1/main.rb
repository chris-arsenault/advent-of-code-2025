lines = File.read('input.txt').split("\n")

def simulate(lines)
  pos = 50
  zero_hits = 0
  crossings = 0

  lines.each do |raw|
    line = raw.strip
    next if line.empty?
    sign = line[0] == 'R' ? 1 : -1
    mag = line[1..].to_i

    first = sign == 1 ? 100 - pos : pos
    first = 100 if first == 0
    crossings += 1 + (mag - first) / 100 if mag >= first

    pos = (pos + sign * mag) % 100
    pos += 100 if pos < 0
    zero_hits += 1 if pos == 0
  end

  [zero_hits, crossings, pos]
end

t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
zero, cross, pos = simulate(lines)
elapsed_ms = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0) * 1000.0
puts "zero_landings=#{zero} crossings=#{cross} final_pos=#{pos} elapsed_ms=#{format('%.3f', elapsed_ms)}"
