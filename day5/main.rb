text = File.read('input.txt').gsub(/\r\n?/, "\n")
ranges_txt, ids_txt = text.split("\n\n", 2)
ids_txt ||= ""

ranges = ranges_txt.split("\n").filter_map do |line|
  next if line.strip.empty?
  a, b = line.split('-').map(&:to_i)
  [a, b]
end

def merge_ranges(ranges)
  ranges.sort_by! { |a, b| [a, b] }
  merged = []
  ranges.each do |a, b|
    if merged.empty? || a > merged[-1][1] + 1
      merged << [a, b]
    else
      merged[-1][1] = b if b > merged[-1][1]
    end
  end
  merged
end

merged = merge_ranges(ranges)
ids = ids_txt.split("\n").map(&:to_i)

def in_any(ranges, x)
  idx = ranges.bsearch_index { |a, _| a > x } || ranges.length
  return false if idx.zero?
  a, b = ranges[idx - 1]
  a <= x && x <= b
end

t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
fresh = ids.count { |id| in_any(merged, id) }
total = merged.sum { |a, b| b - a + 1 }
elapsed_ms = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0) * 1000.0
puts "available_fresh=#{fresh} total_fresh_ids=#{total} elapsed_ms=#{format('%.3f', elapsed_ms)}"
