text = File.read('input.txt').gsub(/\r\n?/, "\n")
ranges_txt, ids_txt = text.split("\n\n", 2)
ids_txt ||= ""

ranges = ranges_txt.split("\n").filter_map do |line|
  next if line.strip.empty?
  a, b = line.split('-').map(&:to_i)
  [a, b]
end

# Merge overlapping intervals using sort_by + each_cons(2) for pairwise comparison
def merge_ranges(ranges)
  sorted = ranges.sort_by { |a, _| a }
  return [] if sorted.empty?

  merged = [sorted.first.dup]
  sorted.each_cons(2) do |(_, _), (a2, b2)|
    if a2 <= merged.last[1] + 1
      # Overlapping or adjacent: extend current interval
      merged.last[1] = [merged.last[1], b2].max
    else
      # Non-overlapping: start new interval
      merged << [a2, b2]
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
