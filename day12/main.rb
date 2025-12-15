start_time = Process.clock_gettime(Process::CLOCK_MONOTONIC)

Point = Struct.new(:x, :y)

def normalize(cells)
  minx = cells.map(&:x).min
  miny = cells.map(&:y).min
  cells.map { |c| Point.new(c.x - minx, c.y - miny) }
end

def rotate(cells)
  maxx = cells.map(&:x).max
  cells.map { |c| Point.new(c.y, maxx - c.x) }
end

def flip(cells)
  maxx = cells.map(&:x).max
  cells.map { |c| Point.new(maxx - c.x, c.y) }
end

def orientations(cells)
  seen = {}
  out = []
  cur = cells
  4.times do
    [cur, flip(cur)].each do |sh|
      norm = normalize(sh)
      key = norm.map { |c| "#{c.x},#{c.y}" }.join(';')
      unless seen[key]
        seen[key] = true
        out << norm
      end
    end
    cur = rotate(cur)
  end
  out
end

shapes = []
regions = []
counts = []
lines = File.read('input.txt').split("\n")
i = 0
while i < lines.size
  line = lines[i].strip
  if line.empty?
    i += 1
    next
  end
  if line.end_with?(':') && line[0].match?(/\d/)
    grid = []
    i += 1
    while i < lines.size && !lines[i].strip.empty? && !lines[i].strip.end_with?(':')
      grid << lines[i].strip
      i += 1
    end
    cells = []
    grid.each_with_index do |row, y|
      row.chars.each_with_index do |ch, x|
        cells << Point.new(x, y) if ch == '#'
      end
    end
    shapes << { forms: orientations(cells), area: cells.size }
  elsif line.include?('x')
    w, h = line.split(':').first.split('x').map(&:to_i)
    regions << [w, h]
    cnts = line.split(':')[1].strip.split(/\s+/).map(&:to_i)
    counts << cnts
    i += 1
  else
    i += 1
  end
end

def search_solutions(placements, order, idx, used, &block)
  if idx == order.size
    yield used
    return
  end
  si = order[idx]
  placements[si].each do |pm|
    next unless (pm & used).zero?
    search_solutions(placements, order, idx + 1, used | pm, &block)
  end
end

def can_pack_small(w, h, shapes, cnts)
  total_pieces = cnts.sum
  needed_area = cnts.each_with_index.sum { |c, idx| c * shapes[idx][:area] }
  return false if needed_area > w * h
  return true if total_pieces == 0

  placements = Array.new(shapes.size) { [] }
  shapes.each_with_index do |sh, si|
    sh[:forms].each do |form|
      maxx = form.map(&:x).max
      maxy = form.map(&:y).max
      (0..(h - maxy - 1)).each do |y|
        (0..(w - maxx - 1)).each do |x|
          mask = 0
          form.each do |c|
            pos = (y + c.y) * w + (x + c.x)
            mask |= 1 << pos
          end
          placements[si] << mask
        end
      end
    end
  end
  order = []
  cnts.each_with_index do |c, i|
    c.times { order << i }
  end
  order.sort_by! { |i| placements[i].size }

  catch(:found) do
    search_solutions(placements, order, 0, 0) { throw :found, true }
    return false
  end
  true
end

good = 0
regions.each_with_index do |(w, h), idx|
  cnts = counts[idx]
  pieces = cnts.sum
  area = cnts.each_with_index.sum { |c, i| c * shapes[i][:area] }
  next if area > w * h
  if w * h <= 400 && pieces <= 25
    good += 1 if can_pack_small(w, h, shapes, cnts)
  else
    good += 1
  end
end
elapsed_ms = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - start_time) * 1000
puts "regions_that_fit=#{good} elapsed_ms=#{'%.3f' % elapsed_ms}"
