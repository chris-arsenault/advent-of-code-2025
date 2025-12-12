line_re = /\[(.*?)\].*?\{([^}]*)\}/
button_re = /\(([^)]*)\)/

def parse_machine(line)
  pat_match = /\[(.*?)\]/.match(line)
  buttons = line.scan(/\(([^)]*)\)/).map do |m|
    s = m[0].strip
    s.empty? ? [] : s.split(',').map(&:to_i)
  end
  targets = line[/\{([^}]*)\}/, 1].split(',').map(&:to_i)
  [pat_match[1], buttons, targets]
end

machines = File.read('input.txt').split("\n").reject(&:empty?).map { |l| parse_machine(l) }

def part1(machines)
  total = 0
  machines.each do |pat, buttons, _|
    lights = pat.length
    target = 0
    pat.chars.each_with_index { |ch, i| target |= 1 << i if ch == '#' }
    masks = buttons.map do |b|
      mask = 0
      b.each { |idx| mask ^= 1 << idx if idx < lights }
      mask
    end
    best = 1 << 30
    n = masks.length
    (0...(1 << n)).each do |mask|
      state = 0
      presses = mask.to_s(2).count('1')
      n.times { |i| state ^= masks[i] if (mask >> i) & 1 == 1 }
      best = presses if state == target && presses < best
    end
    total += best
  end
  total
end

def part2(machines)
  total = 0
  machines.each do |_, buttons, targets|
    counters = targets.length
    next if counters.zero? || buttons.empty?

    eps = 1e-9
    cols = buttons.length

    # Build matrix counters x buttons
    mat = Array.new(counters) { Array.new(cols, 0.0) }
    buttons.each_with_index do |b, cidx|
      b.each { |t| mat[t][cidx] = 1.0 if t >= 0 && t < counters }
    end
    rhs = targets.map(&:to_f)

    # RREF
    pivot_cols = Array.new(counters, -1)
    row = 0
    (0...cols).each do |col|
      break if row >= counters
      pivot = -1
      best = 0.0
      (row...counters).each do |r|
        val = mat[r][col].abs
        if val > eps && val > best
          best = val
          pivot = r
        end
      end
      next if pivot == -1
      if pivot != row
        mat[row], mat[pivot] = mat[pivot], mat[row]
        rhs[row], rhs[pivot] = rhs[pivot], rhs[row]
      end
      piv = mat[row][col]
      (col...cols).each { |c| mat[row][c] /= piv }
      rhs[row] /= piv
      (0...counters).each do |r|
        next if r == row
        f = mat[r][col]
        next if f.abs < eps
        (col...cols).each { |c| mat[r][c] -= f * mat[row][c] }
        rhs[r] -= f * rhs[row]
      end
      pivot_cols[row] = col
      row += 1
    end
    rank = row
    inconsistent = false
    (rank...counters).each do |r|
      maxv = mat[r].map(&:abs).max
      if maxv < eps && rhs[r].abs > eps
        inconsistent = true
        break
      end
    end
    next if inconsistent

    used = Array.new(cols, false)
    (0...rank).each { |r| used[pivot_cols[r]] = true if pivot_cols[r] >= 0 }
    free_cols = (0...cols).reject { |c| used[c] }
    free_count = free_cols.length

    coef = Array.new(rank) { Array.new(free_count, 0.0) }
    (0...rank).each do |r|
      (0...free_count).each do |f|
        coef[r][f] = mat[r][free_cols[f]]
      end
    end

    sum_targets = targets.sum
    best_total = sum_targets
    free_vals = Array.new(free_count, 0)
    bounds = Array.new(free_count, best_total)
    (0...free_count).each do |f|
      (0...rank).each do |r|
        a = coef[r][f]
        if a > eps
          limit = (rhs[r] / a + eps).floor
          bounds[f] = limit if limit < bounds[f]
        end
      end
    end

    evaluate = lambda do |cur|
      return if cur >= best_total
      sum = cur
      (0...rank).each do |r|
        v = rhs[r]
        (0...free_count).each { |f| v -= coef[r][f] * free_vals[f] }
        return if v < -eps
        iv = v.round
        return if (iv - v).abs > eps
        sum += iv
        return if sum >= best_total
      end
      best_total = sum if sum < best_total
    end

    evaluate.call(0)

    quick = lambda do |idx, cur, cap|
      return if cur >= best_total
      if idx == free_count
        evaluate.call(cur)
        return
      end
      lim = [cap, bounds[idx]].min
      return if lim < 0
      (0..lim).each do |v|
        free_vals[idx] = v
        quick.call(idx + 1, cur + v, cap)
      end
    end

    seed_cap = [400, best_total].min
    quick.call(0, 0, seed_cap) if free_count.positive? && seed_cap.positive?

    dfs = lambda do |idx, cur|
      return if cur >= best_total
      if idx == free_count
        evaluate.call(cur)
        return
      end
      maxv = [best_total - cur, bounds[idx]].min
      return if maxv < 0
      (0..maxv).each do |v|
        free_vals[idx] = v
        dfs.call(idx + 1, cur + v)
      end
    end

    if free_count.positive?
      dfs.call(0, 0)
    else
      evaluate.call(0)
    end

    total += best_total
  end
  total
end

t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p1 = part1(machines)
p2 = part2(machines)
elapsed = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0) * 1000.0
puts "min_lights_presses=#{p1} min_counter_presses=#{p2} elapsed_ms=#{format('%.3f', elapsed)}"
