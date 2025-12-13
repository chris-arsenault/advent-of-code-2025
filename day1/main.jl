#!/usr/bin/env julia

function simulate(lines)
    pos = 50
    zero = 0
    crossings = 0
    for line in lines
        s = strip(line)
        isempty(s) && continue
        sign = s[1] == 'R' ? 1 : -1
        mag = parse(Int, s[2:end])
        first = sign == 1 ? 100 - pos : pos
        first == 0 && (first = 100)
        if mag >= first
            crossings += 1 + (mag - first) ÷ 100
        end
        pos = mod(pos + sign * mag, 100)
        pos == 0 && (zero += 1)
    end
    return zero, crossings, pos
end

function main()
    lines = readlines("input.txt")
    t0 = time_ns()
    z, c, pos = simulate(lines)
    elapsed_ms = (time_ns() - t0) / 1e6
    println("zero_landings=$(z) crossings=$(c) final_pos=$(pos) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
