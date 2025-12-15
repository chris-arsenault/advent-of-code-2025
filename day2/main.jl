#!/usr/bin/env julia

digits_len(n) = n == 0 ? 1 : floor(Int, log10(n)) + 1

function pow10(k)
    r = 1
    for _ in 1:k
        r *= 10
    end
    return r
end

function generate_even_half(maxn)
    vals = Int[]
    maxlen = digits_len(maxn)
    for half in 1:div(maxlen, 2)
        start = pow10(half - 1)
        limit = pow10(half)
        mul = pow10(half)
        for num in start:limit-1
            n = num * mul + num
            n > maxn && break
            push!(vals, n)
        end
    end
    sort!(vals)
    return vals
end

function generate_periodic(maxn)
    vals = Int[]
    seen = Set{Int}()
    maxlen = digits_len(maxn)
    for baselen in 1:cld(maxlen, 2)
        start = pow10(baselen - 1)
        limit = pow10(baselen)
        for base in start:limit-1
            base_s = string(base)
            for reps in 2:div(maxlen, baselen)
                s = repeat(base_s, reps)
                n = parse(Int, s)
                n > maxn && break
                if !(n in seen)
                    push!(vals, n)
                    push!(seen, n)
                end
            end
        end
    end
    sort!(vals)
    return vals
end

function prefix_sums(v)
    ps = zeros(Int, length(v) + 1)
    for i in eachindex(v)
        ps[i+1] = ps[i] + v[i]
    end
    return ps
end

function range_sum(v, ps, lo, hi)
    i = searchsortedfirst(v, lo)
    j = searchsortedlast(v, hi)
    if j < i || j == 0 || i > length(v)
        return 0
    end
    return ps[j+1] - ps[i]
end

function parse_ranges(text)
    rngs = Tuple{Int,Int}[]
    for part in split(replace(text, '\n' => ','), ",")
        s = strip(part)
        isempty(s) && continue
        a, b = strip.(split(s, "-"))
        push!(rngs, (parse(Int, a), parse(Int, b)))
    end
    return rngs
end

function solve(text)
    ranges = parse_ranges(strip(text))
    maxn = maximum(last.(ranges))
    evens = generate_even_half(maxn)
    evps = prefix_sums(evens)
    per = generate_periodic(maxn)
    pps = prefix_sums(per)
    p1 = 0
    p2 = 0
    for (a, b) in ranges
        p1 += range_sum(evens, evps, a, b)
        p2 += range_sum(per, pps, a, b)
    end
    return p1, p2
end

function main()
    t0 = time_ns()
    text = read("input.txt", String)
    p1, p2 = solve(text)
    elapsed_ms = (time_ns() - t0)/1e6
    println("repeated-halves-sum=$(p1) repeated-pattern-sum=$(p2) elapsed_ms=$(round(elapsed_ms; digits=3))")
end

main()
