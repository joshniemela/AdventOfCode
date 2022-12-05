priority = [indexin(line, ['a':'z'; 'A':'Z']) for line in eachline("input")]
part1 = sum(x -> ∩(x...), map(elem -> Iterators.partition(elem, length(elem) ÷ 2), priority))
part2 = sum(x -> ∩(x...), Iterators.partition(priority, 3))

println("Part 1: $part1")
println("Part 2: $part2")
