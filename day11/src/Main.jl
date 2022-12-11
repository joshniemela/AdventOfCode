const data = readlines("input")


mutable struct Monkey
    items::Vector{Int}
    throwFunction::Function
    nInspected::Int
end

linesOfInfo = match.(r"\: (.*)", data)

function splitMonkeys(lines)
    monkeys = []
    i = 2
    while i <= length(lines)
        push!(monkeys, lines[i:i+4] .|> first)
        i += 7
    end
    monkeys
end

function parseMonkey(monkeyString)
    items = split.(monkeyString[1], ", ") |> x -> parse.(Int, x)
    newX = (replace(monkeyString[2], "new =" => "x ->", "old" => "x") |> Meta.parse |> eval)
    test = string(replace(monkeyString[3], "divisible by" => "x -> x %"), " == 0") |> Meta.parse |> eval

    decreaseWorry(x) = x/3 |> floor |> Int
    getValue(x) = parse(Int, match(r"(\d+)", x)[1])
    
    newWorry = x -> newX(x) |> decreaseWorry
    newThrow = (x -> newWorry(x) |> test 
    ? (newWorry(x), getValue(monkeyString[4])) 
    : (newWorry(x), getValue(monkeyString[5])))
    Monkey(items, newThrow, 0)
end

monkeys = splitMonkeys(linesOfInfo) .|> parseMonkey

function throwStuff(monkeys)
    for monkey in monkeys
        for i in 1:length(monkey.items)
            monkey.nInspected += 1
            throw = monkey.throwFunction(popfirst!(monkey.items))
            push!(monkeys[throw[2]+1].items, throw[1])
        end
    end
end

    
# Part 1
for i in 1:20
    throwStuff(monkeys)
end

inspections = partialsort!(map(x -> x.nInspected, monkeys), 1:2, rev=true)
println("Part1: $(prod(inspections))")

function parseMonkeyWorriful(monkeyString)
    items = split.(monkeyString[1], ", ") |> x -> parse.(Int, x)
    newX = (replace(monkeyString[2], "new =" => "x ->", "old" => "x") |> Meta.parse |> eval)
    test = string(replace(monkeyString[3], "divisible by" => "x -> x %"), " == 0") |> Meta.parse |> eval

    getValue(x) = parse(Int, match(r"(\d+)", x)[1])
    
    newThrow = (x -> newX(x) |> test 
    ? (newX(x), getValue(monkeyString[4])) 
    : (newX(x), getValue(monkeyString[5])))
    Monkey(items, newThrow, 0)
end

monkeys = splitMonkeys(linesOfInfo) .|> parseMonkeyWorriful
# Part 2
for i in 1:10000
    throwStuff(monkeys)
    for monkey in monkeys
        # Magical constant is the LCM of all of the divisors in the input
        monkey.items = map(x -> x % 9699690, monkey.items)
    end
end
inspections = partialsort!(map(x -> x.nInspected, monkeys), 1:2, rev=true)
println("Part2: $(prod(inspections))")