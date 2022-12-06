data = eachline("input") |> collect

blank = findfirst(isempty, data)

# The regex string matche to the following:
# Everything followed by move, from, to (with spaces in between)
moves = data[blank+1:end] .|> x -> parse.(Int, match(r"move (\d+) from (\d+) to (\d+)", x).captures)

#= 
The regex string matche to the following:
Finds the capitalised letters between square brackets OR 4 consecutive whitespaces
since the whitespace is not captured, Julia will return 'nothing' =#
crateArray = map(eachmatch.(r"\[([A-Z])\]|\ {4}", data[begin:blank-2])) do row
    row .|> x -> x.captures[1] === nothing ? nothing : x.captures[1][1]
end |> x -> hcat(x...)

# Part one
crateStacks = eachrow(crateArray) .|> (x -> filter(!isnothing, x)) .|> reverse
function move!(stacks, n, from, to)
    [push!(stacks[to], pop!(stacks[from])) for _ in 1:n]
end


[move!(crateStacks, instructions...) for instructions in moves]
println("Part one: ", [stack[end] for stack in crateStacks] |> join)

# Part two
crateStacks = eachrow(crateArray) .|> (x -> filter(!isnothing, x)) .|> reverse
function orderedMove!(stacks, n, from, to)
    removed = [pop!(stacks[from]) for _ in 1:n] |> reverse
    push!(stacks[to], removed...)
end

[orderedMove!(crateStacks, instructions...) for instructions in moves]
println("Part one: ", [stack[end] for stack in crateStacks] |> join)
