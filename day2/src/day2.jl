using Match
using Test
function evalRound(message)
    @match message begin
        ("A", "X") => 4
        ("A", "Y") => 8
        ("A", "Z") => 3
        ("B", "X") => 1
        ("B", "Y") => 5
        ("B", "Z") => 9
        ("C", "X") => 7
        ("C", "Y") => 2
        ("C", "Z") => 6
        _ => exception("Invalid input")
    end
end

function getMove(message)
    @match message begin
        ("A", "X") => ("A", "Z")
        ("A", "Y") => ("A", "X")
        ("A", "Z") => ("A", "Y")
        ("B", "X") => ("B", "X")
        ("B", "Y") => ("B", "Y")
        ("B", "Z") => ("B", "Z")
        ("C", "X") => ("C", "Y")
        ("C", "Y") => ("C", "Z")
        ("C", "Z") => ("C", "X")
        _ => exception("Invalid input")
    end
end
    
    open("input") do file
    lines = readlines(file)
    messages = map(x -> split(x, " "), lines)
    @assert length(messages) == 2500
    messagePairs = tuple.([x[1] for x in messages], [x[2] for x in messages])
    sum([evalRound(x) for x in messagePairs]) |> println # part one

    # part two
    newMessages = map(x -> getMove(x), messagePairs)
    sum([evalRound(x) for x in newMessages]) |> println
end

@testset "Rock paper scissors" begin
    @test evalRound(("A", "Y")) == 8
    @test evalRound(("B", "X")) == 1
    @test evalRound(("C", "Z")) == 6

    @test ("A", "Y") |> getMove |> evalRound == 4
    @test ("B", "X") |> getMove |> evalRound == 1
    @test ("C", "Z") |> getMove |> evalRound == 7 
end
    
