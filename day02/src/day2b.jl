abstract type RockPaperScissors end
struct Rock <: RockPaperScissors end
struct Paper <: RockPaperScissors end
struct Scissors <: RockPaperScissors end


abstract type Action end
struct Lose <: Action end
struct Draw <: Action end
struct Win <: Action end

value(::Type{Rock}) = 1
value(::Type{Paper}) = 2
value(::Type{Scissors}) = 3

evalGame(::Type{Scissors}, ::Type{Rock}) = 7
evalGame(::Type{Rock}, ::Type{Paper}) = 8
evalGame(::Type{Paper}, ::Type{Scissors}) = 9
evalGame(a::Type{<:RockPaperScissors}, b::Type{<:RockPaperScissors}) = 
    a == b ? 3 + value(b) : value(b)

evalGame(x::Type{<:RockPaperScissors}, ::Type{Draw}) = evalGame(x, x)
evalGame(x::Type{<:RockPaperScissors}, ::Type{Lose}) = (value(x)-1)%3
evalGame(x::Type{<:RockPaperScissors}, ::Type{Win}) = (value(x)+1)%3 + 6


using Test
@testset "Rock paper scissors" begin
    @test evalGame(Rock, Paper) == 8
    @test evalGame(Paper, Rock) == 1
    @test evalGame(Scissors, Scissors) == 6

    @test evalGame(Rock, Draw) == 4
    @test evalGame(Paper, Lose) == 1
    @test evalGame(Scissors, Win) == 7 
end
    
