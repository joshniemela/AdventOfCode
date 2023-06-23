abstract type Instruction end
struct Noop <: Instruction end
struct AddX <: Instruction
    x::Int
end

    

const rawLines = split.(readlines("input"), " ")

function parseInstruction(line)
    if line[1] == "addx"
        return AddX(parse(Int, line[2]))
    else
        return Noop()
    end
end


const instructions = map(parseInstruction, rawLines)
function runProgram(instructions)
    accumulatedRegister = [1, 1]
    for instruction in instructions
        if instruction isa AddX
            push!(accumulatedRegister, accumulatedRegister[end] + instruction.x)
        end
        push!(accumulatedRegister, accumulatedRegister[end])
    end
    return accumulatedRegister
end

# Part 1
indices = [20, 60, 100, 140, 180, 220]
println(runProgram(instructions)[indices] .* indices |> sum)

# Part 2
function drawLine(values)
    spriteDefault = collect(0:2)
    for i in 1:length(values)
      sprite = spriteDefault .+ values[i]
      if i ∈ sprite
        print("█")
      else
        print(" ")
      end
    end
    
end

function drawImage(values)
    for i in 1:6
        drawLine(values[1+40*(i-1):1+40*i])
        println()
    end
end

println("Part 2")
drawImage(runProgram(instructions))

# Result: REHPRLUB

