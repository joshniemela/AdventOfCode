using Pipe

const C₃ = CartesianIndex{3}
const O₃ = C₃(1, 1, 1)

const neighbours = (
    C₃(-1, 0, 0),
    C₃(1, 0, 0),
    C₃(0, -1, 0),
    C₃(0, 1, 0),
    C₃(0, 0, -1),
    C₃(0, 0, 1),
)


parse_line(line) = @pipe split(strip(line), ',') .|> parse(Int64, _) |> C₃(_...) + O₃

cubes = @pipe readlines("input") .|> parse_line |> collect

function cubeMatrix(cubes)
    maxIndices = maximum(cubes)
    cubeMat = zeros(Bool, Tuple(maxIndices)...)
    faces = length(cubes) * 6
    for cube in cubes
        for Δn in neighbours
            nb = cube + Δn
            checkbounds(Bool, cubeMat, nb) && cubeMat[nb] && (faces -= 2)
        end
        cubeMat[cube] = true
    end
    cubeMat, faces
end

println("Part 1: ", cubeMatrix(cubes)[2])

using DataStructures

function padMatrix(cubeMat)
    newCubeMat = zeros(Bool, size(cubeMat) .+ 2)
    newCubeMat[2:end-1, 2:end-1, 2:end-1] .= cubeMat
    newCubeMat
end

# Find all exposed sides of the 3d matrix 
function exposedSides(cubeMat)
    cubeMat = padMatrix(cubeMat)
    POIs = Stack{C₃}()
    visited = Set{C₃}()
    exposed_faces = 0
    # Initialize the POIs with the origin
    push!(POIs, O₃)

    while !isempty(POIs)
        cube = pop!(POIs)
        if cube ∈ visited
            continue
        end
        push!(visited, cube)
        for Δn in neighbours
            nb = cube + Δn
            checkbounds(Bool, cubeMat, nb) || continue
            cubeMat[nb] ? exposed_faces += 1 : push!(POIs, nb)
        end
    end
    exposed_faces
end


println("Part 2: ", exposedSides(cubeMatrix(cubes)[1]))