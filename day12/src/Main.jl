using Graphs
path = "input"

function parseCharMap(path)
    data = readlines(path)
    charMap = Array{Char, 2}(undef, length(data), length(data[1])) 
    for (i, line) in enumerate(data)
        for (j, char) in enumerate(collect(line))
            charMap[i, j] = char
        end
    end
    charMap
end

charMap = path |> parseCharMap |> rotl90
adjacencyMap = zeros(Bool, length(charMap), length(charMap))


function validNeighbours(heightMap, i)
    mapSize = size(heightMap)

    possibleNeighbours = filter(x -> isassigned(charMap, x), [i+1, i-1, i-mapSize[1], i+mapSize[1]])
    replaceStartEnd(letter) = begin
        if letter == 'S'
            'a' 
        elseif letter == 'E' 
            'z'
        else
            letter
        end
    end
    origin = charMap[i] |> replaceStartEnd
    filter(x -> (origin-(heightMap[x] |> replaceStartEnd)) >= -1, possibleNeighbours)
end

for i in eachindex(charMap)
    neighbours = validNeighbours(charMap, i)
    for j in neighbours
        adjacencyMap[i, j] = true
    end
end

g = SimpleDiGraph(adjacencyMap)

start = findfirst('S', join(charMap))

destination = findfirst('E', join(charMap))

print("Part 1: ")
a_star(g, start, destination) |> length |> println

paths = []
allAs = findall('a', join(vec(charMap)))
for i in allAs
    pathDist = a_star(g, i, destination) |> length
    push!(paths, pathDist == 0 ? Inf : pathDist)
end
print("Part 2: ")
println(minimum(paths))
