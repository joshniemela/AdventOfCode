const data = readlines("inputtest")

regex = r"=(-?\d+)"

manhattan((x1, y1), (x2, y2)) = abs(x1 - x2) + abs(y1 - y2)


function part1(data)
    y = 9
    matches = eachmatch.(regex, data) .|> collect
    pointpairs = map(matches) do m
    ((parse(Int, m[1][1]), parse(Int, m[2][1])), 
        (parse(Int, m[3][1]), parse(Int, m[4][1])))
    end
    
    distances = map(pointpairs) do (p1, p2)
        manhattan(p1, p2)
    end
    distancesToY = map(pointpairs) do ((x1, y1), _)
        manhattan((x1, y1), (x1, y))
    end

    diffDistances = distancesToY .- distances
    ranges = []
    for i in eachindex(diffDistances)
        if distances[i] > 0
            mid = pointpairs[i][1][2]
            push!(ranges, (mid-diffDistances[i]:mid+diffDistances[i]))
        end
    end
    ranges
end
