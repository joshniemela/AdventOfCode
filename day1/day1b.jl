open("input") do file
    data = read(file, String)


    elves = split.(data, "\n\n")
    elves = split.(elves, "\n")[1:end-1]
    
    elfSums = []
    for (i, elf) in enumerate(elves)
        sum = 0
        for item in elf
            sum += parse(Int, item)
        end
        push!(elfSums, sum)
    end
    # find top 3
    top3 = partialsort(elfSums, 1:3, rev=true) |> sum 
    println(top3)
end
