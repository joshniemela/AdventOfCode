open("input") do file
    data = read(file, String)


    elves = split.(data, "\n\n")
    elves = split.(elves, "\n")[1:end-1]
    
    maxElf = (0, 0)
    for (i, elf) in enumerate(elves)
        sum = 0
        for item in elf
            sum += parse(Int, item)
        end
        if sum > maxElf[2]
            maxElf = (i, sum)
        end
    end
    println(maxElf)
end
