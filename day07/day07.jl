using Match
mutable struct Directory
    name::String
    size::Int
    parent::Union{Directory, Nothing}
    subdirs::Dict{String, Directory}
end

Directory(name, size, parent) = Directory(name, size, parent, Dict())
Directory() = Directory("", 0, nothing, Dict())

const rawLines = readlines("input")[2:end]


function Crawler(rawLines)
    parsedLines = split.(replace.(rawLines, r"\$ |ls" => ""), " ")
    root = Directory("/", 0, nothing)
    node = root
    for line in parsedLines
        @match line begin
            ["cd", ".."] => (node = node.parent)
            ["cd", dirName] => (node = node.subdirs[dirName])
            ["dir", dirName] => (node.subdirs[dirName] = Directory(dirName, 0, node))
            [fileSize, _] => begin
                fileSize = parse(Int, fileSize)
                function updateSize(node)
                    node.size += fileSize
                    if node.parent != nothing
                        updateSize(node.parent)
                    end
                end
                updateSize(node)
            end
        end
    end
    root
end


"""Part 1"""
function part1(root, acc=0, mx = 100_000)
    acc += root.size > mx ? 0 : root.size
    
    if isempty(root.subdirs) return acc end

    for (key, val) in root.subdirs
        acc += part1(val) 
    end
    acc
end

"""Part 2"""
function part2( system, mn = system.size ; used = system.size, disk_space = 70_000_000, minreq = 30_000_000)
    unused = disk_space - used

    if system.size + unused ≥ minreq 
        mn = min(system.size, mn) 
    end  

    if isempty(system.subdirs) return mn end

    for (key, val) in system.subdirs
        mn = part2(val, mn; used = used) 
    end
    mn
end
