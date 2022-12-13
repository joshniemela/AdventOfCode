data = split.(readlines("input"), "")
data = reduce(hcat, map(x -> parse.(Int, x), data))

function scenic_check(m)
    h,w = size(m)
    max = 0
    for (ith, col) in enumerate(eachcol(m))
        if ith in (1,w) continue
        else
            for jth in 2:h-1
                row = m[jth, :]

                cnt = scenic_dist(col, jth) * scenic_dist(row, ith)
                if cnt > max
                    max = cnt
                end

            end
        end
    end
    max
   end

function scenic_dist(v, i)
    right = 0
    left = 0
    for j in (i+1):length(v)

        cond = v[i] > v[j]
        right += cond
        if !cond
            right +=1
            break
        end
    end

    for j in (i-1):-1:1
        cond = v[i] > v[j]
        left += cond
        if !cond
            left += 1
            break
        end
    end

    @show left, right
    right*left

end


rightLeft = [scenic_dist(data[:, i], 0) for i in 1:size(data, 1)]
upDown = [scenic_dist(data[i, :], 0) for i in 1:size(data, 2)]

println(maximum(rightLeft .* upDown))