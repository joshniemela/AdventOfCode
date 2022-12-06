data = [split(replace(x, "-" => ":"), ",") .|> Meta.parse .|> eval for x in eachline("../input")]

⫓(a, b) = a ⊆ b || b ⊆ a
sum([⫓(pair...) for pair in data]) |> println
sum([(∩(pair...) ≠ []) for pair in data]) |> println

