using Distributed
@everywhere using JuMP
@everywhere using StaticArrays
@everywhere using Chain
@everywhere import HiGHS
@everywhere struct Blueprint
  id          ::Int64
  oreCost     ::SVector{3, Int64}
  clayCost    ::SVector{3, Int64}
  obsidianCost::SVector{3, Int64}
  geodeCost   ::SVector{3, Int64}
end

function parseBlueprint(blueprint::String)
  regex = r"[\d]+"
  matches = eachmatch(regex, blueprint) |> collect
  Blueprint(
    parse(Int64, matches[1].match),
    SVector{3, Int64}(
      parse(Int64, matches[2].match), 0, 0
    ),
    SVector{3, Int64}(
      parse(Int64, matches[3].match), 0, 0
    ),
    SVector{3, Int64}(
      parse(Int64, matches[4].match),
      parse(Int64, matches[5].match), 
      0
    ),
    SVector{3, Int64}(
      parse(Int64, matches[6].match),
      0,
      parse(Int64, matches[7].match),
    ),
  )
end


@everywhere function maximiseGeodes(blueprint::Blueprint, minutes=24)
  mining = Model(HiGHS.Optimizer)
  T = 1:minutes+1
  T₋ = 1:minutes
  T₊ = 2:minutes+1

  R = (:ore, :clay, :obsidian, :geode)
  @variables(mining, begin
    0 <= robots[R, T], (integer = true)
    0 <= resources[R, T], (integer = true)
    0 <= constructing[R, T₋], (integer = true)
  end)
  for r ∈ R, (t₋, t₊) ∈ zip(T₋, T₊)
    # Robots in the next minute are equal to the robots in the previous minute plus the robots being built
    @constraint(mining, robots[r, t₊] == robots[r, t₋] + constructing[r, t₋])
  end
  for t ∈ T₋
    # Only one robot can be built at a time
    @constraint(mining, sum(constructing[:, t]) <= 1)
  end

  function totalCost(blueprint::Blueprint, t::Int64, index)
    return (
      blueprint.oreCost[index] * constructing[:ore, t] +
      blueprint.clayCost[index] * constructing[:clay, t] +
      blueprint.obsidianCost[index] * constructing[:obsidian, t] +
      blueprint.geodeCost[index] * constructing[:geode, t]
    )
  end
  
  for (t₋, t₊) ∈ zip(T₋, T₊)
    # enumerate
    for (i, r) ∈ enumerate(R[1:3])
      @constraint(mining, robots[r, t₋] + resources[r, t₋] == (
        resources[r, t₊] + 
        totalCost(blueprint, t₋, i)
      ))
    end
    @constraints(mining, begin
      # Geode production
      robots[:geode, t₋] + resources[:geode, t₋] == (resources[:geode, t₊])

      # Resources are always non-negative
      resources[:ore, t₋] >= totalCost(blueprint, t₋, 1)
      resources[:clay, t₋] >= totalCost(blueprint, t₋, 2)
      resources[:obsidian, t₋] >= totalCost(blueprint, t₋, 3)
    end)
  end

  # Initial conditions
  for r ∈ R
    if r == :ore
      @constraints(mining, begin
        robots[r, 1] == 1
        resources[r, 1] == 0
      end)
    else
      @constraints(mining, begin
        robots[r, 1] == 0
        resources[r, 1] == 0
      end)
    end
  end
  
  @objective(mining, Max, resources[:geode, minutes+1])
  optimize!(mining)
  
  objective_value(mining)
end

input = readlines("input")

blueprints = map(parseBlueprint, input)

@everywhere qualityScore(blueprint::Blueprint) = maximiseGeodes(blueprint) * blueprint.id

println("Part1: ", round(Int, sum(pmap(qualityScore, blueprints))))

productOfThreeBest = @chain blueprints begin
  _[1:3]
  maximiseGeodes.(_, 32)
  prod
  round(Int64, _)
end
println("Part2: ", productOfThreeBest)