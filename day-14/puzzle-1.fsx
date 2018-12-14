open System.IO

let rec getRecipes sum = 
    match sum with
    | i when i < 10 -> Array.singleton i
    | i -> Array.append (getRecipes (sum / 10)) (Array.singleton (sum % 10))

let rec getRecipeScore recipes elves t =
    match Array.length recipes with
    | rl when rl >= (t + 10) ->
        recipes
        |> Array.skip t
        |> Array.take 10
        |> Array.map string
        |> String.concat ""
    | rl ->
        let recipes' = getRecipes (Array.sumBy (fun i -> recipes.[i % rl]) elves)
        let elves' = Array.map (fun i -> (i % rl) + recipes.[i % rl] + 1) elves
        getRecipeScore (Array.append recipes recipes') elves' t

let input = File.ReadAllText("./input.txt")
printfn "%s" (getRecipeScore [| 3; 7 |] [| 0; 1 |] (int input))