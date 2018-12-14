open System.IO

let eq (a, b) = a = b

let parse input = input |> Seq.map (fun c -> (int c) - 48) |> Array.ofSeq

let toResizedArray (arr:array<int>) = new ResizeArray<int>(arr)
let appendResized (rarr:ResizeArray<int>) arr = rarr.AddRange(arr); rarr // new ResizeArray<int>(rarr)

let rec getRecipes sum = 
    match sum with
    | i when i < 10 -> Array.singleton i
    | i -> Array.append (getRecipes (sum / 10)) (Array.singleton (sum % 10))
    
let rec findIndexOf (recipes:ResizeArray<int>) elves acc idx searched =
    let rl = recipes.Count
    let sl = Array.length searched
    match Array.length acc with
    | al when al >= sl ->
        match Array.fold (&&) true (Array.zip (Array.take sl acc) searched |> Array.map eq) with
        | true -> idx
        | false -> findIndexOf recipes elves (Array.tail acc) (idx + 1) searched
    | _ ->
        let recipes' = getRecipes (Array.sumBy (fun i -> recipes.[i % rl]) elves)
        let elves' = Array.map (fun i -> (i % rl) + recipes.[i % rl] + 1) elves
        findIndexOf (appendResized recipes recipes') elves' (Array.append acc recipes') idx searched

let input = File.ReadAllText("./input.txt")
printfn "%d" (findIndexOf (toResizedArray [| 3; 7 |]) [| 0; 1 |] [| |] 2 (parse input))