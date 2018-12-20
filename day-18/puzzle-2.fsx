open System.IO

let parse input = Array.map Array.ofSeq input
let draw (area:array<array<char>>) = Array.iter (fun xs -> Array.iter (fun a -> printf "%c" a) xs; printfn "") area

let getAdjacent (area:array<array<'a>>) x y =
    let maxX = Array.length (Array.head area) - 1
    let maxY = Array.length area - 1
    seq {
        for j in (max 0 (y - 1)) .. (min maxY (y + 1)) do
        for i in (max 0 (x - 1)) .. (min maxX (x + 1)) do
        if (i, j) <> (x, y) then yield (i, j) }
    |> Array.ofSeq
    |> Array.map (fun (i, j) -> area.[j].[i])

let countOfType a col = col |> Array.filter (fun a' -> a' = a) |> Array.length

let changeAcre area x y a =
    let adjacent = getAdjacent area x y
    let ts = countOfType '|' adjacent
    let ls = countOfType '#' adjacent
    match a with
    | '.' -> if ts < 3 then '.' else '|'
    | '|' -> if ls < 3 then '|' else '#'
    | '#' -> if ts < 1 || ls < 1 then '.' else '#'
    | _ -> failwith "Invalid acre type."

let changeArea area = Array.mapi (fun y xs -> Array.mapi (fun x a -> changeAcre area x y a) xs) area

let rec iterate n visited area = 
    match n with
    | 0 -> area
    | _ -> 
        match Map.tryFind area visited with
        | Some n' ->
            let cl = n' - n
            let o = n - (n / cl) * cl
            iterate o Map.empty area
        | None -> 
            iterate (n - 1) (Map.add area n visited) (changeArea area)

let getResult area =
    let farea = Array.collect id area
    let ts = countOfType '|' farea
    let ls = countOfType '#' farea
    ts * ls

let input = File.ReadAllLines("./input.txt")
let area = iterate 1000000000 Map.empty (parse input)
printfn "%d" (getResult area)