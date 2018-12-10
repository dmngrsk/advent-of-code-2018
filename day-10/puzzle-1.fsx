open System.IO
open System.Text.RegularExpressions

let height = 10 // found empirically, in test data it's 8 though ;)

let parseLine str = 
    let regexMatches = Regex.Matches(str, "-?[0-9]\\d*(\\.\\d+)?")
    let rmatch = (fun (i:int) -> int (regexMatches.[i].Value))
    (rmatch 0, rmatch 1, rmatch 2, rmatch 3)

let iterate ps = List.map (fun (x, y, vx, vy) -> (x + vx), (y + vy), vx, vy) ps
let rec findGrouped ps = 
    match iterate ps with
    | ps' when (ps' |> List.groupBy (fun (_, y, _, _) -> y) |> List.length) = height -> ps'
    | ps' -> findGrouped ps'

let buildString xs =
    [| 0 .. (Set.maxElement xs) |]
    |> Array.map (fun i -> if Set.contains i xs then '#' else '.')
    |> System.String

let printMessage ps =
    let minX = ps |> List.minBy fst |> fst
    let minY = ps |> List.minBy fst |> fst
    ps
    |> List.map (fun (x, y) -> (x - minX), (y - minY))
    |> List.groupBy snd
    |> List.sortBy fst
    |> List.map (fun (_, ps) -> ps |> List.map fst |> Set.ofList |> buildString)
    |> List.iter (fun s -> printfn "%s" s)

let input = File.ReadAllLines("./input.txt")
input |> List.ofSeq |> List.map parseLine |> findGrouped |> List.map (fun (x, y, _, _) -> x, y) |> printMessage
