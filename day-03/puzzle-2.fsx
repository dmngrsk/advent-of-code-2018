open System.IO
open System.Text.RegularExpressions

let parseLine str = 
    let regexMatch = Regex.Match(str, "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)")
    let group = (fun (i:int) -> (int regexMatch.Groups.[i].Value))
    (group 1, group 2, group 3, group 4, group 5)

let enumeratePoints (id, x, y, a, b) =
    seq { 
        for xi in x .. x + a - 1 do
        for yi in y .. y + b - 1 do
            yield (id, xi, yi)
    }

let toClaimless (_, x, y) = (x, y)

let findClaimWithoutMulticlaimedPoints lines =
    let allPoints = 
        lines
        |> Seq.map parseLine
        |> Seq.map enumeratePoints
        |> Seq.collect id

    let multiclaimedPoints = 
        allPoints
        |> Seq.countBy toClaimless
        |> Seq.filter (fun (_, cnt) -> cnt > 1)
        |> Seq.map fst
        |> Set.ofSeq

    allPoints
    |> Seq.groupBy (fun (id, x, y) -> id)
    |> Seq.filter (fun (id, ps) -> ps |> Seq.map toClaimless |> Seq.filter (fun p -> multiclaimedPoints |> Set.contains p) |> Seq.isEmpty)
    |> Seq.map fst
    |> Seq.head

let input = File.ReadAllLines("./input.txt")
printfn "%d" (findClaimWithoutMulticlaimedPoints input)