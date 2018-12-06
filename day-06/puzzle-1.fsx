open System.IO
open System.Text.RegularExpressions

let parseLine str = 
    let regexMatch = Regex.Match(str, "(\\d+), (\\d+)")
    let group = (fun (i:int) -> (int regexMatch.Groups.[i].Value))
    (group 1, group 2)

let manhattan (a, b) (c, d) = abs(a - c) + abs(b - d)

let closest x y (map:Map<int * int, char>) points =
    let closestPoints = 
        points
        |> List.map (fun (xi, yi) -> (x, y), (xi, yi), manhattan (x, y) (xi, yi))
        |> List.groupBy (fun (_, _, m) -> m)
        |> List.sortBy fst
        |> List.head 
        |> snd
    match closestPoints with
    | (_, y, _) :: [] -> map.[y]
    | _ -> '-'

let makeMap points = 
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*({[]})"
    |> Seq.zip points
    |> Map.ofSeq

let makeGrid map points minX minY maxX maxY =
    seq {
        for x in minX .. maxX do
        for y in minY .. maxY do
            yield (closest x y map points), x, y
    }

let largestFiniteAreaSize input =
    let points = input |> List.ofSeq |> List.map parseLine
    let minX = (points |> Seq.map fst |> Seq.min) - 1
    let minY = (points |> Seq.map snd |> Seq.min) - 1
    let maxX = (points |> Seq.map fst |> Seq.max) + 1
    let maxY = (points |> Seq.map snd |> Seq.max) + 1

    let map = makeMap points
    let grid = makeGrid map points minX minY maxX maxY

    let infiniteAreaNames = 
        grid
        |> Seq.filter (fun (_, x, y) -> x = minX || x = maxX || y = minY || y = maxY)
        |> Seq.map (fun (c, _, _) -> c)
        |> Seq.distinct
        |> Set.ofSeq

    grid
    |> Seq.filter (fun (c, x, y) -> infiniteAreaNames |> Set.contains c |> not)
    |> Seq.countBy (fun (c, x, y) -> c)
    |> Seq.maxBy snd
    |> snd

let input = File.ReadAllLines("./input.txt")
printfn "%d" (largestFiniteAreaSize input)