open System.IO
open System.Text.RegularExpressions

let parseLine str = 
    let regexMatch = Regex.Match(str, "(\\d+), (\\d+)")
    let group = (fun (i:int) -> (int regexMatch.Groups.[i].Value))
    (group 1, group 2)

let manhattan (a, b) (c, d) = abs(a - c) + abs(b - d)

let sumOfDistances x y points = points |> List.sumBy (fun (xi, yi) -> manhattan (x, y) (xi, yi))

let makeGrid points minX minY maxX maxY =
    seq {
        for x in minX .. maxX do
        for y in minY .. maxY do
            yield (sumOfDistances x y points), x, y
    }

let mutualRegionAreaSize input =
    let points = input |> List.ofSeq |> List.map parseLine
    let minX = (points |> Seq.map fst |> Seq.min) - 1
    let minY = (points |> Seq.map snd |> Seq.min) - 1
    let maxX = (points |> Seq.map fst |> Seq.max) + 1
    let maxY = (points |> Seq.map snd |> Seq.max) + 1

    let grid = makeGrid points minX minY maxX maxY

    grid
    |> Seq.filter (fun (tm, _, _) -> tm < 10000)
    |> Seq.length

let input = File.ReadAllLines("./input.txt")
printfn "%d" (mutualRegionAreaSize input)