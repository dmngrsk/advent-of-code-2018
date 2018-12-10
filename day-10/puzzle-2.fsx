open System.IO
open System.Text.RegularExpressions

let height = 10 // found empirically, in test data it's 8 though ;)

let parseLine str = 
    let regexMatches = Regex.Matches(str, "-?[0-9]\\d*(\\.\\d+)?")
    let rmatch = (fun (i:int) -> int (regexMatches.[i].Value))
    (rmatch 0, rmatch 1, rmatch 2, rmatch 3)

let iterate ps = List.map (fun (x, y, vx, vy) -> (x + vx), (y + vy), vx, vy) ps
let rec findStepsCount i ps = 
    match iterate ps with
    | ps' when (ps' |> List.groupBy (fun (_, y, _, _) -> y) |> List.length) = height -> i
    | ps' -> findStepsCount (i + 1) ps'

let input = File.ReadAllLines("./input.txt")
printfn "%d" (input |> List.ofSeq |> List.map parseLine |> findStepsCount 1)
