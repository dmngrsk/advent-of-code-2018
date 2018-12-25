open System.IO
open System.Text.RegularExpressions

let parseLine line = 
    let regexMatches = Regex.Matches(line, "-?[0-9]\\d*(\\.\\d+)?")
    let rmatch = (fun (i:int) -> int (regexMatches.[i].Value))
    (rmatch 0, rmatch 1, rmatch 2), rmatch 3

let manhattan3 (x, y, z) (x', y', z') =
    (abs (x - x')) + (abs (y - y')) + (abs (z - z'))

let countRobotsInRange rbts (p, r) =
    rbts
    |> List.filter (fun (p', r') -> manhattan3 p p' <= max r r')
    |> List.length

let input = File.ReadAllLines("./input.txt")
let rbts = input |> List.ofArray |> List.map parseLine
let rbt = List.maxBy snd rbts
printfn "%d" (countRobotsInRange rbts rbt)