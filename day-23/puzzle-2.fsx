open System.IO
open System.Text.RegularExpressions

let fst3 (a, b, c) = a
let snd3 (a, b, c) = b
let trd3 (a, b, c) = c
let origin = (0L, 0L, 0L)

let parseLine line = 
    let regexMatches = Regex.Matches(line, "-?[0-9]\\d*(\\.\\d+)?")
    let rmatch = (fun (i:int) -> int64 (regexMatches.[i].Value))
    (rmatch 0, rmatch 1, rmatch 2), rmatch 3

let manhattan3 (x, y, z) (x', y', z') =
    (abs (x - x')) + (abs (y - y')) + (abs (z - z'))

let countRobotsInRangeToWindow rbts (wo, w) =
    rbts
    |> List.filter (fun (p, r) -> (manhattan3 wo p) - r < w)
    |> List.length

let getWindowPoints w (x, y, z) (x', y', z') =
    List.ofSeq (seq {
        for i in x .. w .. x' do 
        for j in y .. w .. y' do
        for k in z .. w .. z' do yield (i, j, k) })

let findWindowedOptimum w (min, max) robots =
    let ps = getWindowPoints w min max
    let candidates = List.map (fun p -> countRobotsInRangeToWindow robots (p, w), manhattan3 p origin, p) ps
    candidates |> List.sortBy (fun (rc, md, _) -> -rc, -md) |> List.head |> trd3
    
let getWindowSizes robots =
    let rec getWindowSizes' w d =
        match w / d with
        | 1L -> [ w; 1L ]
        | _ -> w :: getWindowSizes' (w / d) d
    let xs = List.map (fst >> fst3) robots
    let w = (List.max xs) - (List.min xs)
    getWindowSizes' w 2L

let getStartingPoints robots =
    let minf f' = robots |> List.minBy f' |> f'
    let maxf f' = robots |> List.maxBy f' |> f'
    let min = minf (fst >> fst3), minf (fst >> snd3), minf (fst >> trd3)
    let max = maxf (fst >> fst3), maxf (fst >> snd3), maxf (fst >> trd3)
    min, max

let getEdgePoints o (x, y, z) = 
    (x - o, y - o, z - o), (x + o, y + o, z + o)

let findDistanceToBestPoint robots =
    let ws = getWindowSizes robots
    let start = findWindowedOptimum (List.head ws) (getStartingPoints robots) robots
    let bestPoint = List.fold (fun p w -> findWindowedOptimum w (getEdgePoints (w * 2L) p) robots) start (List.tail ws)
    manhattan3 bestPoint origin

let input = File.ReadAllLines("./input.txt")
printfn "%d" (input |> List.ofArray |> List.map parseLine |> findDistanceToBestPoint)