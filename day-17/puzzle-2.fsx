open System.IO
open System.Text.RegularExpressions

module Map =
    let minBy f map = map |> Map.toSeq |> Seq.minBy f
    let maxBy f map = map |> Map.toSeq |> Seq.maxBy f

let getMinX grid = (Map.minBy (fst >> fst) >> fst >> fst) grid
let getMaxX grid = (Map.maxBy (fst >> fst) >> fst >> fst) grid
let getMinY grid = (Map.minBy (fst >> snd) >> fst >> snd) grid
let getMaxY grid = (Map.maxBy (fst >> snd) >> fst >> snd) grid

let draw grid =
    for y in (getMinY grid) .. (getMaxY grid) do
        for x in (getMinX grid) .. (getMaxX grid) do
            match Map.tryFind (x, y) grid with
            | None -> printf "%c" '.'
            | Some x -> printf "%c" x
        printfn ""

let parseLine line = 
    let regexMatches = Regex.Matches(line, "-?[0-9]\\d*(\\.\\d+)?")
    let rmatch = (fun (i:int) -> int (regexMatches.[i].Value))
    rmatch 0, (rmatch 1, rmatch 2)

let getWalls c f (input:array<string>) =
    input 
    |> Array.filter (fun x -> x.[0] = c)
    |> Array.map parseLine 
    |> Array.map f
    |> Array.collect id

let parse input = 
    let wallsX = getWalls 'x' (fun (x, (y1, y2)) -> Array.map (fun y -> (x, y), '#') [| y1 .. y2 |]) input
    let wallsY = getWalls 'y' (fun (y, (x1, x2)) -> Array.map (fun x -> (x, y), '#') [| x1 .. x2 |]) input
    let map = Map.ofArray (Array.append wallsX wallsY)
    Map.add (500, getMinY map) '|' map
    
let rec expandSide grid o (x, y) =
    let c = Map.tryFind (x, y) grid
    let c' = Map.tryFind (x, y + 1) grid
    match (c, c') with
    | Some '#', _ -> (x - o), false, []
    | _, None -> x, true, [(x, y)]
    | _ -> expandSide grid o (x + o, y)

let expand grid (x, y) =
    let xl, ofl, ssl = expandSide grid -1 (x, y)
    let xr, ofr, ssr = expandSide grid 1 (x, y)
    let water = List.map (fun x -> (x, y), if ofl || ofr then '|' else '~') [ xl .. xr ]
    let sources = List.append ssl ssr
    water, sources

let rec proceed grid maxY newSources (x, y) =
    match Map.tryFind (x, y) grid with
    | _ when y > maxY -> grid, newSources
    | Some '~' | Some '#' ->
        let water, newSources' = expand grid (x, y - 1)
        let grid' = List.fold (fun g (xy, w) -> Map.add xy w g) grid water
        proceed grid' maxY (List.append newSources newSources') (x, y - 1)
    | None -> 
        let grid' = Map.add (x, y) '|' grid
        proceed grid' maxY newSources (x, y + 1)
    | _ -> grid, newSources

let proceedSource grid maxY (x, y) =
    match Map.tryFind (x, y) grid with
    | Some '|' -> proceed grid maxY [] (x, y + 1) 
    | _ -> grid, []

let rec buildGridFromSources grid maxY sources =
    match sources with
    | [] -> grid
    | s :: ss ->
        let grid', sources' = proceedSource grid maxY s
        buildGridFromSources grid' maxY (List.append ss sources')

let countWaterTiles grid =
    grid 
    |> Map.toList 
    |> List.filter (fun (_, t) -> t = '~')
    |> List.length

let input = File.ReadAllLines("./input.txt")
let grid = parse input
let grid' = buildGridFromSources grid (getMaxY grid) [(500, getMinY grid)]
printfn "%d" (countWaterTiles grid')