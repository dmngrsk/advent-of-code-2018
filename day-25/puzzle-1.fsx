open System.IO

let parseLine (line:string) =
    let arr = line.Split(',')
    int arr.[0], int arr.[1], int arr.[2], int arr.[3]

let manhattan4 (a, b, c, d) (a', b', c', d') =
    (abs (a - a')) + (abs (b - b')) + (abs (c - c')) + (abs (d - d'))

let rec filterPointsInConstellation ps' p =
    let constellationPoints, remaining = List.partition (fun p' -> manhattan4 p p' <= 3) ps'
    List.fold filterPointsInConstellation remaining constellationPoints

let rec countConstellations acc points =
    match points with
    | [] -> acc
    | p :: ps ->
        let ps' = filterPointsInConstellation ps p
        countConstellations (acc + 1) ps'

let input = File.ReadAllLines("./input.txt")
printfn "%A" (input |> List.ofArray |> List.map parseLine |> countConstellations 0)