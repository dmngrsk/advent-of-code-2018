open System.IO

let m = 20183
let moda a b = (a + b) % m
let modm a b = (a * b) % m

let parse (input:array<string>) =
    let d = int (input.[0].Substring(7))
    let x = int (input.[1].Substring(8).Split(',').[0])
    let y = int (input.[1].Substring(8).Split(',').[1])
    d, x, y

let expandGrid d x y tx ty map =
    match x, y with
    | x', y' when x' = tx && y' = ty -> Map.add (tx, ty) d map
    | x, 0 -> Map.add (x, 0) (moda (modm x 16807) d) map
    | 0, y -> Map.add (0, y) (moda (modm y 48271) d) map
    | x, y -> Map.add (x, y) (moda (modm map.[x, y - 1] map.[x - 1, y]) d) map

let buildGrid d x y =
    seq { 0 .. y }
    |> Seq.fold (fun mj j ->
        seq { 0 .. x }
        |> Seq.fold (fun mi i -> expandGrid d i j x y mi) mj) Map.empty

let getRiskLevel x y (map:Map<int*int,int>) =
    seq { 0 .. y }
    |> Seq.sumBy (fun j ->
        seq { 0 .. x }
        |> Seq.sumBy (fun i -> map.[i, j] % 3))

let input = File.ReadAllLines("./input.txt")
let d, tx, ty = parse input
printfn "%d" (buildGrid d x y |> getRiskLevel x y)