open System.IO

let rec transpose mx =
    match mx with
    | [] :: _ -> []
    | _ -> (List.map List.head mx) :: (transpose (List.map List.tail mx))

let plvl x y serial = ((((x + 10) * y + serial) * (x + 10)) / 100) % 10 - 5
let buildGrid n serial = List.init 300 (fun y -> List.init 300 (fun x -> plvl (x + 1) (y + 1) serial))

let findMaximum sums s =
    sums
    |> List.mapi (fun y ps -> List.mapi (fun x p -> (x + 1), (y + 1), s, p) ps)
    |> List.collect id
    |> List.maxBy (fun (_, _, _, p) -> p)

let sumRows grid gridT s x y =
    let sumX = grid |> List.skip (s + y) |> List.head |> List.skip x |> List.take s |> List.sum
    let sumY = gridT |> List.skip (s + x) |> List.head |> List.skip y |> List.take (s + 1) |> List.sum
    sumX + sumY

let reduceSums sums grid gridT s =
    sums 
    |> List.take (300 - s)
    |> List.mapi (fun y ps -> 
        ps
        |> List.take (300 - s)
        |> List.mapi (fun x p -> p + (sumRows grid gridT s x y)))

let rec findNextPowerSquares (x, y, s, p) sums grid gridT =
    match 300 - (List.length sums) + 1 with
    | 300 -> (x, y, s, p)
    | ns -> 
        let sums' = reduceSums sums grid gridT ns
        let (x', y', s', p') = findMaximum sums' (ns + 1)
        let acc' = if p' > p then (x', y', s', p') else (x, y, s, p)
        findNextPowerSquares acc' sums' grid gridT

let getHighestPowerSquareWithSize grid = findNextPowerSquares (findMaximum grid 1) grid grid (transpose grid)

let input = int (File.ReadAllText("./input.txt"))
let x, y, size, p = input |> buildGrid 300 |> getHighestPowerSquareWithSize
printfn "%d,%d,%d" x y size