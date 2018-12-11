open System.IO

let plvl x y serial = ((((x + 10) * y + serial) * (x + 10)) / 100) % 10 - 5
let buildGrid n serial = List.init 300 (fun y -> List.init 300 (fun x -> plvl (x + 1) (y + 1) serial))

let getSquarePowerSum x y size grid =
    grid
    |> List.skip y
    |> List.take size
    |> List.sumBy (fun xs -> xs |> List.skip x |> List.take size |> List.sum)

let getHighestPowerSquare size grid =
    let L = List.length grid
    List.init (L - size + 1) (fun x -> List.init (L - size + 1) (fun y -> (x + 1), (y + 1), getSquarePowerSum x y size grid))
    |> List.collect id
    |> List.sortBy (fun (x, y, p) -> -p)
    |> List.head

let input = int (File.ReadAllText("./input.txt"))
let x, y, p = input |> buildGrid 300 |> getHighestPowerSquare 3
printfn "%d,%d" x y