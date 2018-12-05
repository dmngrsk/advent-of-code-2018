open System
open System.IO
open System.Threading

let wrapWithStackSize size f =
    let mutable result = -1
    let thread = new Thread((fun () -> result <- f()), size)
    thread.Start()
    thread.Join()
    result

let reacts x y = abs((int x) - (int y)) = 0x20

let rec reduce input =
    match input with
    | [] -> []
    | c :: [] -> [c]
    | c :: cs ->
        match reduce cs with
        | [] -> c :: []
        | rc :: rcs -> 
            match reacts c rc with
            | true -> rcs
            | false -> c :: rc :: rcs

let findShortestPolymer input =
    input
    |> List.map Char.ToUpper
    |> List.distinct
    |> List.map (fun x -> input |> List.filter (fun c -> (Char.ToUpper c) <> x))
    |> List.map reduce
    |> List.minBy List.length
    
let input = File.ReadAllText("./input.txt")
printfn "%d" (wrapWithStackSize (4 * 1024 * 1024) (fun () -> input |> List.ofSeq |> findShortestPolymer |> List.length))