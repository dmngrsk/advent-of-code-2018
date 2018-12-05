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

let input = File.ReadAllText("./input.txt")
printfn "%d" (wrapWithStackSize (4 * 1024 * 1024) (fun () -> input |> List.ofSeq |> reduce |> List.length))