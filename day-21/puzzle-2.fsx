open System
open System.IO

let parseA (line:string) = line.Split([| " " |], StringSplitOptions.None) |> Seq.item 1 |> int
let parse input = (parseA (Array.item 8 input))

let findRegisterValue a pa =
    [ 0; 1; 2 ]
    |> List.map (fun s -> ((pa ||| 0x10000) >>> (s * 8)) &&& 0xFF)
    |> List.fold (fun a a' ->  (((a + a') &&& 0xFFFFFF) * 0x1016B) &&& 0xFFFFFF) a

let rec findLastRegisterValue pa visited a =
    match findRegisterValue a pa with
    | a' when Set.contains a' visited -> pa
    | a' -> findLastRegisterValue a' (Set.add a' visited) a

let input = File.ReadAllLines("./input.txt")
printfn "%d" (findLastRegisterValue 0 Set.empty (parse input))