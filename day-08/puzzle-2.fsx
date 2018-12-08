open System
open System.IO

let parse (input:string) = input.Split ' ' |> List.ofSeq |> List.map int

let rec getRootNodeValue nodes =
    match nodes with
    | [] -> 0, []
    | cl :: ml :: nst ->
        let cms, mrns = List.mapFold (fun ns _ -> getRootNodeValue ns) nst (List.init cl id)
        let ms, rns = List.splitAt ml mrns
        (if cl = 0 then List.sum ms else List.sumBy (fun i -> if i <= cl then cms.[i - 1] else 0) ms), rns
    | _ :: [] -> failwith "Failed to parse the tree."

let input = File.ReadAllText("./input.txt")
printfn "%A" (input |> parse |> getRootNodeValue |> fst)