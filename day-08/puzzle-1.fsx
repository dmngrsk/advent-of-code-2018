open System
open System.IO

let parse (input:string) = input.Split ' ' |> List.ofSeq |> List.map int

let rec getMetadataSum nodes =
    match nodes with
    | [] -> 0, []
    | cl :: ml :: nst ->
        let cms, mrns = List.mapFold (fun ns _ -> getMetadataSum ns) nst (List.init cl id)
        let ms, rns = List.splitAt ml mrns
        (List.sum ms) + (List.sum cms), rns
    | _ :: [] -> failwith "Failed to parse the tree."

let input = File.ReadAllText("./input.txt")
printfn "%A" (input |> parse |> getMetadataSum |> fst)