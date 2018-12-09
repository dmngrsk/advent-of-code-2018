open System.IO
open System.Text.RegularExpressions

let parseLine str = 
    let regexMatch = Regex.Match(str, "(\\d+) players; last marble is worth (\\d+) points")
    let group = (fun (i:int) -> int (regexMatch.Groups.[i].Value))
    (group 1, group 2)
    
let rec getStepScores =
    let rec loop m ms =
        seq {
            match m with
            | 1 ->
                yield 0
                yield! loop 2 [ 1; 0 ]
            | 2 ->
                yield 0
                yield! loop 3 [ 2; 1; 0 ]
            | m when m % 23 = 0 -> 
                let ms1, ms2 = List.splitAt ((List.length ms) - 7) ms
                let ms' = List.append (List.tail ms2) ms1
                yield (m + (List.head ms2))
                yield! loop (m + 1) ms'
            | _ ->
                let ms' =
                    match ms with
                    | ch :: ch1 :: ch2 :: ct -> m :: ch2 :: (List.append ct [ ch; ch1 ])
                    | _ -> failwith "Given marbles circle is too short (should have at least 3 marbles)."
                yield 0
                yield! loop (m + 1) ms'
        }
    loop 1 [ 0 ]

let getMaxPlayerScore players steps =
    getStepScores 
    |> Seq.take (steps * 100) // same thing as #1, but open for optimization.
    |> Seq.mapi (fun i x -> i, x)
    |> Seq.groupBy (fun (i, _) -> i % players)
    |> Seq.map (fun (_, ss) -> Seq.sumBy snd ss)
    |> Seq.max

let input = File.ReadAllText("./input.txt")
let players, steps = parseLine input
printfn "%d" (getMaxPlayerScore players steps)