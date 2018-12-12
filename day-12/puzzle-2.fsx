open System.IO

let substr n (s:string) = s.Substring(n)
let substrl n l (s:string) = s.Substring(n, l)

let parseRule line =
    let pattern = substrl 0 5 line
    let result = substr 9 line
    pattern, result.[0]

let parse input =
    let state = substr 15 (Seq.head input)
    let rules = input |> Seq.skip 2 |> Seq.map parseRule |> Map.ofSeq
    state, rules

let pad state = ("...." + state + "....")

let iterate state (rules:Map<string,char>) = 
    state
    |> List.ofSeq
    |> List.windowed 5
    |> List.map (fun cs -> cs |> Array.ofList |> System.String)
    |> List.map (fun cs -> rules.[cs])
    |> Array.ofList
    |> System.String

let sumState state offset =
    state
    |> Seq.mapi (fun i x -> if x = '.' then 0L else (int64 i) + offset)
    |> Seq.sum

let trim (state:string) = state.Trim('.')
let findPattern state state' = (trim state) = (trim state')

let rec getNumbersSum n offset (state, rules) =
    match n with
    | 0L -> sumState state offset
    | _ ->
        let state' = iterate (pad state) rules
        match findPattern state state' with
        | true ->
            let sum = sumState state offset
            let sum' = sumState state' (offset - 2L)
            sum + (n * (sum' - sum))
        | false -> 
            getNumbersSum (n - 1L) (offset - 2L) (state', rules)

let input = File.ReadAllLines("./input.txt")
printfn "%d" (input |> parse |> getNumbersSum 50000000000L 0L)