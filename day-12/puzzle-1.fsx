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
    |> Seq.mapi (fun i x -> if x = '.' then 0 else i + offset)
    |> Seq.sum

let rec getNumbersSum n offset (state, rules) =
    match n with
    | 0 -> sumState state offset
    | _ -> getNumbersSum (n - 1) (offset - 2) ((iterate (pad state) rules), rules)

let input = File.ReadAllLines("./input.txt")
printfn "%d" (input |> parse |> getNumbersSum 20 0)