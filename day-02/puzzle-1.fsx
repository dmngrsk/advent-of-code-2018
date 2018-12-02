open System.IO

let countWithRepeatingLetters input count =
    input
    |> Seq.map (fun x -> x |> Seq.groupBy (fun c -> c))
    |> Seq.filter (fun gs -> gs |> Seq.exists (fun (k, vs) -> (vs |> Seq.length) = count))
    |> Seq.length

let input = File.ReadAllLines("./input.txt")
printfn "%d" (countWithRepeatingLetters input 2 * countWithRepeatingLetters input 3)