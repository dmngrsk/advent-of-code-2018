open System.IO

let findCommonLettersOfCorrectBoxes input =
    input 
    |> Seq.collect (fun xs -> input |> Seq.map (fun ys -> xs, ys))
    |> Seq.filter (fun (xs, ys) -> xs <> ys)
    |> Seq.map (fun (xs, ys) -> Seq.zip xs ys |> Seq.filter (fun (x, y) -> x = y) |> Seq.map (fun (z, _) -> z))
    |> Seq.sortBy (fun zs -> -1 * (Seq.length zs))
    |> Seq.head
    |> Array.ofSeq
    |> System.String

let input = File.ReadAllLines("./input.txt")
printfn "%s" (findCommonLettersOfCorrectBoxes input)