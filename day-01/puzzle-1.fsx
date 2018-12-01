open System.IO

let input = File.ReadAllLines("./input.txt")
printfn "%d" (Array.fold (fun acc s -> acc + (s |> int)) 0 input)