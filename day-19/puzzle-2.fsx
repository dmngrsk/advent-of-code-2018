open System
open System.IO

let parseB (line:string) = line.Split([| " " |], StringSplitOptions.None) |> Seq.item 2 |> int
let parse input = (parseB (Array.item 22 input)), (parseB (Array.item 24 input))

let findFactors n = seq { for i in 1 .. n do if n % i = 0 then yield i }

let rec run (a, b) =
    let p1 = (2 * 2) * 19 * 11
    let p2 = (27 * 28 + 29) * 30 * 14 * 32
    let n = 22 * a + b + p1 + p2
    Seq.sum (findFactors n) // obtained via reverse-engineering

let input = File.ReadAllLines("./input.txt")
printfn "%d" (run (parse input))