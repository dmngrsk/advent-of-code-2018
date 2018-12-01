open System.IO
open System.Collections.Generic

let findFirstRepeating = 
    let mutable firstRepeating = -1
    let mutable states = new HashSet<int>()
    fun acc s ->
        match firstRepeating with
        | -1 -> 
            let newValue = acc + (s |> int)
            match states.Contains(newValue) with
            | true -> firstRepeating <- newValue
            | false -> states.Add(newValue) |> ignore
            newValue
        | _ -> firstRepeating            

let findOutput input =
    let mutable acc = -1
    let mutable newAcc = 0
    while acc <> newAcc do
        acc <- newAcc
        newAcc <- (Array.fold findFirstRepeating acc input)
    acc

let input = File.ReadAllLines("./input.txt")
printfn "%d" (findOutput input)