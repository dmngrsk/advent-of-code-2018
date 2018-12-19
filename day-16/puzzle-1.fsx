open System
open System.IO

let get c (r0, r1, r2, r3) =
    match c with
    | 0 -> r0
    | 1 -> r1
    | 2 -> r2
    | 3 -> r3
    | _ -> failwith "Invalid register index."

let set v c (r0, r1, r2, r3) =
    match c with
    | 0 -> (v, r1, r2, r3)
    | 1 -> (r0, v, r2, r3)
    | 2 -> (r0, r1, v, r3)
    | 3 -> (r0, r1, r2, v)
    | _ -> failwith "Invalid register name."

let gt a b = if a > b then 1 else 0
let eq a b = if a = b then 1 else 0

let addr a b c reg = set ((get a reg) + (get b reg)) c reg
let addi a b c reg = set ((get a reg) + b) c reg
let mulr a b c reg = set ((get a reg) * (get b reg)) c reg
let muli a b c reg = set ((get a reg) * b) c reg
let banr a b c reg = set ((get a reg) &&& (get b reg)) c reg
let bani a b c reg = set ((get a reg) &&& b) c reg
let borr a b c reg = set ((get a reg) ||| (get b reg)) c reg
let bori a b c reg = set ((get a reg) ||| b) c reg
let setr a b c reg = set (get a reg) c reg
let seti a b c reg = set a c reg
let gtir a b c reg = set (gt a (get b reg)) c reg
let gtri a b c reg = set (gt (get a reg) b) c reg
let gtrr a b c reg = set (gt (get a reg) (get b reg)) c reg
let eqir a b c reg = set (eq a (get b reg)) c reg
let eqri a b c reg = set (eq (get a reg) b) c reg
let eqrr a b c reg = set (eq (get a reg) (get b reg)) c reg

let funMap =
    Map.empty
    |> Map.add 0 addr
    |> Map.add 1 addi
    |> Map.add 2 mulr
    |> Map.add 3 muli
    |> Map.add 4 banr
    |> Map.add 5 bani
    |> Map.add 6 borr
    |> Map.add 7 bori
    |> Map.add 8 setr
    |> Map.add 9 seti
    |> Map.add 10 gtir
    |> Map.add 11 gtri
    |> Map.add 12 gtrr
    |> Map.add 13 eqir
    |> Map.add 14 eqri
    |> Map.add 15 eqrr

let toTuple4 (arr:array<'a>) = arr.[0], arr.[1], arr.[2], arr.[3]
let parseReg (line:string) = line.Substring(9, 10).Split([| ", " |], StringSplitOptions.None) |> Array.map int |> toTuple4
let parseInst (line:string) = line.Split([| " " |], StringSplitOptions.None) |> Array.map int |> toTuple4
let getInputTuple bl il al = (parseReg bl, parseInst il, parseReg al)

let rec parse input =
    match input with
    | bl :: il :: al :: _ :: it -> (getInputTuple bl il al) :: parse it
    | bl :: il :: al :: [] -> List.singleton (getInputTuple bl il al)
    | _ -> failwith "Failed to parse the input."

let getMatchingOpcodes (br, (opcode, a, b, c), ar) =
    opcode, List.filter (fun i -> (get c (funMap.[i] a b c br)) = (get c ar)) [ 0 .. 15 ]

let rec countBehavingLikeMultipleOpcodes n acc ts =
    match ts with
    | [] -> acc
    | t :: ts when List.length (snd (getMatchingOpcodes t)) >= 3 -> countBehavingLikeMultipleOpcodes n (acc + 1) ts
    | _ :: ts -> countBehavingLikeMultipleOpcodes n acc ts

let input = File.ReadAllLines("./input-1.txt")
printfn "%A" (input |> List.ofSeq |> parse |> countBehavingLikeMultipleOpcodes 3 0)