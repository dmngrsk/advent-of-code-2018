open System
open System.IO

let get c (r0, r1, r2, r3, r4, r5) =
    match c with
    | 0 -> r0
    | 1 -> r1
    | 2 -> r2
    | 3 -> r3
    | 4 -> r4
    | 5 -> r5
    | _ -> failwith "Invalid register index."

let set v c (r0, r1, r2, r3, r4, r5) =
    match c with
    | 0 -> (v, r1, r2, r3, r4, r5)
    | 1 -> (r0, v, r2, r3, r4, r5)
    | 2 -> (r0, r1, v, r3, r4, r5)
    | 3 -> (r0, r1, r2, v, r4, r5)
    | 4 -> (r0, r1, r2, r3, v, r5)
    | 5 -> (r0, r1, r2, r3, r4, v)
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
    |> Map.add "addr" addr
    |> Map.add "addi" addi
    |> Map.add "mulr" mulr
    |> Map.add "muli" muli
    |> Map.add "banr" banr
    |> Map.add "bani" bani
    |> Map.add "borr" borr
    |> Map.add "bori" bori
    |> Map.add "setr" setr
    |> Map.add "seti" seti
    |> Map.add "gtir" gtir
    |> Map.add "gtri" gtri
    |> Map.add "gtrr" gtrr
    |> Map.add "eqir" eqir
    |> Map.add "eqri" eqri
    |> Map.add "eqrr" eqrr

let toInstTuple (arr:array<string>) = arr.[0], int arr.[1], int arr.[2], int arr.[3]
let parsePtr (line:string) = line.Substring(4) |> int
let parseInst (line:string) = line.Split([| " " |], StringSplitOptions.None) |> toInstTuple
let fst6 (x, y, z, t, a, b) = x

let parse input =
    let ptr = parsePtr (Seq.head input)
    let is = input |> Seq.tail |> Array.ofSeq |> Array.map parseInst
    ptr, is

let rec run reg (ptr, is) =
    match get ptr reg with
    | i when i >= Array.length is -> fst6 reg
    | i -> 
        let (op, a, b, c) = is.[i]
        let reg' = funMap.[op] a b c reg
        run (addi ptr 1 ptr reg') (ptr, is)

let input = File.ReadAllLines("./input.txt")
printfn "%d" (run (0, 0, 0, 0, 0, 0) (parse input))