open System.IO

// Purely Functional Data Structures (Chris Okasaki), Chapter 3.1
module Heap = 
    type Heap<'a> =
        | Empty
        | Tree of int * 'a * Heap<'a> * Heap<'a>

    let rank h = match h with | Empty -> 0 | Tree(r, _, _, _) -> r

    let make x a b =
        match rank a, rank b with
        | ra, rb when ra > rb -> Tree(ra + 1, x, a, b)
        | ra, rb -> Tree(rb + 1, x, b, a)

    let singleton x = make x Empty Empty

    let rec merge h1 h2 =
        match h1, h2 with
        | Empty, h -> h
        | h, Empty -> h
        | Tree(_, x, a1, b1), Tree(_, y, a2, b2) when x < y -> make x a1 (merge b1 h2)
        | Tree(_, x, a1, b1), Tree(_, y, a2, b2) -> make y a2 (merge h1 b2)

    let insert x h = merge (singleton x) h

    let findMin h =
        match h with
        | Empty -> failwith "The heap is empty, minimum element cannot be obtained."
        | Tree(_, x, _, _) -> x

    let removeMin h =
        match h with
        | Empty -> failwith "The heap is empty, cannot remove minimum element."
        | Tree(_, _, a, b) -> merge a b

let m = 20183
let moda a b = (a + b) % m
let modm a b = (a * b) % m
let order x y = if x < y then (x, y) else (y, x)

let parse (input:array<string>) =
    let d = int (input.[0].Substring(7))
    let x = int (input.[1].Substring(8).Split(',').[0])
    let y = int (input.[1].Substring(8).Split(',').[1])
    d, x, y

let expandGrid d x y tx ty map =
    match x, y with
    | x', y' when x' = tx && y' = ty -> Map.add (tx, ty) d map
    | x, 0 -> Map.add (x, 0) (moda (modm x 16807) d) map
    | 0, y -> Map.add (0, y) (moda (modm y 48271) d) map
    | x, y -> Map.add (x, y) (moda (modm map.[x, y - 1] map.[x - 1, y]) d) map

let buildGrid d x y tx ty =
    seq { 0 .. y }
    |> Seq.fold (fun mj j ->
        seq { 0 .. x }
        |> Seq.fold (fun mi i -> expandGrid d i j tx ty mi) mj) Map.empty

type Tool = 
    | Gear 
    | Torch 
    | Neither

let getValidTools e =
    match e % 3 with
    | 0 -> [ Tool.Gear; Tool.Torch ]
    | 1 -> [ Tool.Gear; Tool.Neither ]
    | 2 -> [ Tool.Torch; Tool.Neither ]
    | _ -> failwith "This should not happen."

let getAdjacentVertices (grid:Map<int*int,int>) bx by (x, y, t) d =
    let oppositeTool = grid.[x, y] |> getValidTools |> List.filter (fun t' -> t' <> t) |> List.head
    [ (x, y - 1); (x + 1, y); (x, y + 1); (x - 1, y) ]
    |> List.filter (fun (x', y') -> x' >= 0 && x' <= bx && y' >= 0 && y' <= by)
    |> List.collect (fun (x', y') -> List.map (fun t' -> x', y', t') (getValidTools grid.[x', y']))
    |> List.map (fun (x', y', t') -> if t = t' then ((x', y', t'), d + 1) else ((x', y', t'), d + 8))
    |> List.append [ ((x, y, oppositeTool), d + 7) ]

let getDistance x y m = 
    match Map.tryFind (order x y) m with
    | Some x -> x
    | None -> 0x4B1D4B1D

let setLowerDistance x y w m =
    match getDistance x y m with
    | w' when w >= w' -> m
    | w' -> m |> Map.remove (order x y) |> Map.add (order x y) w

let dijkstra grid bx by src dest =
    let rec dijkstra' m h =
        match Heap.rank h with
        | 0 -> getDistance src dest m
        | _ ->
            let w, v = Heap.findMin h
            match w > (getDistance src dest m) with
            | true -> dijkstra' m (Heap.removeMin h)
            | false ->
                let queueable, m' =
                    getAdjacentVertices grid bx by v w
                    |> List.filter (fun (v', w') -> getDistance src v' m > w')
                    |> List.mapFold (fun m' (v', w') -> (w', v'), setLowerDistance src v' w' m') m
                let h' = List.fold (fun h v -> Heap.insert v h) (Heap.removeMin h) queueable
                dijkstra' m' h'    
    dijkstra' (Map.add (src, src) 0 Map.empty) (Heap.singleton (0, src))

let getShortestPathLength input =
    let d, tx, ty = parse input
    let x, y = tx * 2, ty * 2
    dijkstra (buildGrid d x y tx ty) x y (0, 0, Tool.Torch) (tx, ty, Tool.Torch)

let input = File.ReadAllLines("./input.txt")
printfn "%d" (getShortestPathLength input)