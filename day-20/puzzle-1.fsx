open System.IO

let swp (a, b) = (b, a)
let ignoreMe = (-42, 42)

module Graph =
    type G<'a when 'a : comparison> = { vertices:Set<'a>; edges:Set<'a * 'a> }

    let addEdge v1 v2 { vertices = vs; edges = es } =
        match Set.contains v1 vs && Set.contains v2 vs with
        | true -> { vertices = vs; edges = Set.add (min v1 v2, max v1 v2) es }
        | false -> failwith (sprintf "Graph does not contain one of the vertices (%A or %A)." v1 v2)

    let addVertex v { vertices = vs; edges = es } = { vertices = Set.add v vs; edges = es }

    let adjacentTo v { vertices = vs; edges = es } = 
        es 
        |> Set.toList 
        |> List.filter (fun (v1, v2) -> v1 = v || v2 = v)
        |> List.map (fun (v1, v2) -> if v1 = v then v2 else v1)
        |> Set.ofList

    let empty = { vertices = Set.empty<'a>; edges = Set.empty<'a * 'a> }

    let vertices { vertices = vs; edges = es } = vs

let rec extractOptions graph depth regex = 
    let o, os = List.head graph, List.tail graph
    match regex with
    | '(' :: rt -> extractOptions (('(' :: o) :: os) (depth + 1) rt
    | '|' :: rt when depth = 0 -> extractOptions ([] :: (List.rev o) :: os) depth rt
    | ')' :: rt when depth = 0 -> List.rev ((List.rev o) :: os), rt
    | ')' :: rt -> extractOptions ((')' :: o) :: os) (depth - 1) rt
    | r :: rt -> extractOptions ((r :: o) :: os) depth rt
    | _ -> failwith "Failed to parse the regular expresssion."

let rec buildGraph' graph (x, y) regex =
    match regex with
    | '^' :: rt -> buildGraph' (graph |> Graph.addVertex (x, y)) (x, y) rt
    | 'N' :: rt -> buildGraph' (graph |> Graph.addVertex (x, y + 1) |> Graph.addEdge (x, y) (x, y + 1)) (x, y + 1) rt
    | 'S' :: rt -> buildGraph' (graph |> Graph.addVertex (x, y - 1) |> Graph.addEdge (x, y) (x, y - 1)) (x, y - 1) rt
    | 'E' :: rt -> buildGraph' (graph |> Graph.addVertex (x + 1, y) |> Graph.addEdge (x, y) (x + 1, y)) (x + 1, y) rt
    | 'W' :: rt -> buildGraph' (graph |> Graph.addVertex (x - 1, y) |> Graph.addEdge (x, y) (x - 1, y)) (x - 1, y) rt
    | '(' :: rt ->
        let options, rt' = extractOptions [[]] 0 rt
        let points, graph' = List.mapFold (fun g o -> swp (buildGraph' g (x, y) o)) graph options
        let completeGraph = List.fold (fun g p -> fst (buildGraph' g p rt')) graph' (List.distinct points)
        completeGraph, ignoreMe // second value can be omitted, because graph generation branches off
    | [] | '$' :: [] -> graph, (x, y)
    | x -> failwith "Failed to parse the regular expresssion."

let rec shortestPaths' acc l visited vs graph =
    match Set.count visited = Set.count (Graph.vertices graph) with
    | true -> Set.toList acc
    | false ->
        let vs' = 
            vs 
            |> List.map (fun v -> Graph.adjacentTo v graph)
            |> List.map Set.toList
            |> List.collect id
            |> List.filter (fun v -> not (Set.contains v visited))
            |> List.distinct            
        let acc' = List.fold (fun s v -> Set.add (v, l) s) acc vs'
        let visited' = List.fold (fun s v -> Set.add v s) visited vs'
        shortestPaths' acc' (l + 1) visited' vs' graph

let buildGraph p r = fst (buildGraph' Graph.empty p r)
let shortestPaths v graph = shortestPaths' Set.empty 1 (Set.add v Set.empty) [v] graph 

let furthestRoom paths = paths |> List.sortBy (fun (_, l) -> -l) |> List.head

let input = File.ReadAllText("./input.txt")
printfn "%d" (input |> Seq.map id |> List.ofSeq |> buildGraph (0, 0) |> shortestPaths (0, 0) |> furthestRoom |> snd)