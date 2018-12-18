open System.IO

type Cell = 
    | Open of int * int
    | Wall of int * int
    | Elf of int * int * int * int
    | Goblin of int * int * int * int

let parse eap input = 
    input
    |> Array.ofSeq
    |> Array.mapi (fun y cs -> 
        cs
        |> Array.ofSeq
        |> Array.mapi (fun x c ->
            match c with
            | '.' -> Open(x, y)
            | '#' -> Wall(x, y)
            | 'E' -> Elf(x, y, 200, eap)
            | 'G' -> Goblin(x, y, 200, 3)
            | _ -> failwith "Invalid cell type."))

let getCellType c =
    match c with
    | Open(_, _) -> '.'
    | Wall(_, _) -> '#'
    | Elf(_, _, _, _) -> 'E'
    | Goblin(_, _, _, _) -> 'G'

let getCellEnemyType c = 
    match c with
    | Elf(_, _, _, _) -> 'G'
    | Goblin(_, _, _, _) -> 'E'
    | _ -> failwith "Invalid cell type."

let getCellCoordinates c =
    match c with
    | Open(x, y) -> x, y
    | Wall(x, y) -> x, y
    | Elf(x, y, _, _) -> x, y
    | Goblin(x, y, _, _) -> x, y

let getCellHealth c =
    match c with
    | Elf(_, _, hp, _) -> hp
    | Goblin(_, _, hp, _) -> hp
    | _ -> failwith "Invalid cell type."

let getCellAttackPower c =
    match c with
    | Elf(_, _, _, ap) -> ap
    | Goblin(_, _, _, ap) -> ap
    | _ -> failwith "Invalid cell type."

let isElfCell c = (getCellType c) = 'E'
let isGoblinCell c = (getCellType c) = 'G'
let isTurnableCell c = (isElfCell c) || (isGoblinCell c)
let areEnemyCells c c' = (getCellType c) = (getCellEnemyType c')

let draw grid = grid |> Array.iter (fun cs -> cs |> Array.iter (fun c -> printf "%c" (getCellType c)); printfn "")

let countCellsOfType c grid =
    grid
    |> Array.collect id
    |> Array.filter (fun c' -> getCellType c' = c)
    |> Array.length

let getTurnableCells grid =
    grid
    |> Array.collect id
    |> Array.filter isTurnableCell
    |> List.ofArray

let getAdjacentCells (grid:array<array<Cell>>) (x, y) = [ 
    grid.[y - 1].[x];
    grid.[y].[x - 1];
    grid.[y].[x + 1];
    grid.[y + 1].[x]
]

let hasEnemies grid c =
    let t' = getCellEnemyType c
    let expand cs =
        cs
        |> List.map getCellCoordinates
        |> List.collect (getAdjacentCells grid)
        |> List.filter (fun c' -> getCellType c' = '.' || getCellType c' = t')
        |> List.append cs
        |> List.sortBy getCellCoordinates
        |> List.distinct
    let rec hasEnemies cs =
        match expand cs with
        | cs' when List.length cs <> List.length cs' -> hasEnemies cs'
        | cs' when List.exists (fun c' -> (getCellType c') = t') cs' -> true
        | cs' when List.fold (&&) true (List.map (fun (x, y) -> x = y) (List.zip cs cs')) -> false
        | cs' -> hasEnemies cs'
    hasEnemies [c]

let rec getPathToClosestEnemy grid c visited ps =
    let bfs grid c visited p =
        p
        |> List.head
        |> getCellCoordinates
        |> getAdjacentCells grid
        |> List.filter (fun c' -> (getCellType c' = '.' || getCellType c' = c) && not (Set.contains c' visited))
        |> List.map (fun c' -> c' :: p)
    let aggregatePaths = (fun vs p ->
        let ps' = (bfs grid c vs p)
        ps', (Set.union vs (Set.ofList (List.map List.head ps'))))
    let startsWithType c p = getCellType (List.head p) = c
    let pss', visited' = List.mapFold aggregatePaths visited ps
    match List.collect id pss' with
    | ps' when List.exists (startsWithType c) ps' -> 
        ps'
        |> List.filter (startsWithType c) 
        |> List.head // order is preserved during path build
        |> List.rev
    | [] -> []
    | ps' -> getPathToClosestEnemy grid c visited' ps'

let getAdjacentEnemies grid c =
    c
    |> getCellCoordinates
    |> getAdjacentCells grid
    |> List.filter isTurnableCell
    |> List.filter (areEnemyCells c)
    |> List.sortBy getCellHealth

let move x y c =
    match c with
    | Open(_, _) -> Open(x, y)
    | Wall(_, _) -> failwith "Walls are not movable."
    | Elf(_, _, hp, ap) -> Elf(x, y, hp, ap)
    | Goblin(_, _, hp, ap) -> Goblin(x, y, hp, ap)

let attack c c' =
    let ap = getCellAttackPower c
    let x', y' = getCellCoordinates c'
    match c' with
    | c' when (getCellHealth c') <= ap -> Open(x', y')
    | Elf(_, _, hp', ap') -> Elf(x', y', (hp' - ap), ap')
    | Goblin(_, _, hp', ap') -> Goblin(x', y', (hp' - ap), ap')
    | _ -> failwith "This cell is not attackable."

let makeTurnMove (grid:array<array<Cell>>) c =
    let x, y = getCellCoordinates c
    match hasEnemies grid c with
    | true ->
        match getPathToClosestEnemy grid (getCellEnemyType c) Set.empty [[c]] with
        | _ :: Open(xm, ym) :: _ ->
            let cm = move xm ym c
            grid.[y].[x] <- Open(x, y)
            grid.[ym].[xm] <- cm
            cm, grid
        | _ -> c, grid
    | false -> c, grid

let makeTurnAttack (grid:array<array<Cell>>) c =
    match getAdjacentEnemies grid c with
    | c' :: _ -> 
        let x', y' = getCellCoordinates c'
        grid.[y'].[x'] <- attack c c'
        grid
    | _ -> grid

let makeTurn (grid:array<array<Cell>>) c round =
    let x, y = getCellCoordinates c
    let c' = grid.[y].[x]
    match (getCellType c) = (getCellType c') && hasEnemies grid c' with
    | true -> 
        let tm, gridm = makeTurnMove grid c'
        makeTurnAttack gridm tm
    | false -> grid

let calculateOutcome round grid =
    let cs = getTurnableCells grid
    let ehp = cs |> List.filter isElfCell |> List.sumBy getCellHealth
    let ghp = cs |> List.filter isGoblinCell |> List.sumBy getCellHealth
    match ehp > 0 && ghp > 0 with
    | true -> 0
    | false -> (max ehp ghp) * round

let rec findOutcome round cells grid =
    match cells with
    | [] ->
        findOutcome (round + 1) (getTurnableCells grid) grid
    | c :: cs -> 
        match calculateOutcome round grid with
        | out when out > 0 -> grid, out
        | _ -> findOutcome round cs (makeTurn grid c round)

let rec findMinimalElfPower min max input =
    let mid = max - (max - min) / 2
    let grid = parse mid input
    let ec = countCellsOfType 'E' grid
    match (findOutcome -1 [] grid) with
    | grid', out when min = max -> out
    | grid', out when min = (max - 1) && (countCellsOfType 'E' grid') <> ec -> out
    | _, _ when min = (max - 1) -> findMinimalElfPower min min input
    | grid', _ when (countCellsOfType 'E' grid') = ec -> findMinimalElfPower min mid input
    | _, _ -> findMinimalElfPower (mid + 1) max input

let input = File.ReadAllLines("./input.txt")
printfn "%d" (findMinimalElfPower 4 200 input)