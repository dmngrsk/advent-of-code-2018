open System.IO

let neg f x = not (f x)

let cartCharacters = [ '^'; '>'; 'v'; '<' ]

let replaceCarts (l:string) = l.Replace("^", "|").Replace(">", "-").Replace("v", "|").Replace("<", "-")
let rec findCarts acc x y line =
    match line with
    | [] -> acc
    | c :: cs when List.contains c cartCharacters -> findCarts ((x, y, c, 0) :: acc) (x + 1) y cs
    | _ :: cs -> findCarts acc (x + 1) y cs

let parse input = 
    let grid = input |> List.ofSeq |> List.map replaceCarts
    let carts = input |> List.ofSeq |> List.mapi (fun y xs -> findCarts [] 0 y (List.ofSeq xs)) |> List.collect id
    grid, carts

let rotateCart c (i:int) = cartCharacters.[((List.findIndex (fun c' -> c' = c) cartCharacters) + i + 4) % 4]
let moveCart (grid:list<string>) (x, y, d, r) =
    let (x', y') =
        match d with
        | '^' -> x, (y - 1)
        | '>' -> (x + 1), y
        | 'v' -> x, (y + 1)
        | '<' -> (x - 1), y
        | _ -> failwith "Attention, cart off the rails!"
    let (d', r') =
        match (d, grid.[y'].[x']) with
        | ('^', '/') | ('>', '\\') | ('v', '/') | ('<', '\\') -> (rotateCart d 1), r
        | ('^', '\\') | ('>', '/') | ('v', '\\') | ('<', '/') -> (rotateCart d -1), r
        | (_, '|') | (_, '-') -> d, r
        | (_, '+') -> (rotateCart d ((r % 3) - 1)), (r + 1)
        | _ -> failwith "Attention, cart off the rails!"
    (x', y', d', r')

let cartsOnSameCoordinate (x, y, d, r) (x', y', d', r') = x = x' && y = y'

let findCollision c carts1 carts2 =
    match c with
    | x, y, d, r when List.exists (cartsOnSameCoordinate (x, y, d, r)) carts1 -> Some (x, y)
    | x, y, d, r when List.exists (cartsOnSameCoordinate (x, y, d, r)) carts2 -> Some (x, y)
    | _ -> None

let moveCartsInOrder grid carts =
    let rec moveCarts carts carts' colls =
        match carts with
        | [] -> carts', colls
        | c :: cs ->
            let c' = moveCart grid c
            match findCollision c' carts carts' with
            | None -> moveCarts cs (c' :: carts') colls
            | Some (x, y) ->
                let filteredCarts = (List.filter (neg (cartsOnSameCoordinate c')) cs)
                let filteredCarts' = (List.filter (neg (cartsOnSameCoordinate c')) carts')
                moveCarts filteredCarts filteredCarts' ((x, y) :: colls)

    moveCarts (carts |> List.sortBy (fun (x, y, _, _) -> y, x)) [] []

let rec findFirstCollisionCoordinates (grid:list<string>, carts) =
    let movedCarts, collisions = moveCartsInOrder grid carts
    match collisions with
    | (x, y) :: _ -> x, y
    | [] -> findFirstCollisionCoordinates (grid, movedCarts)

let input = File.ReadAllLines("./input.txt")
let x, y = findFirstCollisionCoordinates (input |> parse)
printfn "%d,%d" x y