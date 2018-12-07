open System.IO
open System.Text.RegularExpressions

let flip f a b = f b a
let neg f a = not (f a)

let parseLine str = 
    let regexMatch = Regex.Match(str, "Step ([A-Z]) must be finished before step ([A-Z]) can begin.")
    let group = (fun (i:int) -> char (regexMatch.Groups.[i].Value))
    (group 1, group 2)

let makeStepsList steps = steps |> List.map fst |> List.append (List.map snd steps) |> List.distinct
let appendStopConditionSteps steps = List.append steps (steps |> makeStepsList |> List.map (fun x -> x, '~'))

let findIndependentStep steps = 
    let requiredSteps = steps |> Seq.map fst |> Set.ofSeq
    let executedSteps = steps |> Seq.map snd |> Set.ofSeq
    executedSteps |> Set.difference requiredSteps |> Set.toSeq |> Seq.sortBy id |> Seq.head

let rec findStepOrder acc steps =
    match steps with
    | [] -> List.rev acc
    | _ -> 
        let currentStep = findIndependentStep steps
        findStepOrder (currentStep :: acc) (List.filter (fun (req, _) -> req <> currentStep) steps)

let input = File.ReadAllLines("./input.txt")
let steps = input |> List.ofSeq |> List.map parseLine |> appendStopConditionSteps
printfn "%s" (steps |> findStepOrder [] |> Array.ofList |> System.String)